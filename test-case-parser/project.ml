(* This file is part of the Catala project. Copyright (C) 2024 Inria.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils

let to_relative (p : File.t) = File.make_relative_to ~dir:(Sys.getcwd ()) p

let has_clerk_toml dir = File.(exists (dir / "clerk.toml"))

(* The nearest project root above [dir]: where clerk.toml lives. *)
let find_project_root (dir : string) : string option =
  Option.map fst
    (File.find_in_parents ~cwd:(File.make_absolute dir) has_clerk_toml)

let lookup_clerk_toml from_dir =
  let open Catala_utils in
  try
    begin
      let from_dir = File.make_absolute from_dir in
      match File.find_in_parents ~cwd:from_dir has_clerk_toml with
      | None -> None
      | Some (abs_dir, rel) ->
        let clerk_toml_path = File.(abs_dir / "clerk.toml") in
        Message.debug "Found config file: %s" clerk_toml_path;
        let config = Clerk_config.read clerk_toml_path in
        Some (config, rel)
    end
  with _ -> None

let lookup_include_dirs ?(prefix_build = false) ?buffer_path options =
  (* Otherwise, lookup for the toml *)
  let dir =
    match options.Global.input_src with
    | FileName file | Contents (_, file) -> Filename.dirname file
    | Stdin _ -> (
      match buffer_path with
      | None -> Sys.getcwd ()
      | Some buffer_path -> Filename.dirname buffer_path)
  in
  match lookup_clerk_toml dir with
  | None -> ".", []
  | Some (config, rel) ->
    let path_to_build = to_relative File.(dir / rel) in
    let all_include_dirs =
      match options.Global.input_src with
      | Stdin _ ->
        (* We add the test file directory as catala is unable to retrieve its
           dir *)
        List.sort_uniq String.compare
          (to_relative dir :: config.global.include_dirs)
      | _ -> config.global.include_dirs
    in
    let include_dirs =
      if prefix_build then
        List.map (fun p -> File.(path_to_build / "_build" / p)) all_include_dirs
      else List.map (File.( / ) path_to_build) all_include_dirs
    in
    let all_include_dirs =
      match options.Global.input_src with
      | Stdin _ ->
        (* We add the test file directory as catala is unable to retrieve its
           dir *)
        List.sort_uniq String.compare (to_relative dir :: include_dirs)
      | _ -> include_dirs
    in
    Message.debug "@[<h>Found %s dirs:@ %a@]"
      (if prefix_build then "build" else "include")
      Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
      all_include_dirs;
    path_to_build, List.map Global.raw_file all_include_dirs

let build_dir_rel ?buffer_path options =
  (* Otherwise, lookup for the toml *)
  let f dir =
    let dir =
      if Filename.is_relative dir then
        if dir = "." then Sys.getcwd () else File.(Sys.getcwd () / dir)
      else to_relative dir
    in
    lookup_clerk_toml dir
    |> function
    | None -> None
    | Some (_config, rel) -> Some (to_relative File.(dir / rel))
  in
  match options.Global.input_src with
  | FileName file | Contents (_, file) -> f (Filename.dirname file)
  | Stdin _ -> (
    match buffer_path with
    | None -> f (Sys.getcwd ())
    | Some buffer_path -> f (Filename.dirname buffer_path))

exception Unsupported of string

let unsupported fmt = Format.ksprintf (fun msg -> raise (Unsupported msg)) fmt

(* At a command's entry: a refusal, not an internal error. *)
let guarded f = try f () with Unsupported msg -> Message.error "%s" msg

(* The compiler's own diagnostic, as text: the one thing that gets the tester
   out. *)
let error_text (e : exn) : string =
  let of_content c =
    Message.pp_to_string ~ansi:false (fun ppf ->
        Message.Content.emit ~ppf c Message.Error)
  in
  match e with
  | Message.CompilerError c -> of_content c
  | Message.CompilerErrors ((c, _) :: _) -> of_content c
  | Unsupported msg -> "unsupported: " ^ msg
  | e -> Printexc.to_string e


let ( (implicit_stdlib_aliases : Global.backend_lang -> string list),
      (lookup_aliased_name : Global.backend_lang -> string -> string option) )
    =
  let en_names =
    [
      "Date";
      "Duration";
      "MonthYear";
      "Period";
      "Money";
      "Integer";
      "Decimal";
      "List";
    ]
  in
  let en_aliases = List.map (fun s -> s ^ "_en") en_names in
  let fr_aliases = List.map (fun s -> s ^ "_fr") en_names in
  let fr_names =
    [
      "Date";
      "Durée";
      "MoisAnnée";
      "Période";
      "Argent";
      "Entier";
      "Décimal";
      "Liste";
    ]
  in
  let en_implicit_aliases = en_names @ en_aliases in
  let fr_implicit_aliases = fr_names @ fr_aliases in
  let implicit_stdlib_aliases = function
    | `En -> en_implicit_aliases
    | `Fr -> fr_implicit_aliases
    | `Pl -> en_implicit_aliases
  in
  let en_alias_map = String.Map.of_list (List.combine en_aliases en_names) in
  let fr_alias_map = String.Map.of_list (List.combine fr_aliases fr_names) in
  let lookup_aliased_name lang s =
    match lang with
    | `En -> String.Map.find_opt s en_alias_map
    | `Fr -> String.Map.find_opt s fr_alias_map
    | `Pl -> String.Map.find_opt s en_alias_map
  in
  implicit_stdlib_aliases, lookup_aliased_name

let is_implicit_stdlib_alias lang alias =
  List.exists (fun a -> String.equal a alias) (implicit_stdlib_aliases lang)

let project_root (from_dir : string) : string =
  Option.value (find_project_root from_dir) ~default:from_dir

module Scan = Clerk_utils.Scan

(* Per-file failures dropped: broken files are recovery's normal case. *)
let scan_catala_files (dir : string) : Scan.item list =
  File.scan_tree
    (fun f ->
      match Scan.get_lang f with
      | None -> None
      | Some lang -> ( try Some (Scan.catala_file f lang) with _ -> None))
    dir
  |> Seq.concat_map (fun (_, _, items) -> List.to_seq items)
  |> List.of_seq

(* Lexical scan: resolves in a project that does not compile. *)
let find_module_file (name : string) (from_dir : string) : string option =
  let declares (it : Scan.item) =
    match it.Scan.module_def with
    | Some m -> String.equal (Mark.remove m) name
    | None -> false
  in
  let find dir =
    Option.map
      (fun (it : Scan.item) -> it.Scan.file_name)
      (List.find_opt declares (scan_catala_files dir))
  in
  match find from_dir with
  | Some f -> Some f
  | None -> find (project_root from_dir)

