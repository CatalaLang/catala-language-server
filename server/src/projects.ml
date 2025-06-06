(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
   Vincent Botbol <vincent.botbol@inria.fr>

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
open Server_types

module Scan_item = struct
  type t = Clerk_scan.item

  let compare
      { Clerk_scan.file_name; _ }
      { Clerk_scan.file_name = file_name'; _ } =
    Doc_id.(compare (of_file file_name) (of_file file_name'))

  let format fmt { Clerk_scan.file_name; _ } =
    Format.pp_print_string fmt file_name
end

module ScanItemFiles = Set.Make (Scan_item)

type project_file = {
  file : Clerk_scan.item;
  including_files : ScanItemFiles.t;
  used_by : ScanItemFiles.t;
}

type project_kind =
  | Clerk of { clerk_root_dir : string; clerk_config : Clerk_config.t }
  | No_clerk

type project = {
  project_dir : string;
  project_kind : project_kind;
  project_files : project_file Doc_id.Map.t;
}

module Projects = Set.Make (struct
  type t = project

  let compare p1 p2 = String.compare p1.project_dir p2.project_dir
end)

type projects = Projects.t
type t = projects

let empty = Projects.empty

let format_file ppf { file; including_files; used_by } =
  let open Format in
  fprintf ppf "file: %s, %aused by: %a" file.file_name
    (fun fmt -> function
      | [] -> ()
      | l -> fprintf fmt "%a, " (pp_print_list pp_print_string) l)
    (ScanItemFiles.elements including_files
    |> List.map (fun { Clerk_scan.file_name; _ } -> file_name))
    (pp_print_list pp_print_string)
    (ScanItemFiles.elements used_by
    |> List.map (fun { Clerk_scan.file_name; _ } -> file_name))

let format_kind ppf =
  let open Format in
  function
  | Clerk { clerk_root_dir; _ } ->
    fprintf ppf "kind: clerk found in %s" clerk_root_dir
  | No_clerk -> fprintf ppf "kind: no clerk found"

let format_project ppf (p : project) =
  let open Format in
  fprintf ppf "@[<v 2>project dir: %s, %a, files:@ %a@]" p.project_dir
    format_kind p.project_kind
    (pp_print_list format_file)
    (List.map snd (Doc_id.Map.bindings p.project_files))

let format_projects ppf (projects : Projects.t) =
  let open Format in
  fprintf ppf "@[<v>%a@]"
    (pp_print_list format_project)
    (Projects.elements projects)

let lookup_project (doc_id : Doc_id.t) projects =
  let file = (doc_id :> File.t) in
  (* Lookup file in longest prefix project dir *)
  assert (not (Filename.is_relative file));
  let file = Filename.dirname file in
  let prefix_found = ref false in
  let is_prefix x = String.starts_with ~prefix:x.project_dir file in
  let lookup x =
    let is_prefix = is_prefix x in
    if is_prefix && not !prefix_found then (
      prefix_found := true;
      true)
    else if (not is_prefix) && !prefix_found then false
    else String.compare file x.project_dir >= 0
  in
  match Projects.find_last_opt lookup projects with
  | None -> None
  | Some x when is_prefix x -> Some x
  | Some _ -> None

let clean_item ({ Clerk_scan.file_name; included_files; _ } as item) :
    Clerk_scan.item =
  {
    item with
    file_name = File.clean_path file_name;
    included_files = List.map (Mark.map File.clean_path) included_files;
  }

let find_module_candidate
    ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
    includes
    (file : Scan_item.t)
    (possible_modules : Scan_item.t list)
    (used_module : string Mark.pos) : Scan_item.t option =
  let file_dir = File.dirname file.file_name in
  let includes = file_dir :: includes in
  let possible_modules =
    List.filter
      (fun m -> List.mem (File.dirname m.Clerk_scan.file_name) includes)
      possible_modules
  in
  match possible_modules with
  | [] -> None
  | [modul] -> Some modul
  | l -> (
    match
      List.find_opt (fun m -> file_dir = File.dirname m.Clerk_scan.file_name) l
    with
    | Some x -> Some x
    | None ->
      (* Multiple candidates possible: warn about it *)
      let mod_def, mod_use_pos = used_module in
      let msg = Format.sprintf "Module %s defined multiple times" mod_def in
      let related =
        List.filter_map
          (fun modul ->
            Option.map
              (fun mod_def -> Mark.get mod_def, "Conflicting definition")
              modul.Clerk_scan.module_def)
          l
      in
      (Lwt.async
      @@ fun () ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep 5. in
      let diag = Diagnostic.error_p ~related mod_use_pos (`String msg) in
      notify_back#set_uri
        (Linol_lwt.DocumentUri.of_path file.Clerk_scan.file_name);
      notify_back#send_diagnostic [diag]);
      None)

let retrieve_project_files
    ~notify_back
    (clerk_config : Clerk_config.t)
    clerk_root_dir =
  let open Clerk_scan in
  Log.info (fun m -> m "building inclusion graph");
  let tree = tree clerk_root_dir in
  let known_items : (string, item) Hashtbl.t = Hashtbl.create 10 in
  let known_modules = Hashtbl.create 10 in
  Seq.iter
    (fun (_, _, items) ->
      List.iter
        (fun item ->
          let item = clean_item item in
          Hashtbl.add known_items item.file_name item;
          Option.iter
            (fun module_def ->
              let module_def = Mark.remove module_def in
              (* Module with the same name may be declared multiple times *)
              match Hashtbl.find_opt known_modules module_def with
              | None -> Hashtbl.add known_modules module_def [item]
              | Some l -> Hashtbl.replace known_modules module_def (item :: l))
            item.module_def)
        items)
    tree;
  let g =
    Hashtbl.fold
      (fun _n item g ->
        Doc_id.(Map.add (of_file item.file_name))
          {
            file = item;
            including_files = ScanItemFiles.empty;
            used_by = ScanItemFiles.empty;
          }
          g)
      known_items Doc_id.Map.empty
  in
  Hashtbl.fold
    (fun n item g ->
      let included_items =
        List.filter_map
          (fun includ ->
            Hashtbl.find_opt known_items (Mark.remove includ)
            |> function
            | Some x -> Some x
            | None ->
              Log.warn (fun m ->
                  m "Did not find included file '%s' declared in '%s'"
                    (Mark.remove includ) n);
              None)
          item.included_files
      in
      (* Update including files *)
      let g =
        List.fold_left
          (fun g included_item ->
            Doc_id.(Map.update (of_file included_item.file_name))
              (function
                | None ->
                  Some
                    {
                      file = included_item;
                      including_files = ScanItemFiles.singleton item;
                      used_by = ScanItemFiles.empty;
                    }
                | Some pf ->
                  Some
                    {
                      pf with
                      including_files =
                        ScanItemFiles.add item pf.including_files;
                    })
              g)
          g included_items
      in
      (* Update used-by files *)
      List.fold_left
        (fun g (used_module : string Mark.pos) ->
          let used_module_name = Mark.remove used_module in
          let possible_modules =
            Option.value ~default:[]
              (Hashtbl.find_opt known_modules used_module_name)
          in
          find_module_candidate ~notify_back clerk_config.global.include_dirs
            item possible_modules used_module
          |> function
          | None -> (* No file using this module *) g
          | Some modul ->
            (* Found a good module candidate *)
            Doc_id.(Map.update (of_file item.file_name))
              (function
                | None ->
                  Some
                    {
                      file = item;
                      including_files = ScanItemFiles.empty;
                      used_by = ScanItemFiles.singleton modul;
                    }
                | Some pf ->
                  Some { pf with used_by = ScanItemFiles.add modul pf.used_by })
              g)
        g item.used_modules)
    known_items g

let project_of_folder ~notify_back project_dir =
  match Utils.lookup_clerk_toml project_dir with
  | None ->
    Log.warn (fun m ->
        m "no clerk config file found, assuming default configuration");
    let project_files =
      retrieve_project_files ~notify_back Clerk_config.default_config
        project_dir
    in
    let project_kind = No_clerk in
    { project_dir; project_kind; project_files }
  | Some (clerk_config, clerk_root_dir) ->
    Log.debug (fun m -> m "clerk file found in '%s' directory" clerk_root_dir);
    let project_kind = Clerk { clerk_root_dir; clerk_config } in
    (* We also consider Catala files that may be upper in the hierarchy but
       under the discovered "clerk.toml" scope *)
    let project_files =
      retrieve_project_files ~notify_back clerk_config clerk_root_dir
    in
    { project_dir; project_kind; project_files }

let project_of_workspace_folder ~notify_back workspace_folder =
  (* Normalize path *)
  let project_dir =
    Uri.pct_decode
      (Linol_lwt.DocumentUri.to_path
         workspace_folder.Linol_lwt.WorkspaceFolder.uri)
  in
  project_of_folder ~notify_back project_dir

let init ~notify_back (params : Linol_lwt.InitializeParams.t) : projects =
  let ( let*? ) (x, err_msg) f =
    match x with
    | None ->
      err_msg ();
      Projects.empty
    | Some x -> f x
  in
  Log.debug (fun m -> m "initializing project");
  let no_workspace_folder_provided () =
    Log.warn (fun m -> m "no workspace folder provided")
  in
  let*? workspace_folders =
    params.workspaceFolders, no_workspace_folder_provided
  in
  let workspace_folders =
    match workspace_folders with None | Some [] -> None | x -> x
  in
  let*? workspace_folders = workspace_folders, no_workspace_folder_provided in
  List.fold_left
    (fun projects workspace_folder ->
      let project = project_of_workspace_folder ~notify_back workspace_folder in
      Log.app (fun m -> m "project %s loaded" project.project_dir);
      Log.debug (fun m -> m "@[<v 2>Projects:@ %a@]" format_project project);
      Projects.add project projects)
    Projects.empty workspace_folders

let find_file_in_project doc_id (project : project) =
  Doc_id.(Map.find_opt doc_id project.project_files)

let reload_project ~notify_back project projects =
  let project = project_of_folder ~notify_back project.project_dir in
  project, Projects.add project projects

exception Project_not_found

let find_or_populate_project ~notify_back (doc_id : Doc_id.t) projects =
  let scan_dir () =
    let new_project =
      project_of_folder ~notify_back (Filename.dirname (doc_id :> File.t))
    in
    match find_file_in_project doc_id new_project with
    | None ->
      Log.err (fun m ->
          m "did not find project for file %a after scanning" Doc_id.format
            doc_id);
      raise Project_not_found
    | Some file ->
      file, new_project, `Changed (Projects.add new_project projects)
  in
  match lookup_project doc_id projects with
  | Some project -> (
    let file_opt = find_file_in_project doc_id project in
    match file_opt with
    | None ->
      (* File is not found, let's rescan the whole project.. *)
      scan_dir ()
    | Some file -> file, project, `Unchanged)
  | None -> scan_dir ()
