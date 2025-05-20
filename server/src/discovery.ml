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

module Scan_item = struct
  type t = Clerk_scan.item

  let compare
      { Clerk_scan.file_name; _ }
      { Clerk_scan.file_name = file_name'; _ } =
    String.compare file_name file_name'

  let format fmt { Clerk_scan.file_name; _ } =
    Format.pp_print_string fmt file_name
end

module Scan_map = Map.Make (String)
module Module_map = Map.Make (String)
module Project_files = Set.Make (Scan_item)

type project_file = {
  file : Clerk_scan.item;
  including_files : Project_files.t;
  used_by : Project_files.t;
}

type project = {
  clerk_root_dir : string;
  clerk_config : Clerk_config.t;
  project_files : project_file Scan_map.t;
}

type projects = (string * project) list

let default =
  {
    clerk_root_dir = "";
    clerk_config = Clerk_config.default_config;
    project_files = Scan_map.empty;
  }

module S = Set.Make (String)

let clean_item ({ Clerk_scan.file_name; included_files; _ } as item) =
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
  let known_items = Hashtbl.create 10 in
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
        Scan_map.add item.file_name
          {
            file = item;
            including_files = Project_files.empty;
            used_by = Project_files.empty;
          }
          g)
      known_items Scan_map.empty
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
            Scan_map.update included_item.file_name
              (function
                | None ->
                  Some
                    {
                      file = included_item;
                      including_files = Project_files.singleton item;
                      used_by = Project_files.empty;
                    }
                | Some pf ->
                  Some
                    {
                      pf with
                      including_files =
                        Project_files.add item pf.including_files;
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
            Scan_map.update item.file_name
              (function
                | None ->
                  Some
                    {
                      file = item;
                      including_files = Project_files.empty;
                      used_by = Project_files.singleton modul;
                    }
                | Some pf ->
                  Some { pf with used_by = Project_files.add modul pf.used_by })
              g)
        g item.used_modules)
    known_items g

let init ~notify_back (params : Linol_lwt.InitializeParams.t) : projects option
    =
  let open Monad.Option in
  Log.debug (fun m -> m "initializing project");
  let no_workspace_folder_provided () =
    Log.warn (fun m -> m "no workspace folder provided")
  in
  let*?! workspaceFolders =
    params.workspaceFolders, no_workspace_folder_provided
  in
  let*?! workspaceFolders = workspaceFolders, no_workspace_folder_provided in
  if workspaceFolders = [] then (
    no_workspace_folder_provided ();
    None)
  else
    List.filter_map
      (fun workspaceFolder ->
        let workspace_path =
          Uri.pct_decode
            (Linol_lwt.DocumentUri.to_path
               workspaceFolder.Linol_lwt.WorkspaceFolder.uri)
        in
        match Utils.lookup_clerk_toml workspace_path with
        | None ->
          Log.warn (fun m ->
              m
                "no clerk config file found, assuming project directory as \
                 root directory");
          let clerk_config = Clerk_config.default_config in
          let clerk_root_dir = workspace_path in
          let project_files =
            retrieve_project_files ~notify_back clerk_config clerk_root_dir
          in
          ok (workspace_path, { clerk_root_dir; clerk_config; project_files })
        | Some (clerk_config, clerk_root_dir) ->
          Log.debug (fun m ->
              m "clerk file found in '%s' directory" clerk_root_dir);
          let project_files =
            retrieve_project_files ~notify_back clerk_config clerk_root_dir
          in
          ok (workspace_path, { clerk_root_dir; clerk_config; project_files }))
      workspaceFolders
    |> function
    | [] ->
      no_workspace_folder_provided ();
      None
    | l ->
      List.iter
        (fun (project, _) -> Log.app (fun m -> m "project %s loaded" project))
        l;
      Some l
