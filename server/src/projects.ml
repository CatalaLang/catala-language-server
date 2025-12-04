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

let to_doc_id (i : Clerk_scan.item) = Doc_id.of_file i.file_name

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
module ModuleMap = String.Map

type project_file = {
  file : Clerk_scan.item;
  including_files : ScanItemFiles.t;
  used_by : ScanItemFiles.t;
}

type project_kind =
  | Clerk of { clerk_root_dir : string; clerk_config : Clerk_config.t }
  | No_clerk

module Project_graph = struct
  type relation = Used_by | Including

  module G :
    Graph.Sig.P
      with type V.t = Doc_id.t
       and type E.t = Doc_id.t * relation * Doc_id.t
       and type E.label = relation =
    Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
      (struct
        type t = Doc_id.t

        let hash (doc_id : Doc_id.t) = Hashtbl.hash (doc_id :> string)

        let compare (x : Doc_id.t) (y : Doc_id.t) =
          String.compare (x :> string) (y :> string)

        let equal (x : Doc_id.t) (y : Doc_id.t) =
          String.equal (x :> string) (y :> string)
      end)
      (struct
        type t = relation

        let compare = compare
        let default = Used_by
      end)

  type t = G.t

  let build_graph project_files =
    Doc_id.Map.fold
      (fun _ { file; including_files; used_by } g ->
        let d_file = to_doc_id file in
        let g = G.add_vertex g d_file in
        let g =
          ScanItemFiles.fold
            (fun file' g -> G.add_edge_e g (to_doc_id file', Including, d_file))
            including_files g
        in
        let g =
          ScanItemFiles.fold
            (fun file' g -> G.add_edge_e g (to_doc_id file', Used_by, d_file))
            used_by g
        in
        g)
      project_files G.empty

  let remove_vertex
      ~(item : Clerk_scan.item)
      ~(compute_using_modules : Doc_id.t -> ScanItemFiles.t)
      g : t * Doc_id.Set.t (* returns (new graph * affected docs) *) =
    let deleted_doc_id = to_doc_id item in
    assert (G.mem_vertex g deleted_doc_id);
    let pred_documents =
      List.filter_map
        (function v, Including, _ -> Some v | _ -> None)
        (G.pred_e g deleted_doc_id)
    in
    let using_documents =
      List.filter_map
        (function _, Used_by, v -> Some v | _ -> None)
        (G.succ_e g deleted_doc_id)
    in
    (* module using this module should also try to relink to possible modules *)
    let g = G.remove_vertex g deleted_doc_id in
    let g =
      List.fold_left
        (fun g d ->
          let modules = compute_using_modules d in
          ScanItemFiles.fold
            (fun item g -> G.add_edge_e g (to_doc_id item, Used_by, d))
            modules g)
        g using_documents
    in
    g, Doc_id.Set.of_list (pred_documents @ using_documents)

  let update_vertex
      ~(prev_item : Clerk_scan.item)
      ~(new_item : Clerk_scan.item)
      ~(compute_used_modules : unit -> ScanItemFiles.t)
      g =
    let prev_doc_id = to_doc_id prev_item in
    let doc_id = to_doc_id new_item in
    assert (G.mem_vertex g prev_doc_id);
    let pred_edges =
      List.map
        (fun (v, r, _) -> v, r, doc_id)
        (G.pred_e g (to_doc_id prev_item))
    in
    let succ_edges =
      let using_edges =
        ScanItemFiles.fold
          (fun item el -> (doc_id, Used_by, to_doc_id item) :: el)
          (compute_used_modules ()) []
      in
      List.fold_left
        (fun el included_file ->
          let included_file = Mark.remove included_file in
          assert (not (Filename.is_relative included_file));
          (doc_id, Including, Doc_id.of_file included_file) :: el)
        using_edges new_item.included_files
    in
    let g = G.remove_vertex g prev_doc_id in
    List.fold_left G.add_edge_e g (pred_edges @ succ_edges)

  let all_affected_files ~ignored_documents doc_id g =
    let rec loop (visited, affected_files) vertex =
      if Doc_id.Set.mem vertex visited then visited, affected_files
      else
        let visited = Doc_id.Set.add vertex visited in
        let pred_edges = G.pred_e g vertex in
        let including_files =
          List.filter_map
            (function d, Including, _ -> Some d | _ -> None)
            pred_edges
        in
        let visited, affected_files =
          match including_files with
          | [] -> visited, Doc_id.Set.add vertex affected_files
          | l -> List.fold_left loop (visited, affected_files) l
        in
        let succ_edges = G.succ_e g vertex in
        let fwd_deps = List.map (fun (_, _, d) -> d) succ_edges in
        List.fold_left loop (visited, affected_files) fwd_deps
    in
    let _, affected_files =
      (* [doc_id] may be part of the processed files, let's remove it *)
      loop (Doc_id.Set.remove doc_id ignored_documents, Doc_id.Set.empty) doc_id
    in
    affected_files

  let is_an_included_file vertex g =
    let pred_edges = G.pred_e g vertex in
    List.exists (function _, Including, _ -> true | _ -> false) pred_edges

  let including_files vertex g =
    let rec loop v =
      let pred_edges = G.pred_e g v in
      let including_documents =
        List.filter_map
          (function r, Including, _ -> Some r | _ -> None)
          pred_edges
      in
      if including_documents = [] then [v]
      else List.concat_map loop including_documents
    in
    loop vertex

  let included_files vertex g =
    let succ_edges = G.succ_e g vertex in
    List.filter_map
      (function _, Including, inc -> Some inc | _ -> None)
      succ_edges
end

type project = {
  project_dir : string;
  project_kind : project_kind;
  project_files : project_file Doc_id.Map.t;
  project_graph : Project_graph.t;
  known_modules : ScanItemFiles.t ModuleMap.t;
}

module Projects = Set.Make (struct
  type t = project

  let compare p1 p2 = String.compare p1.project_dir p2.project_dir
end)

type projects = Projects.t
type t = projects
type error_handler = Doc_id.t * Linol_lwt.Range.t * Diagnostic.t -> unit

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
    ~(on_error : error_handler)
    ~includes
    (file : Scan_item.t)
    (known_modules : ScanItemFiles.t ModuleMap.t)
    (used_module : string Mark.pos) : Scan_item.t option =
  let file_dir = File.dirname file.file_name in
  let includes = file_dir :: includes in
  let used_module_name = Mark.remove used_module in
  let possible_modules =
    Option.value ~default:ScanItemFiles.empty
      (ModuleMap.find_opt used_module_name known_modules)
    |> ScanItemFiles.filter (fun m ->
           List.mem (File.dirname m.Clerk_scan.file_name) includes)
    |> ScanItemFiles.elements
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
      let diag = Diagnostic.error_p ~related mod_use_pos (`String msg) in
      on_error (to_doc_id file, Utils.range_of_pos mod_use_pos, diag);
      (* TODO: handle the multiple case list *)
      None)

let retrieve_project_files
    ~on_error
    (clerk_config : Clerk_config.t)
    ~project_dir =
  let open Clerk_scan in
  Log.info (fun m -> m "building inclusion graph");
  let tree = tree project_dir in
  let known_items : (string, item) Hashtbl.t = Hashtbl.create 10 in
  let known_modules =
    Seq.fold_left
      (fun mod_map (_, _, items) ->
        List.fold_left
          (fun mod_map item ->
            let item = clean_item item in
            Hashtbl.add known_items item.file_name item;
            match item.module_def with
            | None -> mod_map
            | Some module_def ->
              let module_def = Mark.remove module_def in
              (* Module with the same name may be declared multiple times *)
              ModuleMap.update module_def
                (function
                  | None -> Some (ScanItemFiles.singleton item)
                  | Some s -> Some (ScanItemFiles.add item s))
                mod_map)
          mod_map items)
      ModuleMap.empty tree
  in
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
  let project_files =
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
            find_module_candidate ~on_error
              ~includes:clerk_config.global.include_dirs item known_modules
              used_module
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
                    Some
                      { pf with used_by = ScanItemFiles.add modul pf.used_by })
                g)
          g item.used_modules)
      known_items g
  in
  project_files, known_modules

let project_of_folder ~on_error project_dir =
  match Utils.lookup_clerk_toml project_dir with
  | None ->
    Log.warn (fun m ->
        m "no clerk config file found, assuming default configuration");
    let project_files, known_modules =
      retrieve_project_files ~on_error Clerk_config.default_config ~project_dir
    in
    let project_kind = No_clerk in
    let project_graph = Project_graph.build_graph project_files in
    { project_dir; project_kind; project_files; project_graph; known_modules }
  | Some (clerk_config, clerk_root_dir) ->
    Log.debug (fun m -> m "clerk file found in '%s' directory" clerk_root_dir);
    let project_kind = Clerk { clerk_root_dir; clerk_config } in
    let project_files, known_modules =
      retrieve_project_files ~on_error clerk_config ~project_dir
    in
    let project_graph = Project_graph.build_graph project_files in

    (* let module M = Graph.Graphviz.Dot (struct *)
    (*   include Project_graph.G *)

    (*   let edge_attributes (_, e, _) = *)
    (*     [ *)
    (*       `Label *)
    (*         (match e with *)
    (*         | Project_graph.Used_by -> "used by" *)
    (*         | Including -> "including"); *)
    (*       `Color 4711; *)
    (*     ] *)

    (*   let default_edge_attributes _ = [] *)
    (*   let get_subgraph _ = None *)
    (*   let vertex_attributes _ = [`Shape `Box] *)

    (*   let vertex_name (v : Doc_id.t) = *)
    (*     File.(basename (v :> string) |> fun f -> f -.- "") *)

    (*   let default_vertex_attributes _ = [] *)
    (*   let graph_attributes _ = [] *)
    (* end) in *)
    (* let f = Filename.temp_file "graph" "" in *)
    (* let oc = open_out f in *)
    (* M.output_graph oc project_graph; *)
    { project_dir; project_kind; project_files; project_graph; known_modules }

let project_of_workspace_folder ~on_error workspace_folder =
  (* Normalize path *)
  let project_dir =
    Uri.pct_decode
      (Linol_lwt.DocumentUri.to_path
         workspace_folder.Linol_lwt.WorkspaceFolder.uri)
  in
  project_of_folder ~on_error project_dir

let init ~on_error (params : Linol_lwt.InitializeParams.t) : projects =
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
      let project = project_of_workspace_folder ~on_error workspace_folder in
      Log.app (fun m -> m "project %s loaded" project.project_dir);
      Log.debug (fun m -> m "@[<v 2>Projects:@ %a@]" format_project project);
      Projects.add project projects)
    Projects.empty workspace_folders

let find_file_in_project doc_id (project : project) =
  Doc_id.(Map.find_opt doc_id project.project_files)

let reload_project ~on_error project projects =
  let project = project_of_folder ~on_error project.project_dir in
  project, Projects.add project projects

exception Project_not_found

let find_or_populate_project ~on_error (doc_id : Doc_id.t) projects =
  let scan_dir () =
    let new_project =
      project_of_folder ~on_error (Filename.dirname (doc_id :> File.t))
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

let update_known_modules ~prev_item ~new_item known_modules =
  let known_modules =
    match prev_item.Clerk_scan.module_def with
    | None -> known_modules
    | Some mod_def ->
      ModuleMap.update (Mark.remove mod_def)
        (function
          | None -> None
          | Some s ->
            let s = ScanItemFiles.remove prev_item s in
            if ScanItemFiles.is_empty s then None else Some s)
        known_modules
  in
  let known_modules =
    match new_item.Clerk_scan.module_def with
    | None -> known_modules
    | Some mod_def ->
      let mod_def = Mark.remove mod_def in
      ModuleMap.update mod_def
        (function
          | None -> Some (ScanItemFiles.singleton new_item)
          | Some s -> Some (ScanItemFiles.add new_item s))
        known_modules
  in
  known_modules

let eq_item (i : Clerk_scan.item) (i' : Clerk_scan.item) =
  let open Clerk_scan in
  let {
    file_name = fn;
    module_def = md;
    extrnal = ex;
    used_modules = um;
    included_files = inc;
    has_inline_tests = _;
    has_scope_tests = _;
  } =
    i
  in
  let {
    file_name = fn';
    module_def = md';
    extrnal = ex';
    used_modules = um';
    included_files = inc';
    has_inline_tests = _;
    has_scope_tests = _;
  } =
    i'
  in
  File.equal fn fn'
  && (Option.equal (Mark.equal String.equal)) md md'
  && ex = ex'
  && List.equal (Mark.equal String.equal) um um'
  && List.equal (Mark.equal File.equal) inc inc'

type update_result = {
  projects : projects;
  project : project;
  possibly_affected_files : Doc_id.Set.t;
}

let update_project_file
    ?project
    ~on_error
    ~(ignored_documents : Doc_id.Set.t)
    (doc_id : Doc_id.t)
    projects =
  let project, projects =
    match project with
    | None -> (
      let _project_file, project, r =
        find_or_populate_project ~on_error doc_id projects
      in
      match r with
      | `Unchanged -> project, projects
      | `Changed projects ->
        Log.debug (fun m ->
            m "Did not find file %a in projects" Doc_id.format doc_id);
        project, projects)
    | Some p ->
      Log.debug (fun m ->
          m "Found file %a in project %s" Doc_id.format doc_id p.project_dir);
      p, projects
  in
  match find_file_in_project doc_id project with
  | None ->
    Log.debug (fun m -> m "Did not find file %a " Doc_id.format doc_id);
    (* File did not previously exist: let's rescan everything *)
    let project, projects = reload_project ~on_error project projects in
    let possibly_affected_files =
      Project_graph.all_affected_files ~ignored_documents doc_id
        project.project_graph
    in
    { projects; project; possibly_affected_files }
  | Some project_file ->
    (* We found the file in a project, let's check if we need to reload the
       project or not *)
    let prev_item = project_file.file in
    let new_item =
      Clerk_scan.catala_file
        (doc_id :> string)
        (Option.get (Clerk_scan.get_lang (doc_id :> string)))
    in
    if eq_item prev_item new_item then
      let possibly_affected_files =
        Project_graph.all_affected_files ~ignored_documents doc_id
          project.project_graph
      in
      { projects; project; possibly_affected_files }
    else
      let () =
        Log.debug (fun m ->
            m
              "Found existing file %a in project %s with different \
               dependencies: updating project"
              Doc_id.format doc_id project.project_dir)
      in
      let known_modules =
        update_known_modules ~prev_item ~new_item project.known_modules
      in
      let project_graph =
        Project_graph.update_vertex ~prev_item ~new_item
          ~compute_used_modules:(fun () ->
            List.filter_map
              (fun mod_use ->
                let includes =
                  match project.project_kind with
                  | Clerk { clerk_config; _ } ->
                    clerk_config.global.include_dirs
                  | No_clerk -> []
                in
                find_module_candidate ~on_error ~includes new_item known_modules
                  mod_use)
              new_item.used_modules
            |> ScanItemFiles.of_list)
          project.project_graph
      in
      let project = { project with known_modules; project_graph } in
      let possibly_affected_files =
        Project_graph.all_affected_files ~ignored_documents doc_id
          project.project_graph
      in
      Log.debug (fun m ->
          let open Format in
          m "Possibly affected files: %a"
            (pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
               Doc_id.format)
            (Doc_id.Set.elements possibly_affected_files));
      let projects = Projects.add project projects in
      { projects; project; possibly_affected_files }

let is_an_included_file doc_id project =
  Project_graph.is_an_included_file doc_id project.project_graph

let including_files doc_id project =
  Project_graph.including_files doc_id project.project_graph

let included_files doc_id project =
  Project_graph.included_files doc_id project.project_graph

let remove_project_file ~on_error doc_id project projects =
  Log.debug (fun m ->
      m "Deleting document %a from project" Doc_id.format doc_id);
  match find_file_in_project doc_id project with
  | None ->
    Log.debug (fun m ->
        m "Did not find project for removed file %a, doing nothing"
          Doc_id.format doc_id);
    (* File did not previously exist: let's rescan everything *)
    { projects; project; possibly_affected_files = Doc_id.Set.empty }
  | Some project_file ->
    (* Let's gather the dependencies and retrieve the affected files *)
    let item = project_file.file in
    let known_modules =
      (* Start by removing the known modules *)
      match item.Clerk_scan.module_def with
      | None -> project.known_modules
      | Some mod_def ->
        ModuleMap.update (Mark.remove mod_def)
          (function
            | None -> None
            | Some s ->
              let s = ScanItemFiles.remove item s in
              if ScanItemFiles.is_empty s then None else Some s)
          project.known_modules
    in
    let includes =
      match project.project_kind with
      | Clerk { clerk_config; _ } -> clerk_config.global.include_dirs
      | No_clerk -> []
    in
    let project_files =
      (* We also remove existing [used_by] relations *)
      let used_by_files =
        let open Project_graph in
        List.filter_map
          (function
            | l, Used_by, _ -> find_file_in_project l project | _ -> None)
          (G.pred_e project.project_graph doc_id)
      in
      List.fold_left
        (fun project_files (f : project_file) ->
          Doc_id.Map.update (to_doc_id f.file)
            (fun _ ->
              Some { f with used_by = ScanItemFiles.remove item f.used_by })
            project_files)
        project.project_files used_by_files
    in
    let compute_using_modules doc_id =
      match Doc_id.Map.find_opt doc_id project.project_files with
      | None -> ScanItemFiles.empty
      | Some { file; _ } ->
        List.filter_map
          (fun mod_use ->
            Log.debug (fun m ->
                m "lookup module candidate for %a" Doc_id.format doc_id);
            find_module_candidate ~on_error ~includes file known_modules mod_use)
          file.used_modules
        |> ScanItemFiles.of_list
    in
    let project_files =
      (* Don't forget to remove the file from the project *)
      Doc_id.Map.remove doc_id project_files
    in
    let project_graph, possibly_affected_files =
      Project_graph.remove_vertex ~item ~compute_using_modules
        project.project_graph
    in
    let project =
      { project with project_files; known_modules; project_graph }
    in
    Log.debug (fun m ->
        let open Format in
        m "Possibly affected files: %a"
          (pp_print_list ~pp_sep:pp_print_space Doc_id.format)
          (Doc_id.Set.elements possibly_affected_files));
    let projects = Projects.add project projects in
    { projects; project; possibly_affected_files }
