(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
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

open Linol_lsp
open Catala_utils
open Server_types
open Lwt.Syntax
open Utils
module St = Server_state
module DQ = Doc_queries

let ( let*? ) v f =
  Lwt.bind v @@ function None -> Lwt.return_none | Some x -> f x

exception ServerError of string

let lookup_project ~on_error doc_id projects =
  match Projects.find_or_populate_project ~on_error doc_id projects with
  | file, project, `Unchanged -> (file, project), None
  | file, project, `Changed new_projects -> (file, project), Some new_projects

let set_log_level =
  let open Linol_lwt.TraceValues in
  function
  | None | Some Off ->
    Logs.app (fun m -> m "switching log level to [none]");
    Logs.set_level None
  | Some Messages ->
    Logs.set_level (Some App);
    Logs.app (fun m -> m "switching log level to [messages]")
  | Some Compact ->
    Logs.set_level (Some Info);
    Logs.app (fun m -> m "switching log level to [compact]")
  | Some Verbose ->
    Logs.set_level (Some Debug);
    Logs.app (fun m -> m "switching log level to [verbose]")

let scan_project_config = ref true

let protect_project_not_found f =
  Lwt.catch f (function
    | Projects.Project_not_found -> Lwt.return_unit
    | e -> Lwt.fail e)

let protect_project_not_found_opt f =
  Lwt.catch f (function
    | Projects.Project_not_found -> Lwt.return_none
    | e -> Lwt.fail e)

let should_ignore (uri : Doc_id.t) =
  let file = (uri :> File.t) in
  let p = File.path_to_list file in
  let b =
    List.exists (fun s -> String.length s > 0 && (s.[0] = '.' || s.[0] = '_')) p
  in
  Log.debug (fun m ->
      if b then
        m "file %s ignored: either a hidden file or inside an ignored directory"
          file);
  b

let retrieve_existing_document_if_ready doc_id server_state =
  St.use_if_ready server_state
  @@ fun { open_documents; diagnostics; _ } ->
  match Doc_id.Map.find_opt doc_id open_documents with
  | None -> Lwt.return_none
  | Some doc -> Lwt.return_some (doc, diagnostics)

let retrieve_existing_document_when_ready doc_id server_state =
  St.use_when_ready server_state
  @@ fun { open_documents; diagnostics; _ } ->
  match Doc_id.Map.find_opt doc_id open_documents with
  | None -> Lwt.return_none
  | Some doc -> Lwt.return_some (doc, diagnostics)

let retrieve_existing_document_now doc_id server_state =
  St.use_now server_state
  @@ fun { open_documents; diagnostics; _ } ->
  match Doc_id.Map.find_opt doc_id open_documents with
  | None -> Lwt.return_none
  | Some doc -> Lwt.return_some (doc, diagnostics)

let retrieve_existing_document doc_id server_state =
  St.use server_state
  @@ fun { open_documents; diagnostics; _ } ->
  match Doc_id.Map.find_opt doc_id open_documents with
  | None -> Lwt.return_none
  | Some doc -> Lwt.return_some (doc, diagnostics)

let unlocked_raw_send_all_diagnostics =
  let previous_faulty_documents = ref Doc_id.Set.empty in
  fun ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
      (diags : diagnostic Range.Map.t Doc_id.Map.t) ->
    let send_diagnostics (doc_id, diags) =
      notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
      notify_back#send_diagnostic diags
    in
    let needed_diagnostics =
      Doc_id.Map.filter
        (fun doc_id rmap ->
          (* We do not send empty diagnostics for file that weren't faulty -- it
             saves a communication *)
          if Range.Map.is_empty rmap then
            Doc_id.Set.mem doc_id !previous_faulty_documents
          else true)
        diags
    in
    (* All diagnostics are considered: we only retain non-empty needed
       diagnostics *)
    previous_faulty_documents :=
      Doc_id.Map.to_seq needed_diagnostics
      |> Seq.filter_map (fun (doc_id, rmap) ->
             if Range.Map.is_empty rmap then None else Some doc_id)
      |> Doc_id.Set.of_seq;
    Doc_id.Map.fold
      (fun doc_id diags r ->
        let* () = r in
        let diags =
          List.map (fun (_, { diag; _ }) -> diag) (Range.Map.bindings diags)
        in
        send_diagnostics (doc_id, diags))
      needed_diagnostics Lwt.return_unit

let merge_diags new_diags diags =
  Doc_id.Map.union
    (fun _ new_diags old_diags ->
      Some
        (Range.Map.union
           (fun _ new_diag _ -> Some new_diag)
           new_diags old_diags))
    new_diags diags

let unlocked_send_all_diagnostics
    ?doc_id
    ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
    { St.open_documents; diagnostics; _ } =
  let diags =
    match doc_id with
    | None -> Doc_id.Map.empty
    | Some doc_id -> Doc_id.Map.singleton doc_id Range.Map.empty
  in
  let all_diagnostics : diagnostic Range.Map.t Doc_id.Map.t =
    Doc_id.Map.fold
      (fun _doc_id doc_state acc ->
        let including_files_empty_diags =
          Projects.included_files _doc_id doc_state.St.project
          |> List.fold_left
               (fun m doc_id -> Doc_id.Map.add doc_id Range.Map.empty m)
               Doc_id.Map.empty
        in
        let diags = diagnostics in
        merge_diags (merge_diags acc diags) including_files_empty_diags)
      open_documents diags
  in
  unlocked_raw_send_all_diagnostics ~notify_back all_diagnostics

let send_all_diagnostics ?doc_id ~notify_back server_state =
  St.use_now server_state
  @@ fun unlocked_sstate ->
  unlocked_send_all_diagnostics ?doc_id ~notify_back unlocked_sstate

let unlocked_process_document document open_documents :
    bool * St.document_state * diagnostics =
  Log.info (fun m ->
      m "Processing document %s" (document.St.document_id :> string));
  let resolve_file_content path =
    (* We provide the modified document's buffer states to the Catala parser in
       order not to trigger strange errors on modified documents. *)
    let doc_id = File.clean_path path |> Doc_id.of_file in
    let document =
      if Doc_id.equal document.document_id doc_id then Some document
      else Doc_id.Map.find_opt doc_id open_documents
    in
    match document with
    | None | Some { St.buffer_state = Saved; _ } ->
      Global.FileName (doc_id :> string)
    | Some { St.buffer_state = Modified { contents }; _ } ->
      Global.Contents (contents, (doc_id :> string))
  in
  let processing_result, diags =
    Document_processing.process ~resolve_file_content document
  in
  let is_valid, document =
    match processing_result with
    | None -> false, document
    | valid_result -> true, { document with last_valid_result = valid_result }
  in
  is_valid, document, diags

let make_error_handler () =
  let m = ref Doc_id.Map.empty in
  ( m,
    fun (doc_id, range, diag) ->
      m :=
        Doc_id.Map.add doc_id
          (Range.Map.singleton range { range; lsp_error = None; diag })
          !m )

let add_diagnostics ({ St.diagnostics; _ } as server_state) new_diags =
  St.{ server_state with diagnostics = merge_diags new_diags diagnostics }

let process_affected_files project possibly_affected_files server_state =
  let open_documents = server_state.St.open_documents in
  let diagnostics, new_open_documents =
    Doc_id.Set.fold
      (fun doc_id (diagnostics, new_open_documents) ->
        (* Affected files are necessarily present in the project *)
        let project_file =
          Option.get @@ Projects.find_file_in_project doc_id project
        in
        let document =
          match Doc_id.Map.find_opt doc_id open_documents with
          | None -> St.make_document St.Saved doc_id project project_file
          | Some document -> document
        in
        let new_open_documents =
          Doc_id.Map.add doc_id document new_open_documents
        in
        if Projects.is_an_included_file doc_id project then
          (* We do not process included files or unsaved file *)
          diagnostics, new_open_documents
        else
          let _processed_file, document, new_diags =
            unlocked_process_document document open_documents
          in
          let new_diags =
            if Doc_id.Map.is_empty new_diags then
              Doc_id.Map.add document.document_id Range.Map.empty diagnostics
            else new_diags
          in
          new_diags, new_open_documents)
      possibly_affected_files
      (Doc_id.Map.empty, open_documents)
  in
  let new_server_state =
    { server_state with St.open_documents = new_open_documents }
  in
  diagnostics, new_server_state

let process_document_dependencies
    ~on_error
    doc_id
    (document : St.document_state)
    ({ St.projects; open_documents; _ } as server_state) :
    diagnostics * St.server_state =
  (* Check project files for potential errors *)
  let () =
    Log.debug (fun m ->
        m "File %a modified, checking project" Doc_id.format doc_id)
  in
  let ignored_documents =
    Doc_id.Map.fold
      (fun doc_id { St.buffer_state; _ } s ->
        if buffer_state <> Saved then Doc_id.Set.add doc_id s else s)
      open_documents Doc_id.Set.empty
  in
  let { Projects.project; projects; possibly_affected_files } =
    Projects.update_project_file ~on_error ~ignored_documents
      ~project:document.project doc_id projects
  in
  Log.debug (fun m ->
      match Doc_id.Set.elements possibly_affected_files with
      | [] -> ()
      | l ->
        m "Files to reprocess: %a"
          Format.(
            pp_print_list ~pp_sep:pp_print_space (fun fmt (s : Doc_id.t) ->
                pp_print_string fmt (s :> string)))
          l);
  let new_server_state = { server_state with projects } in
  let diagnostics, new_server_state =
    process_affected_files project possibly_affected_files new_server_state
  in
  diagnostics, new_server_state

(* FIXME: projects errors (e.g., ambiguous module usage) will be discarded when
   reprocessing a file as we do not pass through the dependency check. *)
let unlocked_process_file
    buffer_state
    doc_id
    { St.projects; open_documents; diagnostics } : St.server_state =
  let doc_errors, on_error = make_error_handler () in
  let document, projects =
    Doc_id.Map.find_opt doc_id open_documents
    |> function
    | Some document -> { document with buffer_state }, projects
    | None ->
      let (project_file, project), new_projects_opt =
        lookup_project ~on_error doc_id projects
      in
      let projects = Option.value ~default:projects new_projects_opt in
      St.make_document buffer_state doc_id project project_file, projects
  in
  let is_valid, new_document, document_diagnostics =
    unlocked_process_document { document with buffer_state } open_documents
  in
  let is_fully_saved, document_diagnostics =
    match
      (* ignore other including files*)
      Projects.ScanItemFiles.choose_opt document.project_file.including_files
    with
    | None -> buffer_state = St.Saved, document_diagnostics
    | Some { Clerk_scan.file_name; _ } ->
      (* If we are considering an included file, add an empty diag to other
         included file to reset their diagnostics. *)
      let included_files =
        Projects.included_files (Doc_id.of_file file_name) document.project
      in
      let included_files_empty_diags =
        List.map (fun f -> f, Range.Map.empty) included_files
      in
      let all_documents_saved =
        buffer_state = St.Saved
        && List.filter_map
             (fun doc_id -> Doc_id.Map.find_opt doc_id open_documents)
             included_files
           |> List.for_all (function
                | { St.buffer_state = Saved; _ } -> true
                | _ -> false)
      in
      ( all_documents_saved,
        merge_diags document_diagnostics
          (Doc_id.Map.of_list included_files_empty_diags) )
  in
  let should_process_dependencies =
    Log.debug (fun m ->
        m "should_process_dependencies %b %b %b" is_fully_saved is_valid
          !scan_project_config);
    is_fully_saved && is_valid && !scan_project_config
  in
  let new_server_state =
    (* Update server state with the new processed document : we will add updated
       diagnostics later on *)
    let open_documents =
      Doc_id.Map.add new_document.document_id new_document open_documents
    in
    { St.projects; open_documents; diagnostics }
  in
  let new_diagnostics, new_server_state =
    if should_process_dependencies then
      let other_diagnostics, new_server_state =
        process_document_dependencies ~on_error doc_id new_document
          new_server_state
      in
      other_diagnostics, new_server_state
    else document_diagnostics, new_server_state
  in
  let diagnostics =
    let new_diagnostics = merge_diags !doc_errors new_diagnostics in
    Doc_id.Map.union
      (fun _doc_id new_diag _ -> Some new_diag)
      new_diagnostics diagnostics
  in
  { new_server_state with diagnostics }

let process_saved_file server_state doc_id =
  St.use_and_update server_state
  @@ fun unlocked_server_state ->
  let new_state = unlocked_process_file St.Saved doc_id unlocked_server_state in
  Lwt.return ((), new_state)

let server_initialized, resolve_init = Lwt.wait ()

class catala_lsp_server =
  let open Linol_lwt in
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    method spawn_query_handler = Lwt.async

    val mutable initialize_params : InitializeParams.t =
      (* Placeholder overwrote in [on_req_initialize] *)
      InitializeParams.create ~capabilities:(ClientCapabilities.create ()) ()

    (* Extra-configurations *)
    method! config_code_action_provider = `Bool true
    method! config_completion = Some (CompletionOptions.create ())
    method! config_definition = Some (`Bool true)
    method! config_hover = Some (`Bool true)
    method! config_symbol = Some (`Bool false)

    method! config_code_lens_options : CodeLensOptions.t option =
      Some { resolveProvider = Some true; workDoneProgress = None }

    method private config_workspace_symbol = `Bool false
    method private config_declaration = Some (`Bool true)
    method private config_references = Some (`Bool true)
    method private config_type_definition = Some (`Bool true)

    method! config_sync_opts =
      (* configure how sync happens *)
      let change = TextDocumentSyncKind.Incremental in
      TextDocumentSyncOptions.create ~openClose:true ~change
        ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
        ()

    (* Server state *)
    val server_state : St.locked_server_state = St.make ()

    method! on_req_initialize
        ~notify_back:_
        (i : InitializeParams.t)
        : InitializeResult.t t =
      Logs.app (fun m -> m "Starting lsp server");
      initialize_params <- i;
      let sync_opts = self#config_sync_opts in
      let capabilities =
        ServerCapabilities.create
          ?codeLensProvider:self#config_code_lens_options
          ~codeActionProvider:self#config_code_action_provider
          ~executeCommandProvider:
            (ExecuteCommandOptions.create ~commands:self#config_list_commands ())
          ~documentFormattingProvider:(`Bool true)
          ?completionProvider:self#config_completion
          ?definitionProvider:self#config_definition
          ?declarationProvider:self#config_declaration
          ?referencesProvider:self#config_references
          ?hoverProvider:self#config_hover
          ?inlayHintProvider:self#config_inlay_hints
          ?documentSymbolProvider:self#config_symbol
          ~textDocumentSync:(`TextDocumentSyncOptions sync_opts)
          ~workspaceSymbolProvider:self#config_workspace_symbol
          ?typeDefinitionProvider:self#config_type_definition ()
      in
      Lwt.return (InitializeResult.create ~capabilities ())

    method private process_saved_document
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (doc_id : Doc_id.t) =
      if should_ignore doc_id then Lwt.return_unit
      else
        protect_project_not_found
        @@ fun () ->
        let* () = process_saved_file server_state doc_id in
        send_all_diagnostics ~doc_id ~notify_back server_state

    method on_notif_doc_did_open ~notify_back d ~content:_ =
      let doc_id = Doc_id.of_lsp_uri d.uri in
      self#process_saved_document ~notify_back doc_id

    method private document_changed ~notify_back ~new_contents doc_id =
      if should_ignore doc_id then Lwt.return_unit
      else
        protect_project_not_found
        @@ fun () ->
        notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
        St.delayed_update doc_id server_state
        @@ fun state ->
        let new_state =
          unlocked_process_file
            (St.Modified { contents = new_contents })
            doc_id state
        in
        let* () =
          unlocked_send_all_diagnostics ~doc_id ~notify_back new_state
        in
        Lwt.return new_state

    method on_notif_doc_did_change
        ~notify_back
        d
        (_c : TextDocumentContentChangeEvent.t list)
        ~old_content:_
        ~new_content:new_contents =
      self#document_changed ~notify_back ~new_contents (Doc_id.of_lsp_uri d.uri)

    method! on_notif_doc_did_save ~notify_back d =
      let doc_id = Doc_id.of_lsp_uri d.textDocument.uri in
      self#process_saved_document ~notify_back doc_id

    method private on_doc_delete ~notify_back (doc_id : Doc_id.doc_id) =
      if should_ignore doc_id then Lwt.return_unit
      else
        protect_project_not_found
        @@ fun () ->
        St.use_and_update server_state
        @@ fun ({ projects; open_documents; diagnostics = _ } as sstate) ->
        let doc_errors, on_error = make_error_handler () in
        match Projects.lookup_project doc_id projects with
        | None ->
          (* Shouldn't happen but nothing to do otherwise *)
          Lwt.return ((), sstate)
        | Some project ->
          let { Projects.projects; project; possibly_affected_files } =
            Projects.remove_project_file ~on_error doc_id project projects
          in
          let new_diagnostics, new_server_state =
            process_affected_files project possibly_affected_files sstate
          in
          let diagnostics =
            let existing_diagnostics =
              Doc_id.Map.remove doc_id new_server_state.diagnostics
            in
            let new_diagnostics = merge_diags !doc_errors new_diagnostics in
            Doc_id.Map.union
              (fun _doc_id new_diag _ -> Some new_diag)
              new_diagnostics existing_diagnostics
          in
          let new_state = { St.projects; open_documents; diagnostics } in
          let* () = unlocked_send_all_diagnostics ~notify_back new_state in
          Lwt.return ((), new_state)

    method private on_notif_did_change_watched_files ~notify_back changes =
      (* Triggers even on save.. *)
      Lwt_list.iter_p
        (fun { FileEvent.uri; type_ } ->
          let doc_id = Doc_id.of_lsp_uri uri in
          match type_ with
          | Created -> self#process_saved_document ~notify_back doc_id
          | Changed -> self#process_saved_document ~notify_back doc_id
          | Deleted ->
            let* () = self#on_doc_delete ~notify_back doc_id in
            self#on_doc_did_close ~notify_back doc_id)
        changes

    method private scan_project
        ~notify_back
        { St.projects; open_documents; diagnostics = _ } =
      let diagnostics, open_documents =
        Projects.Projects.fold
          (fun project documents ->
            Doc_id.Map.fold
              (fun doc_id _ (diagnostics, documents) ->
                if Projects.is_an_included_file doc_id project then
                  (* We do not consider included files *)
                  diagnostics, documents
                else
                  (* Affected files are necessarily present in the project *)
                  let project_file =
                    Option.get @@ Projects.find_file_in_project doc_id project
                  in
                  let document =
                    match Doc_id.Map.find_opt doc_id documents with
                    | None ->
                      St.make_document St.Saved doc_id project project_file
                    | Some document -> document
                  in
                  let _processed_file, document, document_diagnostics =
                    unlocked_process_document document open_documents
                  in
                  let new_diagnostics =
                    merge_diags document_diagnostics diagnostics
                  in
                  new_diagnostics, Doc_id.Map.add doc_id document documents)
              project.project_files documents)
          projects
          (Doc_id.Map.empty, open_documents)
      in
      let sstate = { St.projects; open_documents; diagnostics } in
      let* () = unlocked_send_all_diagnostics ~notify_back sstate in
      Lwt.return sstate

    method! on_notification_unhandled ~notify_back (n : Client_notification.t) =
      match n with
      | SetTrace params ->
        set_log_level (Some params.value);
        Lwt.return_unit
      | UnknownNotification notif ->
        let json = Linol.Jsonrpc.Notification.yojson_of_t notif in
        Log.warn (fun m -> m "unknown notification: %a" Yojson.Safe.pp json);
        Lwt.return_unit
      | DidChangeWatchedFiles { changes } ->
        self#on_notif_did_change_watched_files ~notify_back changes
      | Initialized ->
        let* () =
          let* project_scanning_enabled =
            lookup_catala_enable_project_scan ~notify_back
          in
          Lwt.return
          @@
          match project_scanning_enabled with
          | None -> scan_project_config := true
          | Some b -> scan_project_config := b
        in
        let* () =
          let* available =
            Utils.check_catala_format_availability ~notify_back
          in
          if available then Lwt.return_unit
          else
            let* () =
              Utils.send_notification ~type_:MessageType.Warning ~notify_back
                "Could not find 'catala-format', code formatting is unavailable.\n\
                 Follow the instructions in the \
                 [README](https://github.com/CatalaLang/catala-language-server?tab=readme-ov-file#code-formatting) \
                 to setup code formatting."
            in
            Lwt.return_unit
        in
        set_log_level initialize_params.trace;
        St.use_and_update server_state
        @@ fun sstate ->
        let errors, on_error = make_error_handler () in
        let projects = Projects.init ~on_error initialize_params in
        let sstate = { sstate with projects } in
        let* sstate =
          if !scan_project_config then self#scan_project ~notify_back sstate
          else Lwt.return sstate
        in
        let* () = unlocked_raw_send_all_diagnostics ~notify_back !errors in
        Lwt.wakeup resolve_init ();
        Lwt.return ((), sstate)
      | _ -> Lwt.return_unit

    method private on_req_get_all_scopes ~tests_only () : Yojson.Safe.t Lwt.t =
      let open Shared_ast in
      let* () = server_initialized in
      St.use_now server_state
      @@ fun { projects; _ } ->
      let scopes =
        Projects.Projects.fold
          (fun ({ Projects.project_files; _ } as project) acc ->
            (* FIXME: deleted files are actually not deleted? *)
            Doc_id.Map.fold
              (fun doc_id _f acc ->
                if Projects.is_an_included_file doc_id project then acc
                else
                  match Utils.list_scopes ~tests_only (doc_id :> string) with
                  | [] -> acc
                  | scopes -> Doc_id.Map.add doc_id scopes acc
                  | exception e ->
                    Log.err (fun m ->
                        m "Cannot read file %a: %s" Doc_id.format doc_id
                          (Printexc.to_string e));
                    acc)
              project_files acc)
          projects Doc_id.Map.empty
      in
      let json_list : Yojson.Safe.t =
        `List
          (Doc_id.Map.bindings scopes
          |> List.map (fun ((doc_id : Doc_id.t), scopes) ->
                 `Assoc
                   [
                     "path", `String (doc_id :> string);
                     ( "scopes",
                       `List
                         (List.map
                            (fun s ->
                              let pos = Mark.get (ScopeName.get_info s) in
                              `Assoc
                                [
                                  ( "name",
                                    `String (Shared_ast.ScopeName.to_string s) );
                                  ( "range",
                                    Range.yojson_of_t (Utils.range_of_pos pos) );
                                ])
                            scopes) );
                   ]))
      in
      Lwt.return json_list

    method private on_req_scope projects list_f =
      let scopes =
        Projects.Projects.fold
          (fun ({ Projects.project_files; _ } as project) acc ->
            Doc_id.Map.fold
              (fun doc_id _f acc ->
                if Projects.is_an_included_file doc_id project then acc
                else
                  match list_f (doc_id :> string) with
                  | [] -> acc
                  | scopes -> Doc_id.Map.add doc_id scopes acc)
              project_files acc)
          projects Doc_id.Map.empty
      in
      let json_list : Yojson.Safe.t =
        `List
          (Doc_id.Map.bindings scopes
          |> List.map (fun ((doc_id : Doc_id.t), scopes) ->
                 `Assoc
                   [
                     "path", `String (doc_id :> string);
                     ( "scopes",
                       `List
                         (List.map
                            (fun s ->
                              `String (Shared_ast.ScopeName.to_string s))
                            scopes) );
                   ]))
      in
      Lwt.return json_list

    method private on_req_get_all_testable_scopes params : Yojson.Safe.t Lwt.t =
      let workspace_path_opt =
        match params with
        | Some p -> (
          match Linol.Jsonrpc.Structured.yojson_of_t p with
          | `String s -> Some s
          | `List [`String s] -> Some s
          | `Null -> None
          | _ -> None)
        | None -> None
      in
      St.use_now server_state
      @@ fun { projects; _ } ->
      let projects_to_scan =
        match workspace_path_opt with
        | None -> projects
        | Some (workspace_path as p) -> (
          (* pick the first project whose directory is a prefix of the given
             path *)
          let target =
            Projects.Projects.elements projects
            |> List.find_opt (fun (project : Projects.project) ->
                   let dir = project.project_dir in
                   String.length p >= String.length dir
                   && String.equal dir (String.sub p 0 (String.length dir)))
          in
          match target with
          | Some proj -> Projects.Projects.singleton proj
          | None ->
            Log.warn (fun m ->
                m
                  "Could not find a project in %s: scanning scopes in all \
                   projects"
                  workspace_path);
            projects)
      in
      self#on_req_scope projects_to_scan Utils.list_testable_scopes

    method! on_unknown_request ~notify_back ~server_request:_ ~id meth params =
      self#on_request_unhandled ~notify_back ~id
        (Client_request.UnknownRequest { meth; params })

    method! on_request_unhandled : type r.
        notify_back:_ -> id:_ -> r Client_request.t -> r Lwt.t =
      (* Override to process other requests *)
      fun ~notify_back ~id r ->
        match r with
        | TextDocumentDeclaration (params : TextDocumentPositionParams.t) ->
          self#on_req_declaration ~notify_back ~uri:params.textDocument.uri
            ~pos:params.position ()
        | TextDocumentReferences (params : ReferenceParams.t) ->
          self#on_req_references ~notify_back ~uri:params.textDocument.uri
            ~pos:params.position ()
        | TextDocumentTypeDefinition (params : TypeDefinitionParams.t) ->
          self#on_req_type_definition ~notify_back ~uri:params.textDocument.uri
            ~pos:params.position ()
        | TextDocumentFormatting params ->
          self#on_req_document_formatting ~notify_back params
        | UnknownRequest { meth = "catala.getRunnableScopes"; params = _ } ->
          self#on_req_get_all_scopes ~tests_only:false ()
        | UnknownRequest { meth = "catala.getTestScopes"; params = _ } ->
          self#on_req_get_all_scopes ~tests_only:true ()
        | UnknownRequest { meth = "catala.getTestableScopes"; params } ->
          self#on_req_get_all_testable_scopes params
        | UnknownRequest { meth; _ } ->
          Format.kasprintf Lwt.fail_with "Unknown LSP request received: %s" meth
        | _ -> super#on_request_unhandled ~notify_back ~id r

    method private on_doc_did_close ~notify_back:_ (doc_id : Doc_id.t) =
      if should_ignore doc_id then Lwt.return_unit
      else
        St.use_and_update server_state
        @@ fun { projects; open_documents; diagnostics } ->
        let open_documents = Doc_id.Map.remove doc_id open_documents in
        let diagnostics = Doc_id.Map.remove doc_id diagnostics in
        Lwt.return ((), { St.projects; open_documents; diagnostics })

    method on_notif_doc_did_close ~notify_back d =
      self#on_doc_did_close ~notify_back (Doc_id.of_lsp_uri d.uri)

    method! on_req_code_action
        ~notify_back:_
        ~id:_
        {
          textDocument;
          range;
          context = _;
          partialResultToken = _;
          workDoneToken = _;
        }
        : CodeActionResult.t Lwt.t =
      let doc_id = Doc_id.of_lsp_uri textDocument.uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let* r = retrieve_existing_document_when_ready doc_id server_state in
        match r with
        | None -> Lwt.return_none
        | Some ({ St.document_id; _ }, diagnostics) ->
          let suggestions_opt =
            DQ.lookup_suggestions document_id diagnostics range
          in
          let actions_opt : CodeAction.t list option =
            Option.map
              (fun (range, suggestions) ->
                let changes : (DocumentUri.t * TextEdit.t list) list option =
                  Option.some
                  @@ List.map
                       (fun suggestion ->
                         ( textDocument.uri,
                           [TextEdit.create ~range ~newText:suggestion] ))
                       suggestions
                in
                [
                  CodeAction.create ~title:"suggestions"
                    ~kind:CodeActionKind.QuickFix ~isPreferred:true
                    ~edit:
                      {
                        changes;
                        documentChanges = None;
                        changeAnnotations = None;
                      }
                    ();
                ])
              suggestions_opt
          in
          let result =
            Option.map
              (fun l -> List.map (fun action -> `CodeAction action) l)
              actions_opt
          in
          Lwt.return result

    method! on_req_completion
        ~notify_back:_
        ~id:_
        ~uri
        ~pos
        ~ctx:_
        ~workDoneToken:_
        ~partialResultToken:_
        _doc_state =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let*? { St.document_id; _ }, diagnostics =
          retrieve_existing_document_now doc_id server_state
        in
        let suggestions_opt =
          DQ.lookup_suggestions_by_pos document_id diagnostics pos
        in
        match suggestions_opt with
        | None -> Lwt.return_none
        | Some (range, suggestions) ->
          Lwt.return
          @@ Some
               (`List
                 (List.map
                    (fun sugg ->
                      let textEdit =
                        `TextEdit (TextEdit.create ~range ~newText:sugg)
                      in
                      CompletionItem.create ~label:sugg ~textEdit ())
                    suggestions))

    method! on_req_definition
        ~notify_back:_
        ~id:_
        ~uri
        ~pos
        ~workDoneToken:_
        ~partialResultToken:_
        _doc_state =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let*? doc, _diagnostics =
          retrieve_existing_document doc_id server_state
        in
        match DQ.lookup_def ~doc_id doc pos with
        | None -> Lwt.return_none
        | Some l ->
          let locs =
            List.map
              (fun (file, range) ->
                let uri = DocumentUri.of_path file in
                Location.create ~range ~uri)
              l
          in
          if locs = [] then Lwt.return_none
          else Lwt.return_some (`Location locs)

    method private on_req_declaration
        ~notify_back:_
        ~(uri : Uri0.t)
        ~(pos : Position.t)
        ()
        : Locations.t option t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let*? doc, _ = retrieve_existing_document doc_id server_state in
        match DQ.lookup_declaration ~doc_id doc pos with
        | None -> Lwt.return_none
        | Some l ->
          let locations =
            List.map
              (fun (file, range) ->
                let uri = DocumentUri.of_path file in
                Location.create ~range ~uri)
              l
          in
          let locs : Linol_lwt.Locations.t = `Location locations in
          Lwt.return_some locs

    method private on_req_references
        ~notify_back:_
        ~(uri : Uri0.t)
        ~(pos : Position.t)
        ()
        : Location.t list option Lwt.t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let*? doc, _ = retrieve_existing_document doc_id server_state in
        match DQ.lookup_usages ~doc_id doc pos with
        | None -> Lwt.return_none
        | Some l ->
          let locs =
            List.map
              (fun (file, range) ->
                let uri = DocumentUri.of_path file in
                Location.create ~range ~uri)
              l
          in
          if locs = [] then Lwt.return_none else Lwt.return_some locs

    method! on_req_hover
        ~notify_back:_
        ~id:_
        ~uri
        ~pos
        ~workDoneToken:_
        _doc_state
        : Hover.t option Lwt.t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let*? doc, _ = retrieve_existing_document doc_id server_state in
        let markdown =
          let ( let*? ) = Option.bind in
          let*? { hover; _ } = initialize_params.capabilities.textDocument in
          let*? { contentFormat; _ } = hover in
          Option.map (fun l -> List.mem MarkupKind.Markdown l) contentFormat
        in
        match DQ.get_hover_type ?markdown doc pos with
        | None -> Lwt.return_none
        | Some content -> Lwt.return_some content

    method private on_req_type_definition
        ~notify_back:_
        ~(uri : Uri0.t)
        ~(pos : Position.t)
        ()
        : Locations.t option Lwt.t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let*? doc, _ = retrieve_existing_document doc_id server_state in
        match DQ.lookup_type_declaration doc pos with
        | None -> Lwt.return_none
        | Some (file, range) ->
          let uri = DocumentUri.of_path file in
          let loc = Location.create ~range ~uri in
          Lwt.return_some (`Location [loc])

    method! on_req_symbol
        ~notify_back:_
        ~id:_
        ~uri
        ~workDoneToken:_
        ~partialResultToken:_
        ()
        : [ `DocumentSymbol of DocumentSymbol.t list
          | `SymbolInformation of SymbolInformation.t list ]
          option
          t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        protect_project_not_found_opt
        @@ fun () ->
        let*? doc, _ = retrieve_existing_document doc_id server_state in
        let all_symbols = DQ.lookup_document_symbols doc in
        Lwt.return_some (`SymbolInformation all_symbols)

    method! on_req_code_lens
        ~notify_back:_
        ~id:_
        ~uri
        ~workDoneToken:_
        ~partialResultToken:_
        _doc_state
        : CodeLens.t list Lwt.t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_nil
      else
        let* r =
          protect_project_not_found_opt
          @@ fun () ->
          let*? doc, _ =
            retrieve_existing_document_when_ready doc_id server_state
          in
          Lwt.return (DQ.lookup_lenses doc)
        in
        match r with None -> Lwt.return_nil | Some l -> Lwt.return l

    method private on_req_document_formatting
        ~notify_back
        (params : DocumentFormattingParams.t)
        : TextEdit.t list option Lwt.t =
      let doc_id = Doc_id.of_lsp_uri params.textDocument.uri in
      if should_ignore doc_id then Lwt.return_none
      else
        protect_project_not_found_opt
        @@ fun () ->
        St.use server_state
        @@ fun _ ->
        Log.info (fun m ->
            m "trying to format document %a" Doc_id.format doc_id);
        match self#find_doc params.textDocument.uri with
        | None ->
          Log.warn (fun m ->
              m "cannot find document content of document %a" Doc_id.format
                doc_id);
          Lwt.return_none
        | Some { content = doc_content; _ } -> (
          let* r =
            Formatting.try_format_document ~notify_back ~doc_content doc_id
          in
          match r with
          | None ->
            Log.info (fun m -> m "failed to format document");
            Lwt.return_none
          | Some r ->
            Log.info (fun m -> m "document formatting done");
            Lwt.return_some r)
  end
