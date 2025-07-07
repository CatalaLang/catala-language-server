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

open Catala_utils
open Server_types
open Lwt.Syntax
open Utils
module SState = Server_state

let ( let*? ) v f =
  Lwt.bind v @@ function None -> Lwt.return_none | Some x -> f x

exception ServerError of string

let lookup_project ~notify_back doc_id projects =
  match Projects.find_or_populate_project ~notify_back doc_id projects with
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
  SState.use_if_ready server_state
  @@ fun { open_documents; _ } ->
  match Doc_id.Map.find_opt doc_id open_documents with
  | Some { last_valid_result = Some f; _ } -> Lwt.return_some f
  | _ -> Lwt.return_none

let retrieve_existing_document_now doc_id server_state =
  SState.use_now server_state
  @@ fun { open_documents; _ } ->
  match Doc_id.Map.find_opt doc_id open_documents with
  | Some { last_valid_result = Some f; _ } -> Lwt.return_some f
  | _ -> Lwt.return_none

let retrieve_existing_document doc_id server_state =
  SState.use server_state
  @@ fun { open_documents; _ } ->
  match Doc_id.Map.find_opt doc_id open_documents with
  | Some { last_valid_result = Some f; _ } -> Lwt.return_some f
  | _ -> Lwt.return_none

let unlocked_send_all_diagnostics =
  let previous_faulty_documents = ref Doc_id.Set.empty in
  fun ~notify_back { SState.open_documents; _ } ->
    let open Lwt.Syntax in
    let send_diagnostics (doc_id, diags) =
      notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
      notify_back#send_diagnostic diags
    in
    let all_diagnostics : Diagnostic.t RangeMap.t Doc_id.Map.t =
      Doc_id.Map.fold
        (fun _doc_id doc_state acc ->
          Doc_id.Map.union
            (fun _ l r -> Some (RangeMap.union (fun _ l _ -> Some l) l r))
            doc_state.SState.errors acc)
        open_documents Doc_id.Map.empty
    in
    let extra_diagnostics =
      Doc_id.Set.fold
        (fun doc_id acc ->
          if Doc_id.Map.mem doc_id all_diagnostics then acc
          else Doc_id.Map.add doc_id RangeMap.empty acc)
        !previous_faulty_documents Doc_id.Map.empty
    in
    let all_diagnostics =
      Doc_id.Map.union
        (fun _ _ _ -> assert false)
        extra_diagnostics all_diagnostics
    in
    Doc_id.Map.fold
      (fun doc_id diags r ->
        let* () = r in
        Log.debug (fun m ->
            m "sending diagnostics of file %a" Doc_id.format doc_id);
        let diags = List.map snd (RangeMap.bindings diags) in
        send_diagnostics (doc_id, diags))
      all_diagnostics Lwt.return_unit

let send_all_diagnostics ~notify_back server_state =
  SState.use_now server_state
  @@ fun unlocked_sstate ->
  unlocked_send_all_diagnostics ~notify_back unlocked_sstate

let unlocked_process_document document :
    State.file * State.file SState.document_state =
  Log.info (fun m ->
      m "Processing document %s" (document.SState.document_id :> string));
  let new_file =
    State.process_document ?contents:document.SState.contents document
  in
  let errors = State.all_diagnostics new_file in
  let document = { document with errors } in
  if Option.is_some new_file.result then
    new_file, { document with last_valid_result = Some new_file }
  else new_file, document

let unlocked_process_file
    ?contents
    ~is_saved
    ~notify_back
    doc_id
    { SState.projects; open_documents } :
    (State.file * State.file SState.server_state) Lwt.t =
  let document, projects =
    Doc_id.Map.find_opt doc_id open_documents
    |> function
    | Some document -> { document with saved = is_saved; contents }, projects
    | None ->
      let (project_file, project), new_projects_opt =
        lookup_project ~notify_back doc_id projects
      in
      let projects = Option.value ~default:projects new_projects_opt in
      ( SState.make_document ?contents ~saved:is_saved doc_id project
          project_file,
        projects )
  in
  let new_file, new_document =
    unlocked_process_document { document with contents }
  in
  let is_valid = Option.is_some new_file.result in
  let open_documents = Doc_id.Map.add doc_id new_document open_documents in
  if is_saved && is_valid && !scan_project_config then (
    (* Check project files for potential errors *)
    let () =
      Log.debug (fun m ->
          m "File %a modified, checking project" Doc_id.format doc_id)
    in
    let ignored_documents =
      Doc_id.Map.fold
        (fun doc_id { SState.saved; _ } s ->
          if not saved then Doc_id.Set.add doc_id s else s)
        open_documents Doc_id.Set.empty
    in
    let { Projects.project; projects; possibly_affected_files } =
      Projects.update_project_file ~notify_back ~ignored_documents
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
    let modified_documents =
      Doc_id.Set.fold
        (fun doc_id documents ->
          if Projects.is_an_included_file doc_id project then
            (* We do not consider included files *)
            documents
          else
            (* Affected files are necessarily present in the project *)
            let project_file =
              Option.get @@ Projects.find_file_in_project doc_id project
            in
            let document =
              match Doc_id.Map.find_opt doc_id open_documents with
              | None ->
                SState.make_document ?contents:None ~saved:true doc_id project
                  project_file
              | Some document -> document
            in
            let _processed_file, document =
              unlocked_process_document document
            in
            Doc_id.Map.add doc_id document documents)
        possibly_affected_files Doc_id.Map.empty
    in
    let open_documents =
      Doc_id.Map.union
        (fun _ modified_doc _ -> Some modified_doc)
        modified_documents open_documents
    in
    Lwt.return (new_file, SState.{ projects; open_documents }))
  else
    (* Unsaved or invalid : return without scanning files *)
    Lwt.return (new_file, { SState.projects; open_documents })

let process_file ?contents ~is_saved ~notify_back server_state doc_id =
  SState.use_and_update server_state
  @@ fun unlocked_server_state ->
  let* new_file, new_state =
    unlocked_process_file ?contents ~is_saved ~notify_back doc_id
      unlocked_server_state
  in
  Lwt.return (new_file, new_state)

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
    val server_state : State.file SState.locked_server_state = SState.make ()

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

    method private use_or_process_file ~notify_back ~is_saved doc_id =
      let* (doc_opt : State.file SState.document_state option) =
        SState.use server_state
        @@ fun { projects = _; open_documents } ->
        Lwt.return (Doc_id.Map.find_opt doc_id open_documents)
      in
      match doc_opt with
      | Some { SState.last_valid_result = None; _ } | None ->
        process_file ~notify_back ~is_saved server_state doc_id
      | Some { last_valid_result = Some x; _ } -> Lwt.return x

    method private process_document
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        ~is_saved
        ?(is_open = false)
        (doc_id : Doc_id.t)
        (contents : string option) =
      if should_ignore doc_id then Lwt.return_unit
      else
        protect_project_not_found
        @@ fun () ->
        let* should_skip =
          SState.use server_state
          @@ fun sstate ->
          if not is_open then Lwt.return_false
          else
            match Doc_id.Map.find_opt doc_id sstate.open_documents with
            | Some { last_valid_result = Some _; _ } ->
              let () =
                Log.debug (fun m ->
                    m "document %a already in cache, skipping validation"
                      Doc_id.format doc_id)
              in
              Lwt.return_true
            | _ -> Lwt.return_false
        in
        if should_skip then Lwt.return_unit
        else
          let* _file =
            process_file ?contents ~is_saved ~notify_back server_state doc_id
          in
          send_all_diagnostics ~notify_back server_state

    method on_notif_doc_did_open ~notify_back d ~content =
      let doc_id = Doc_id.of_lsp_uri d.uri in
      self#process_document ~notify_back ~is_open:true ~is_saved:true doc_id
        (Some content)

    method private document_changed ~notify_back ?new_content doc_id =
      if should_ignore doc_id then Lwt.return_unit
      else
        protect_project_not_found
        @@ fun () ->
        notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
        SState.delayed_update doc_id server_state
        @@ fun state ->
        let* _file, new_state =
          unlocked_process_file ?contents:new_content ~is_saved:false
            ~notify_back doc_id state
        in
        let* () = unlocked_send_all_diagnostics ~notify_back new_state in
        Lwt.return new_state

    method on_notif_doc_did_change
        ~notify_back
        d
        (_c : TextDocumentContentChangeEvent.t list)
        ~old_content:_
        ~new_content =
      self#document_changed ~notify_back ~new_content (Doc_id.of_lsp_uri d.uri)

    method! on_notif_doc_did_save ~notify_back d =
      let doc_id = Doc_id.of_lsp_uri d.textDocument.uri in
      self#process_document ~notify_back ~is_saved:true doc_id None

    method private on_notif_did_change_watched_files ~notify_back changes =
      (* Triggers even on save.. *)
      Lwt_list.iter_p
        (fun { FileEvent.uri; type_ } ->
          let doc_id = Doc_id.of_lsp_uri uri in
          match type_ with
          | Created | Changed -> self#document_changed ~notify_back doc_id
          | Deleted -> self#on_doc_did_close ~notify_back doc_id)
        changes

    method private scan_project ~notify_back { SState.projects; open_documents }
        =
      let open_documents =
        Projects.Projects.fold
          (fun project documents ->
            Doc_id.Map.fold
              (fun doc_id _ documents ->
                if Projects.is_an_included_file doc_id project then
                  (* We do not consider included files *)
                  documents
                else
                  (* Affected files are necessarily present in the project *)
                  let project_file =
                    Option.get @@ Projects.find_file_in_project doc_id project
                  in
                  let document =
                    match Doc_id.Map.find_opt doc_id documents with
                    | None ->
                      SState.make_document ?contents:None ~saved:true doc_id
                        project project_file
                    | Some document -> document
                  in
                  let _processed_file, document =
                    unlocked_process_document document
                  in
                  Doc_id.Map.add doc_id document documents)
              project.project_files documents)
          projects open_documents
      in
      let sstate = { SState.projects; open_documents } in
      let* () = unlocked_send_all_diagnostics ~notify_back sstate in
      Lwt.return sstate

    method! on_notification_unhandled
        ~notify_back
        (n : Lsp.Client_notification.t) =
      match n with
      | SetTrace params ->
        set_log_level (Some params.value);
        Lwt.return_unit
      | UnknownNotification notif ->
        let json = Jsonrpc.Notification.yojson_of_t notif in
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
        SState.use_and_update server_state
        @@ fun sstate ->
        let projects = Projects.init ~notify_back initialize_params in
        let sstate = { sstate with projects } in
        let* sstate =
          if !scan_project_config then self#scan_project ~notify_back sstate
          else Lwt.return sstate
        in
        Lwt.return ((), sstate)
      | _ -> Lwt.return_unit

    method! on_request_unhandled : type r.
        notify_back:_ -> id:_ -> r Lsp.Client_request.t -> r Lwt.t =
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
        | _ -> super#on_request_unhandled ~notify_back ~id r

    method private on_doc_did_close ~notify_back:_ (doc_id : Doc_id.t) =
      if should_ignore doc_id then Lwt.return_unit
      else
        SState.use_and_update server_state
        @@ fun { projects; open_documents } ->
        let open_documents = Doc_id.Map.remove doc_id open_documents in
        Lwt.return ((), { SState.projects; open_documents })

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
        let*? r = retrieve_existing_document_if_ready doc_id server_state in
        match r with
        | None -> Lwt.return_none
        | Some f ->
          let suggestions_opt = State.lookup_suggestions f range in
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
        let*? f = retrieve_existing_document_now doc_id server_state in
        let suggestions_opt = State.lookup_suggestions_by_pos f pos in
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
        let*? f = retrieve_existing_document doc_id server_state in
        match State.lookup_def ~doc_id f pos with
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
        ~(uri : Lsp.Uri.t)
        ~(pos : Position.t)
        ()
        : Locations.t option t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let*? f = retrieve_existing_document doc_id server_state in
        match State.lookup_declaration f pos with
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
        ~(uri : Lsp.Uri.t)
        ~(pos : Position.t)
        ()
        : Location.t list option Lwt.t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let*? f = retrieve_existing_document doc_id server_state in
        match State.lookup_usages ~doc_id f pos with
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
        let*? f = retrieve_existing_document doc_id server_state in
        match State.lookup_type f pos with
        | None -> Lwt.return_none
        | Some (range, md) ->
          Lwt.return_some (Hover.create ~range ~contents:(`MarkupContent md) ())

    method private on_req_type_definition
        ~notify_back:_
        ~(uri : Lsp.Uri.t)
        ~(pos : Position.t)
        ()
        : Locations.t option Lwt.t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        let*? f = retrieve_existing_document doc_id server_state in
        match State.lookup_type_declaration f pos with
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
        let*? f = retrieve_existing_document doc_id server_state in
        let all_symbols = State.lookup_document_symbols f in
        Lwt.return_some (`SymbolInformation all_symbols)

    method private on_req_document_formatting
        ~notify_back
        (params : DocumentFormattingParams.t)
        : TextEdit.t list option Lwt.t =
      let doc_id = Doc_id.of_lsp_uri params.textDocument.uri in
      if should_ignore doc_id then Lwt.return_none
      else
        protect_project_not_found_opt
        @@ fun () ->
        Log.info (fun m ->
            m "trying to format document %a" Doc_id.format doc_id);
        match self#find_doc params.textDocument.uri with
        | None ->
          Log.warn (fun m ->
              m "cannot find document content of document %a" Doc_id.format
                doc_id);
          Lwt.return_none
        | Some { content = doc_content; _ } -> (
          let* (_f : State.file) =
            self#use_or_process_file ~is_saved:false ~notify_back doc_id
          in
          let* r = Utils.try_format_document ~notify_back ~doc_content doc_id in
          match r with
          | None ->
            Log.info (fun m -> m "failed to format document");
            Lwt.return_none
          | Some r ->
            Log.info (fun m -> m "document formatting done");
            Lwt.return_some r)
  end
