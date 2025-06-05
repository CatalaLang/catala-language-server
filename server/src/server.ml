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
module SState = Server_state

let ( let*? ) v f =
  Lwt.bind v @@ function None -> Lwt.return_none | Some x -> f x

let f (_ : Doc_id.t) = assert false

exception ServerError of string

let lookup_project ~notify_back doc_id projects =
  match Projects.find_or_populate_project ~notify_back doc_id projects with
  | file, project, `Unchanged -> (file, project), None
  | file, project, `Changed new_projects -> (file, project), Some new_projects

let reload_project ~notify_back doc_id projects =
  match Projects.find_or_populate_project ~notify_back doc_id !projects with
  | _file, project, `Unchanged ->
    projects := Projects.reload_project ~notify_back project !projects
  | _file, _project, `Changed new_projects -> projects := new_projects

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
  @@ fun { documents; _ } ->
  match Doc_id.Map.find_opt doc_id documents with
  | Some { process_result = Some f; _ } -> Lwt.return_some f
  | _ -> Lwt.return_none

let retrieve_existing_document_now doc_id server_state =
  SState.use_now server_state
  @@ fun { documents; _ } ->
  match Doc_id.Map.find_opt doc_id documents with
  | Some { process_result = Some f; _ } -> Lwt.return_some f
  | _ -> Lwt.return_none

let retrieve_existing_document doc_id server_state =
  SState.use server_state
  @@ fun { documents; _ } ->
  match Doc_id.Map.find_opt doc_id documents with
  | Some { process_result = Some f; _ } -> Lwt.return_some f
  | _ -> Lwt.return_none

let send_diagnostics ~notify_back doc_id file =
  (* FIXME: use project scan to send all diagnostics *)
  Log.debug (fun m -> m "sending diagnostics");
  State.all_diagnostics file
  |> function
  | [] ->
    notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
    notify_back#send_diagnostic []
  | all_diags ->
    Lwt_list.iter_s
      (fun (doc_id, diags) ->
        notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
        notify_back#send_diagnostic diags)
      all_diags

class catala_lsp_server =
  let open Linol_lwt in
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    method spawn_query_handler = Lwt.async

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

    method! on_req_initialize ~notify_back (i : InitializeParams.t) =
      set_log_level i.trace;
      Logs.app (fun m -> m "Starting lsp server");
      SState.use_and_update server_state
      @@ fun { projects = _; documents } ->
      let projects = Projects.init ~notify_back i in
      let sync_opts = self#config_sync_opts in
      let* documentFormattingProvider =
        let* available = Utils.check_catala_format_availability () in
        if available then Lwt.return (`Bool true)
        else
          let* () =
            Utils.send_notification ~type_:MessageType.Warning ~notify_back
              "Could not find 'catala-format', code formatting is unavailable.\n\
               Follow the instructions in the \
               [README](https://github.com/CatalaLang/catala-language-server?tab=readme-ov-file#code-formatting) \
               to setup code formatting."
          in
          Lwt.return (`Bool false)
      in
      let capabilities =
        ServerCapabilities.create
          ?codeLensProvider:self#config_code_lens_options
          ~codeActionProvider:self#config_code_action_provider
          ~executeCommandProvider:
            (ExecuteCommandOptions.create ~commands:self#config_list_commands ())
          ~documentFormattingProvider ?completionProvider:self#config_completion
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
      Lwt.return
        ( InitializeResult.create ~capabilities (),
          { SState.projects; documents } )

    method private unlocked_process_file
        ?contents
        ~notify_back
        doc_id
        { SState.projects; documents } =
      Doc_id.Map.find_opt doc_id documents
      |> function
      | Some document ->
        let previous_file = document.process_result in
        let new_file =
          State.process_document ?previous_file ?contents document
        in
        let documents =
          Doc_id.Map.add document.document_id
            { document with process_result = Some new_file }
            documents
        in
        Lwt.return (new_file, { SState.projects; documents })
      | None ->
        let (project_file, project), new_projects_opt =
          lookup_project ~notify_back doc_id projects
        in
        let projects = Option.value ~default:projects new_projects_opt in
        let document = SState.make_empty_document doc_id project project_file in
        let new_file = State.process_document ?contents document in
        let documents =
          Doc_id.Map.add document.document_id
            { document with process_result = Some new_file }
            documents
        in
        let new_state = { SState.projects; documents } in
        (* TODO: rescan project if includes/module uses changed *)
        Lwt.return (new_file, new_state)

    method private process_file ?contents ~notify_back doc_id =
      SState.use_and_update server_state
      @@ fun unlocked_server_state ->
      self#unlocked_process_file ?contents ~notify_back doc_id
        unlocked_server_state

    method private use_or_process_file ~notify_back doc_id =
      let* (doc_opt : State.file SState.document_state option) =
        SState.use server_state
        @@ fun { projects = _; documents } ->
        Lwt.return (Doc_id.Map.find_opt doc_id documents)
      in
      match doc_opt with
      | Some { SState.process_result = None; _ } | None ->
        self#process_file ~notify_back doc_id
      | Some { process_result = Some x; _ } -> Lwt.return x

    method private on_doc
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (doc_id : Doc_id.t)
        (contents : string option) =
      if should_ignore doc_id then Lwt.return_unit
      else
        protect_project_not_found
        @@ fun () ->
        let* file = self#process_file ?contents ~notify_back doc_id in
        send_diagnostics ~notify_back doc_id file

    method on_notif_doc_did_open ~notify_back d ~content =
      let doc_id = Doc_id.of_lsp_uri d.uri in
      self#on_doc ~notify_back doc_id (Some content)

    method private document_changed ~notify_back ?new_content doc_id =
      if should_ignore doc_id then Lwt.return_unit
      else
        protect_project_not_found
        @@ fun () ->
        SState.delayed_update doc_id server_state
        @@ fun state ->
        notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
        let* file, new_state =
          self#unlocked_process_file ?contents:new_content ~notify_back doc_id
            state
        in
        let* () = send_diagnostics ~notify_back doc_id file in
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
      self#on_doc ~notify_back doc_id None

    method private on_notif_did_change_watched_files ~notify_back changes =
      (* Triggers even on save.. *)
      Lwt_list.iter_p
        (fun { FileEvent.uri; type_ } ->
          let doc_id = Doc_id.of_lsp_uri uri in
          match type_ with
          | Created | Changed -> self#document_changed ~notify_back doc_id
          | Deleted -> self#on_doc_did_close ~notify_back doc_id)
        changes

    method! on_notification_unhandled
        ~notify_back
        (n : Lsp.Client_notification.t) =
      match n with
      | SetTrace params ->
        set_log_level (Some params.value);
        Lwt.return_unit
      | UnknownNotification notif ->
        let json = Jsonrpc.Notification.yojson_of_t notif in
        Log.warn (fun m -> m "unkown notification: %a" Yojson.Safe.pp json);
        Lwt.return_unit
      | DidChangeWatchedFiles { changes } ->
        self#on_notif_did_change_watched_files ~notify_back changes
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
        @@ fun { projects; documents } ->
        let documents = Doc_id.Map.remove doc_id documents in
        Lwt.return ((), { SState.projects; documents })

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
            self#use_or_process_file ~notify_back doc_id
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
