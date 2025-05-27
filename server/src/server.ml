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

open Lwt.Syntax
open Catala_utils
open Server_types

let f (_ : Doc_id.t) = assert false

exception ServerError of string

let parse_settings settings =
  match settings with
  | `Assoc [("preludes", `List l)] ->
    List.map
      (fun s ->
        match s with
        | `String f -> f
        | _ ->
          raise
            (ServerError
               (Format.asprintf
                  "Expected a path to a prelude file as a string, instead got \
                   %a"
                  Yojson.Safe.pp s)))
      l
  | _ ->
    raise
      (ServerError
         (Format.asprintf
            "ChangeConfiguration: received unexpected settings: %a"
            Yojson.Safe.pp settings))

let lookup_project ~notify_back doc_id projects =
  match Projects.find_or_populate_project ~notify_back doc_id !projects with
  | file, project, `Unchanged -> file, project
  | file, project, `Changed new_projects ->
    projects := new_projects;
    file, project

let reload_project ~notify_back doc_id projects =
  match Projects.find_or_populate_project ~notify_back doc_id !projects with
  | _file, project, `Unchanged ->
    projects := Projects.reload_project ~notify_back project !projects
  | _file, _project, `Changed new_projects -> projects := new_projects

let mk_prelude _files = []
let current_thread = ref Lwt.return_unit

let run_with_delay ?(delay = 1.) ~when_ready f =
  let run_with_delay () =
    let* () = Lwt_unix.sleep delay in
    f ()
  in
  match Lwt.state !current_thread with
  | Fail _ | Return () ->
    when_ready ();
    current_thread := run_with_delay ()
  | Sleep ->
    Lwt.cancel !current_thread;
    current_thread := run_with_delay ()

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
    List.exists (fun s -> String.length s <= 0 && s.[0] = '.' && s.[0] = '_') p
  in
  Log.debug (fun m ->
      if b then
        m "file %s ignored: either a hidden file or inside an ignored directory"
          file);
  b

class catala_lsp_server =
  let open Linol_lwt in
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    method! config_code_action_provider = `Bool true
    method! config_completion = Some (CompletionOptions.create ())
    method! config_definition = Some (`Bool true)
    method! config_hover = Some (`Bool true)
    method! config_symbol = Some (`Bool true)
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

    val buffers : (Doc_id.t, State.t) Hashtbl.t = Hashtbl.create 32
    val projects : Projects.t ref = ref Projects.empty
    method spawn_query_handler = Lwt.async

    method! on_req_initialize ~notify_back (i : InitializeParams.t) =
      let open Lwt.Syntax in
      set_log_level i.trace;
      Logs.app (fun m -> m "Starting lsp server");
      projects := Projects.init ~notify_back i;
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
      Lwt.return (InitializeResult.create ~capabilities ())

    (* A list of include statements of the prelude files *)
    val mutable prelude = []

    method private process_and_update_file ?contents ~notify_back doc_id =
      let previous_file = Hashtbl.find_opt buffers doc_id in
      let f =
        let project_file, project =
          lookup_project ~notify_back doc_id projects
        in
        State.process_document ?previous_file ?contents project_file project
          doc_id
      in
      Hashtbl.replace buffers doc_id f;
      f

    method private use_or_process_file ?contents ~notify_back uri =
      match Hashtbl.find_opt buffers uri with
      | Some v -> v
      | None -> self#process_and_update_file ?contents ~notify_back uri

    method private on_doc
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (doc_id : Doc_id.t)
        (contents : string option) =
      if should_ignore doc_id then Lwt.return_unit
      else
        protect_project_not_found
        @@ fun () ->
        let file = self#process_and_update_file ?contents ~notify_back doc_id in
        State.all_diagnostics file
        |> function
        | [] -> Lwt.return_unit
        | all_diags ->
          Lwt_list.iter_s
            (fun (doc_id, diags) ->
              notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
              notify_back#send_diagnostic diags)
            all_diags

    method on_notif_doc_did_open ~notify_back d ~content =
      let doc_id = Doc_id.of_lsp_uri d.uri in
      if should_ignore doc_id then Lwt.return_unit
      else
        protect_project_not_found
        @@ fun () -> self#on_doc ~notify_back doc_id (Some content)

    method private document_changed ~notify_back ?new_content doc_id =
      let when_ready () =
        Lwt.async
        @@ fun () ->
        notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
        notify_back#send_diagnostic []
      in
      if should_ignore doc_id then Lwt.return_unit
      else (
        run_with_delay ~when_ready (fun () ->
            notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
            self#on_doc ~notify_back doc_id new_content);
        Lwt.return_unit)

    method on_notif_doc_did_change
        ~notify_back
        d
        (_c : TextDocumentContentChangeEvent.t list)
        ~old_content:_
        ~new_content =
      self#document_changed ~notify_back ~new_content (Doc_id.of_lsp_uri d.uri)

    method! on_notif_doc_did_save ~notify_back d =
      Lwt.cancel !current_thread;
      let doc_id = Doc_id.of_lsp_uri d.textDocument.uri in
      if should_ignore doc_id then Lwt.return_unit
      else (
        (* FIXME: are we sure ? *)
        reload_project ~notify_back doc_id projects;
        self#on_doc ~notify_back doc_id None)

    method private on_notif_did_change_watched_files ~notify_back changes =
      Lwt_list.iter_s
        (fun { FileEvent.uri; type_ } ->
          match type_ with
          | Created | Changed ->
            self#document_changed ~notify_back (Doc_id.of_lsp_uri uri)
          | Deleted ->
            self#on_notif_doc_did_close ~notify_back
              (TextDocumentIdentifier.create ~uri))
        changes

    method! on_notification_unhandled
        ~notify_back
        (n : Lsp.Client_notification.t) =
      match n with
      | Lsp.Client_notification.ChangeConfiguration { settings } -> begin
        try
          prelude <- mk_prelude (parse_settings settings);
          Linol_lwt.Jsonrpc2.IO.return ()
        with ServerError s -> Linol_lwt.Jsonrpc2.IO.failwith s
      end
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

    method on_notif_doc_did_close ~notify_back:_ d =
      let doc_id = Doc_id.of_lsp_uri d.uri in
      Hashtbl.remove buffers doc_id;
      Lwt.return_unit

    method! on_req_code_action
        ~notify_back
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
        protect_project_not_found_opt
        @@ fun () ->
        let f = self#use_or_process_file ~notify_back doc_id in
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
                CodeAction.create ~title:"completion"
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
        ~notify_back
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
        protect_project_not_found_opt
        @@ fun () ->
        let f = self#use_or_process_file ~notify_back doc_id in
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
        ~notify_back
        ~(id : Jsonrpc2.Req_id.t)
        ~(uri : Lsp.Uri.t)
        ~(pos : Position.t)
        ~(workDoneToken : Linol_lwt.ProgressToken.t option)
        ~(partialResultToken : Linol_lwt.ProgressToken.t option)
        doc_state =
      ignore (notify_back, id, workDoneToken, partialResultToken, doc_state);
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        protect_project_not_found_opt
        @@ fun () ->
        let _f = self#use_or_process_file doc_id in
        let all_locs =
          Hashtbl.fold
            (fun _uri f acc ->
              match State.lookup_def ~doc_id f pos with
              | None -> acc
              | Some l ->
                let locs =
                  List.map
                    (fun (file, range) ->
                      let uri = DocumentUri.of_path file in
                      Location.create ~range ~uri)
                    l
                in
                locs @ acc)
            buffers []
        in
        if all_locs = [] then Lwt.return_none
        else Lwt.return_some (`Location all_locs)

    method private on_req_declaration
        ~notify_back
        ~(uri : Lsp.Uri.t)
        ~(pos : Position.t)
        ()
        : Locations.t option t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        protect_project_not_found_opt
        @@ fun () ->
        let f = self#use_or_process_file ~notify_back doc_id in
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
        protect_project_not_found_opt
        @@ fun () ->
        let _f = self#use_or_process_file doc_id in
        let all_locs =
          Hashtbl.fold
            (fun _uri (f : State.file) acc ->
              match State.lookup_usages ~doc_id f pos with
              | None -> acc
              | Some l ->
                let locs =
                  List.map
                    (fun (file, range) ->
                      let uri = DocumentUri.of_path file in
                      Location.create ~range ~uri)
                    l
                in
                locs @ acc)
            buffers []
        in
        if all_locs = [] then Lwt.return_none else Lwt.return_some all_locs

    method! on_req_hover
        ~notify_back
        ~id:_
        ~uri
        ~pos
        ~workDoneToken:_
        _doc_state
        : Hover.t option Lwt.t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        protect_project_not_found_opt
        @@ fun () ->
        let f = self#use_or_process_file ~notify_back doc_id in
        match State.lookup_type f pos with
        | None -> Lwt.return_none
        | Some (range, md) ->
          Lwt.return_some (Hover.create ~range ~contents:(`MarkupContent md) ())

    method private on_req_type_definition
        ~notify_back
        ~(uri : Lsp.Uri.t)
        ~(pos : Position.t)
        ()
        : Locations.t option Lwt.t =
      let doc_id = Doc_id.of_lsp_uri uri in
      if should_ignore doc_id then Lwt.return_none
      else
        protect_project_not_found_opt
        @@ fun () ->
        let f = self#use_or_process_file ~notify_back doc_id in
        match State.lookup_type_declaration f pos with
        | None -> Lwt.return_none
        | Some (file, range) ->
          let uri = DocumentUri.of_path file in
          let loc = Location.create ~range ~uri in
          Lwt.return_some (`Location [loc])

    method! on_req_symbol
        ~notify_back
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
        let f = self#use_or_process_file ~notify_back doc_id in
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
          let (_f : State.file) =
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
