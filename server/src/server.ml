open Lwt.Syntax

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
                   %a@."
                  Yojson.Safe.pp s)))
      l
  | _ ->
    raise
      (ServerError
         (Format.asprintf
            "ChangeConfiguration: received unexpected settings: \n%a@."
            Yojson.Safe.pp settings))

let preprocess_uri uri =
  let prefix = "file://" in
  let prefix_len = String.length prefix in
  let uri_len = String.length uri in
  if prefix_len < uri_len && String.sub uri 0 prefix_len = prefix then
    String.sub uri prefix_len (uri_len - prefix_len)
  else uri

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

class catala_lsp_server =
  let open Linol_lwt in
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    method! config_code_action_provider = `Bool true
    method! config_completion = Some (CompletionOptions.create ())
    method! config_definition = Some (`Bool true)
    method private config_declaration = Some (`Bool true)
    method private config_references = Some (`Bool true)

    method! config_sync_opts =
      (* configure how sync happens *)
      let change = Lsp.Types.TextDocumentSyncKind.Incremental in
      (* Lsp.Types.TextDocumentSyncKind.Full *)
      Lsp.Types.TextDocumentSyncOptions.create ~openClose:true ~change
        ~save:
          (`SaveOptions (Lsp.Types.SaveOptions.create ~includeText:false ()))
        ()

    val buffers : (string, State.t) Hashtbl.t = Hashtbl.create 32
    method spawn_query_handler = Lwt.async

    method! on_req_initialize ~notify_back:_ (_i : InitializeParams.t) =
      let sync_opts = self#config_sync_opts in
      let capabilities =
        ServerCapabilities.create
          ?codeLensProvider:self#config_code_lens_options
          ~codeActionProvider:self#config_code_action_provider
          ~executeCommandProvider:
            (ExecuteCommandOptions.create ~commands:self#config_list_commands ())
          ?completionProvider:self#config_completion
          ?definitionProvider:self#config_definition
          ?declarationProvider:self#config_declaration
          ?referencesProvider:self#config_references
          ?hoverProvider:self#config_hover
          ?inlayHintProvider:self#config_inlay_hints
          ?documentSymbolProvider:self#config_symbol
          ~textDocumentSync:(`TextDocumentSyncOptions sync_opts) ()
        |> self#config_modify_capabilities
      in
      Lwt.return (InitializeResult.create ~capabilities ())

    (* A list of include statements of the prelude files *)
    val mutable prelude = []

    method private process_and_update_file ?contents uri =
      let f = State.process_document ?contents uri in
      Hashtbl.replace buffers uri f;
      f

    method private use_or_process_file ?contents uri =
      match Hashtbl.find_opt buffers uri with
      | Some v -> v
      | None -> self#process_and_update_file ?contents uri

    method private _on_doc
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : DocumentUri.t)
        (contents : string option) =
      let uri_s = DocumentUri.to_string uri in
      let file = self#process_and_update_file ?contents uri_s in
      State.all_diagnostics file
      |> function
      | [] -> Lwt.return_unit | diags -> notify_back#send_diagnostic diags

    method on_notif_doc_did_open ~notify_back d ~content =
      self#_on_doc ~notify_back d.uri (Some content)

    method on_notif_doc_did_change
        ~notify_back
        d
        (_c : TextDocumentContentChangeEvent.t list)
        ~old_content:_
        ~new_content =
      let when_ready () =
        Lwt.async @@ fun () -> notify_back#send_diagnostic []
      in
      run_with_delay ~when_ready (fun () ->
          self#_on_doc ~notify_back d.uri (Some new_content));
      Lwt.return_unit

    method! on_notif_doc_did_save ~notify_back d =
      let { DidSaveTextDocumentParams.textDocument; _ } = d in
      self#_on_doc ~notify_back textDocument.uri None

    method! on_notification_unhandled
        ~notify_back:_
        (n : Lsp.Client_notification.t) =
      Log.debug (fun m -> m "%s" __LOC__);
      match n with
      | Lsp.Client_notification.ChangeConfiguration { settings } -> begin
        try
          prelude <- mk_prelude (parse_settings settings);
          Linol_lwt.Jsonrpc2.IO.return ()
        with ServerError s -> Linol_lwt.Jsonrpc2.IO.failwith s
      end
      | UnknownNotification notif ->
        let json = Jsonrpc.Notification.yojson_of_t notif in
        Format.eprintf "[unkown notification] %a@." Yojson.Safe.pp json;
        Lwt.return ()
      | _ -> Lwt.return ()

    method! on_request_unhandled : type r.
        notify_back:_ -> id:_ -> r Lsp.Client_request.t -> r Lwt.t =
      fun ~notify_back ~id r ->
        Log.debug (fun m -> m "%s" __LOC__);
        Log.debug (fun k -> k "req: unhandled request");
        match r with
        | TextDocumentDeclaration (params : TextDocumentPositionParams.t) ->
          Log.debug (fun m -> m "%s" __LOC__);
          self#on_req_declaration ~notify_back ~uri:params.textDocument.uri
            ~pos:params.position ()
        | TextDocumentReferences (params : ReferenceParams.t) ->
          Log.debug (fun m -> m "%s" __LOC__);
          self#on_req_references ~notify_back ~uri:params.textDocument.uri
            ~pos:params.position ()
        | _ -> super#on_request_unhandled ~notify_back ~id r
    (** Override to process other requests *)

    method on_notif_doc_did_close ~notify_back d =
      Hashtbl.remove buffers (DocumentUri.to_string d.uri);
      notify_back#send_diagnostic []

    method! on_req_code_action
        ~notify_back:_
        ~id:_
        {
          textDocument = doc_id;
          range;
          context = _;
          partialResultToken = _;
          workDoneToken = _;
        }
        : CodeActionResult.t Lwt.t =
      let f = self#use_or_process_file (DocumentUri.to_string doc_id.uri) in
      let suggestions_opt = State.lookup_suggestions f range in
      let actions_opt : CodeAction.t list option =
        Option.map
          (fun (range, suggestions) ->
            let changes : (DocumentUri.t * TextEdit.t list) list option =
              Option.some
              @@ List.map
                   (fun suggestion ->
                     doc_id.uri, [TextEdit.create ~range ~newText:suggestion])
                   suggestions
            in
            [
              CodeAction.create ~title:"completion"
                ~kind:CodeActionKind.QuickFix ~isPreferred:true
                ~edit:
                  { changes; documentChanges = None; changeAnnotations = None }
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
      let f = self#use_or_process_file (DocumentUri.to_string uri) in
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
      let f = self#use_or_process_file (DocumentUri.to_string uri) in
      match State.lookup_def f pos with
      | None -> Lwt.return_none
      | Some (file, range) ->
        let uri = DocumentUri.of_path file in
        let location = Lsp.Types.Location.create ~range ~uri in
        let locs : Linol_lwt.Locations.t = `Location [location] in
        Lwt.return_some locs

    method private on_req_declaration
        ~notify_back
        ~(uri : Lsp.Uri.t)
        ~(pos : Position.t)
        ()
        : Locations.t option t =
      ignore notify_back;
      let f = self#use_or_process_file (DocumentUri.to_string uri) in
      match State.lookup_declaration f pos with
      | None -> Lwt.return_none
      | Some (file, range) ->
        let uri = DocumentUri.of_path file in
        let location = Lsp.Types.Location.create ~range ~uri in
        let locs : Linol_lwt.Locations.t = `Location [location] in
        Lwt.return_some locs

    method private on_req_references
        ~notify_back
        ~(uri : Lsp.Uri.t)
        ~(pos : Position.t)
        ()
        : Location.t list option Lwt.t =
      ignore notify_back;
      let f = self#use_or_process_file (DocumentUri.to_string uri) in
      match State.lookup_usages f pos with
      | None -> Lwt.return_none
      | Some l ->
        let locs =
          List.map
            (fun (file, range) ->
              let uri = DocumentUri.of_path file in
              Lsp.Types.Location.create ~range ~uri)
            l
        in
        Lwt.return_some locs
  end
