open Diagnostic
open Linol_lwt

module RangeMap = Map.Make (struct
  type t = Range.t

  let compare
      ({ Range.start; end_ } as x)
      ({ Range.start = start'; end_ = end_' } as y) =
    if (start' >= start && end_' <= end_) || (start >= start' && end_ <= end_')
    then 0
    else compare x y
end)

type err_kind =
  | Parsing of Surface.Parser_driver.error
  | Typing of Shared_ast.Typing.typing_error
  | Generic of Catala_utils.Message.generic_error
  | Warning of string

type metadata = { suggestions : string list }

type file = {
  has_errors : bool;
  uri : string;
  errors : (Range.t * err_kind * metadata) RangeMap.t;
}

type t = file

let err_severity = function
  | Parsing _ -> DiagnosticSeverity.Error
  | Typing _ -> DiagnosticSeverity.Error
  | Generic _ -> DiagnosticSeverity.Error
  | Warning _ -> DiagnosticSeverity.Warning

let msg_of_kind = function
  | Parsing (Parsing_error err) -> err.msg
  | Parsing (Lexing_error err) -> err.msg
  | Typing err -> err.msg
  | Generic err -> err.msg
  | Warning s -> s

let pp_range fmt { Range.start; end_ } =
  let open Format in
  let pp_pos fmt { Position.line; character } =
    fprintf fmt "l:%d, c:%d" line character
  in
  fprintf fmt "start:(%a), end:(%a)" pp_pos start pp_pos end_

let create uri = { has_errors = false; uri; errors = RangeMap.empty }

let add_suggestions file range kind suggestions =
  let metadata = { suggestions } in
  let errors = RangeMap.add range (range, kind, metadata) file.errors in
  { file with errors }

let lookup_suggestions file range =
  RangeMap.find_opt range file.errors
  |> function
  | None -> None
  | Some (range, _kind, metadata) ->
    let suggs = metadata.suggestions in
    if suggs <> [] then Some (range, suggs) else None

let lookup_suggestions_by_pos file pos =
  let range = { Range.start = pos; end_ = pos } in
  lookup_suggestions file range

let all_diagnostics file =
  let errs = RangeMap.bindings file.errors in
  List.map
    (fun (range, (_range, kind, _metadata)) ->
      let severity = err_severity kind in
      let message = msg_of_kind kind in
      diag_r severity range message)
    errs

let process_document ?contents (uri : string) : t =
  Log.debug (fun m -> m "Processing %s" uri);
  let uri = Uri.path (Uri.of_string uri) in
  let input_src =
    let open Catala_utils.Global in
    match contents with None -> FileName uri | Some c -> Contents (c, uri)
  in
  let l = ref [] in
  let on_parsing_error e = l := Parsing e :: !l in
  let on_typing_error e = l := Typing e :: !l in
  let on_generic_error e = l := Generic e :: !l in
  let () = Shared_ast.Typing.install_typing_error_catcher on_typing_error in
  let () =
    Catala_utils.Message.install_generic_error_catcher on_generic_error
  in
  let parsing_errs =
    try
      (* Resets the lexing context to a fresh one *)
      Surface.Lexer_common.context := Law;
      let prg =
        Surface.Parser_driver.parse_top_level_file ~on_error:on_parsing_error
          input_src
      in
      let prg = Surface.Fill_positions.fill_pos_with_legislative_info prg in
      let open Catala_utils in
      let ctx =
        Desugared.Name_resolution.form_context (prg, String.Map.empty)
          Uid.Module.Map.empty
      in
      let prg = Desugared.From_surface.translate_program ctx prg in
      let _prg = Desugared.Disambiguate.program prg in
      []
    with e ->
      (match e with
      | Catala_utils.Message.CompilerError er ->
        Log.debug (fun _m -> Catala_utils.Message.Content.emit er Error)
      | _ -> ());
      Log.debug (fun m ->
          m "caught exn: %s - %d diags to send" (Printexc.to_string e)
            (List.length !l));
      List.rev !l
  in
  let file = create uri in
  List.fold_left
    (fun f -> function
      | Parsing (Parsing_error parse_err) as err ->
        let range = range_of_pos parse_err.pos in
        add_suggestions f range err parse_err.suggestions
      | Parsing (Lexing_error lexing_err) as err ->
        let range = unclosed_range_of_pos lexing_err.pos in
        add_suggestions f range err []
      | Typing type_err as err ->
        let range = range_of_pos type_err.pos in
        add_suggestions f range err []
      | Generic { msg = _; pos } as err ->
        let dummy_range () =
          Range.create
            ~start:{ line = -1; character = -1 }
            ~end_:{ line = -1; character = -1 }
        in
        let range =
          match pos with None -> dummy_range () | Some pos -> range_of_pos pos
        in
        add_suggestions f range err []
      | _ -> assert false)
    file parsing_errs
