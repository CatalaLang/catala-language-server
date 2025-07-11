open Test_case_parser_lib
module Enc = Test_case_parser_lib.O

let error () : ('a, Format.formatter, unit, (_, string) Result.t) format4 -> 'a
    =
  Format.kasprintf Result.error

let ( let* ) = Result.bind

let to_yojson : (Buffer.t -> unit) -> Yojson.Safe.t =
  let buf = Buffer.create 4096 in
  fun f ->
    Buffer.clear buf;
    let () = f buf in
    Yojson.Safe.from_string (Buffer.contents buf)

let wf f x buf = f buf x

let parse_request ~(meth : string) ~(params : Jsonrpc.Structured.t option) :
    (Enc.lsp_request, string) Result.t =
  let* (_meth : Enc.lsp_method) =
    try Ok (J.lsp_method_of_string meth)
    with _ -> error () "No support in Catala LSP for '%s' method" meth
  in
  let* params =
    match params with
    | None ->
      error () "Expected non-empty request parameters for '%s' method" meth
    | Some x -> Ok x
  in
  try
    let json_string =
      Jsonrpc.Structured.yojson_of_t params |> Yojson.Safe.to_string
    in
    let message = J.lsp_request_of_string json_string in
    Ok message
  with exn ->
    let s =
      let b = Buffer.create 150 in
      `Read { buffer_path = "caca/pipi"; payload = "prout" }
      |> J.write_lsp_request b;
      Buffer.contents b
    in
    ignore s;
    error () "Failed to decode parameters for request '%s' :@\n%a@\n%s" meth
      (Yojson.Safe.pretty_print ~std:false)
      (Jsonrpc.Structured.yojson_of_t params)
      (* s *) (Printexc.to_string exn)

let list_scopes file =
  try
    let include_dirs = [] (* TODO *) in
    let options =
      Catala_utils.Global.enforce_options ~input_src:(FileName file) ()
    in
    let scopes = list_scopes include_dirs options in
    Ok (to_yojson (wf J.write_scope_def_list scopes))
  with exn ->
    error () "Failed to list scopes of file %s: %s" file
      (Printexc.to_string exn)

let run_scopes file scope =
  try
    let options =
      Catala_utils.Global.enforce_options ~input_src:(FileName file) ()
    in
    let include_dirs = [] (* TODO *) in
    let test = run_test scope include_dirs options in
    Ok (to_yojson (wf J.write_test test))
  with exn ->
    error () "Failed to run scope %s of file %s: %s" scope file
      (Printexc.to_string exn)

let write_test lang tests =
  try
    let open Catala_utils in
    let lang =
      Option.value ~default:Global.En (List.assoc_opt lang Cli.languages)
    in
    let buf = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer buf in
    let () = write_catala lang tests ppf in
    Ok (`String (Buffer.contents buf))
  with exn -> error () "Failed to write test: %s" (Printexc.to_string exn)

let read_test buffer_path payload =
  try
    let include_dirs = [] (* TODO *) in
    let options =
      Catala_utils.Global.enforce_options
        ~input_src:(Contents (payload, buffer_path))
        ()
    in
    let tests = read_test include_dirs options (Some buffer_path) in
    Ok (to_yojson (wf J.write_test_list tests))
  with exn ->
    error () "Failed to read tests from %s: %s" buffer_path
      (Printexc.to_string exn)

let generate_test file scope =
  try
    let include_dirs = [] (* TODO *) in
    let options =
      Catala_utils.Global.enforce_options ~input_src:(FileName file) ()
    in
    let test = generate_test scope include_dirs options in
    Ok (to_yojson (wf J.write_test test))
  with exn ->
    error () "Failed to generate test from file %s: %s" file
      (Printexc.to_string exn)

let handle_request (msg : Enc.lsp_request) : (Yojson.Safe.t, string) Result.t =
  match msg with
  | `List_scopes { file } -> list_scopes file
  | `Run { file; scope } -> run_scopes file scope
  | `Write { lang; tests } -> write_test lang tests
  | `Read { buffer_path; payload } -> read_test buffer_path payload
  | `Generate { file; scope } -> generate_test file scope
