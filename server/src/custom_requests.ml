(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria, contributor:
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
open Lwt.Syntax
module DQ = Doc_queries
open Server_types
open Server_state
open Catala_utils
open Utils
module Atd = Catala_types_j

let ( let*? ) v f =
  Lwt.bind v @@ function None -> Lwt.return_none | Some x -> f x

let exceptions_at sstate (params : Yojson.Safe.t option) : Yojson.Safe.t Lwt.t =
  let { open_documents; _ } = sstate in
  match params with
  | None -> Lwt.return `Null
  | Some json -> (
    let uri = Yojson.Safe.Util.(json |> member "uri" |> to_string) in
    let position_json = Yojson.Safe.Util.(json |> member "position") in
    let line = Yojson.Safe.Util.(position_json |> member "line" |> to_int) in
    let character =
      Yojson.Safe.Util.(position_json |> member "character" |> to_int)
    in
    let doc_id = Doc_id.of_lsp_uri (Uri0.of_string uri) in
    match Doc_id.Map.find_opt doc_id open_documents with
    | None -> Lwt.return `Null
    | Some doc ->
      let pos = Linol_lwt.Position.create ~line ~character in
      Lwt.return (Option.value ~default:`Null (DQ.exceptions_at doc pos)))

let decode_atd_params f params =
  Option.map (fun json -> f (Yojson.Safe.to_string json)) params

let atd_buf = Buffer.create 1024

let return_json_atd f =
  let buf = atd_buf in
  Buffer.clear buf;
  f buf;
  Lwt.return @@ Yojson.Safe.from_string ~buf (Buffer.contents buf)

let list_entrypoints sstate (params : Yojson.Safe.t option) :
    Yojson.Safe.t Lwt.t =
  let { projects; open_documents; _ } = sstate in
  let open Projects in
  let params =
    decode_atd_params Atd.entrypoints_params_of_string params
    |> Option.value
         ~default:
           {
             Catala_types_t.only = None;
             path = None;
             no_lambdas = None;
             no_variables = None;
           }
  in
  let all_projects = Projects.elements projects in
  let* entrypoint_list =
    Lwt_list.map_s
      (fun project ->
        let get_prog doc_id =
          Doc_id.Map.find_opt doc_id open_documents
          |> function
          | Some { last_valid_result = Some { prg; _ }; _ } ->
            Lwt.return_some prg
          | _ -> (
            let*? project_file =
              Lwt.return (Doc_id.Map.find_opt doc_id project.project_files)
            in
            let resolve_file_content path =
              Global.FileName (File.clean_path path)
            in
            let document =
              Server_state.(make_document Saved doc_id project project_file)
            in
            let validation_result =
              let get_module_content = Server_state.get_module_content sstate in
              Document_processing.process ~get_module_content
                ~resolve_file_content document
            in
            match validation_result with
            | Skipped | Faulty _ | Partial _ -> Lwt.return_none
            | Valid r -> Lwt.return_some r.prg)
        in
        list_entrypoints ~get_prog project params)
      all_projects
  in
  return_json_atd (fun buf ->
      Atd.write_entrypoints buf (List.concat entrypoint_list))

let lang_of_string s =
  List.assoc_opt s Cli.languages
  |> function
  | None -> Format.ksprintf failwith "unsupported '%s' language" s
  | Some x -> x

let read_test (_sstate : server_state) params =
  let { lang; buffer_path; contents } : Atd.read_test_params =
    Option.get @@ decode_atd_params Atd.read_test_params_of_string params
  in
  let options =
    let input_src =
      Global.Contents (contents, Option.value ~default:"" buffer_path)
    in
    Global.enforce_options ~input_src ~language:(Some (lang_of_string lang)) ()
  in
  let writer = Test_case_parser_lib.read_test [] options buffer_path in
  return_json_atd writer

let write_test (_sstate : server_state) params =
  let { lang; tests } : Atd.write_test_params =
    Option.get @@ decode_atd_params Atd.write_test_params_of_string params
  in
  let options =
    Global.enforce_options ~language:(Some (lang_of_string lang)) ()
  in
  let writer = Test_case_parser_lib.write_catala options tests in
  let buf = Buffer.create 1024 in
  writer buf;
  let writer =
    Test_case_parser_lib.(
      writer Atd.write_write_test_output (Buffer.contents buf))
  in
  return_json_atd writer

let wrap_request meth f =
  Lwt.catch f (fun exn ->
      Log.err (fun m ->
          m "request %s request failed: %s" meth (Printexc.to_string exn));
      Lwt.return `Null)

let handle_request sstate meth params =
  let params =
    (* identity *)
    Option.map Linol_jsonrpc.Jsonrpc.Structured.yojson_of_t params
  in
  match meth with
  | "catala.listEntrypoints" ->
    wrap_request meth @@ fun () -> list_entrypoints sstate params
  | "catala.exceptionsAt" ->
    wrap_request meth @@ fun () -> exceptions_at sstate params
  | "catala.readTest" -> wrap_request meth @@ fun () -> read_test sstate params
  | "catala.writeTest" ->
    wrap_request meth @@ fun () -> write_test sstate params
  | _ ->
    (* TODO: Add ad hoc methods for testcase *)
    (* read_test : { "lang": <string>, "buffer-path": <string>, "contents":
       <string> } -> O.test_list result write_tests : { "lang": <string>,
       "tests": <O.test_list> } : unit run: { "scope": <string>, "file":
       <string>, "input"?: <test_inputs>} : kind: 'Ok', value: { test_outputs,
       assert_failures, diffs } | { "kind": 'Error', "value": <err_string> }
       list-scopes : { "file": <string> } : scope_def_list generate : { "file":
       <string>, "scope": <string>, "default_values": <bool>, "force_module":
       <bool> } serialize-inputs : { "test-inputs" : <O.test_inputs>} *)
    Format.kasprintf Lwt.fail_with "Unsupported LSP request received: %s" meth
