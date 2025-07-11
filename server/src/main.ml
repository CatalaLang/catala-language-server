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

let () =
  let _options =
    Catala_utils.Global.enforce_options
    (* FIXME: this still prints warnings on stderr due to [Message] hard-wiring
       [ifprintf] formatters *)
      ~disable_warnings:false ~message_format:Lsp ()
  in
  let err_std =
    let pp_header ppf (l, h) =
      match h with
      | None ->
        if l = Logs.App then Format.fprintf ppf "[LSP] "
        else Format.fprintf ppf "[LSP|%a] " Logs.pp_level l
      | Some h -> Format.fprintf ppf "[LSP|%s] " h
    in
    Logs.format_reporter ~pp_header ~app:Format.err_formatter
      ~dst:Format.err_formatter ()
  in
  Logs.set_reporter err_std;
  ()

let () = Test_case_parser_lib.register_attributes ()

let run () =
  Log.debug (fun m ->
      m "Command: %a"
        Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
        (Array.to_list Sys.argv));
  let s = new Server.catala_lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in
  let task = Linol_lwt.Jsonrpc2.run (server ~env:()) in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    Log.err (fun m -> m "uncaught exception: %s" (Printexc.to_string e));
    exit 1

let () = run ()
