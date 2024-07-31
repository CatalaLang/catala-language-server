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
  let combine r1 r2 =
    let report src level ~over k msgf =
      let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
      r2.Logs.report src level ~over (fun () -> v) msgf
    in
    { Logs.report }
  in
  let logfile =
    let oc = open_out "/tmp/log" in
    let fmt = Format.formatter_of_out_channel oc in
    Logs.format_reporter ~app:fmt ~dst:fmt ()
  in
  let err_std =
    Logs.format_reporter ~app:Format.err_formatter ~dst:Format.err_formatter ()
  in
  Logs.set_reporter (combine logfile err_std);
  Logs.set_level (Some Logs.Debug)

let run () =
  Log.app (fun m ->
      m "cmd: %a"
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
