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
  (* TODO: figure out how to combine two sources.. *)
  ignore Debug_rpc.log_src;
  Logs.set_reporter err_std;
  ()

type Catala_utils.Pos.attr += Nil

let () =
  (* Dummy registration *)
  Driver.Plugin.register_subcommands "testcase" ~doc:"" ~man:[] [];
  Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["uid"]
    ~contexts:[Desugared.Name_resolution.Expression] (fun ~pos:_ _ -> Some Nil);
  Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["test_description"]
    ~contexts:[Desugared.Name_resolution.ScopeDecl] (fun ~pos:_ _ -> Some Nil)

let start_debug_server () =
  let open Lwt.Syntax in
  let port = 8730 in
  let addr = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let on_connection client_addr (in_, out) =
    Log.debug (fun m ->
        m "Debug connection established with %s"
          (match client_addr with
          | Unix.ADDR_UNIX s -> s
          | ADDR_INET (addr, p) ->
            Format.sprintf "%s:%i" (Unix.string_of_inet_addr addr) p));
    Lwt.async (fun () ->
        let rpc = Debug_rpc.create ~in_ ~out () in
        Debug_rpc.set_command_handler rpc
          (module Debug_protocol.Initialize_command)
          (fun _ -> Lwt.return @@ Debug_protocol.Capabilities.make ());
        Lwt.finalize
          (fun () -> Debug_rpc.start rpc)
          (fun () ->
            Log.debug (fun m -> m "Debug connection ended");
            Lwt.return_unit));
    Lwt.return_unit
  in
  Log.debug (fun m -> m "Launching debugging server");
  let _ =
    Lwt.catch
      (fun () ->
        let* _ =
          Lwt_io.establish_server_with_client_address addr on_connection
        in
        Lwt.return_unit)
      (fun e ->
        Log.err (fun m ->
            m "Error at debug server launch: %s" (Printexc.to_string e));
        Lwt.return_unit)
  in
  Log.debug (fun m -> m "Debugging server established");
  ()

let run () =
  Log.debug (fun m ->
      m "Command: %a"
        Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
        (Array.to_list Sys.argv));
  let () = start_debug_server () in
  let s = new Server.catala_lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in
  let task = Linol_lwt.Jsonrpc2.run (server ~env:()) in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    Log.err (fun m -> m "uncaught exception: %s" (Printexc.to_string e));
    exit 1

let () = run ()
