(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
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

open Debug_protocol

let address = Unix.inet_addr_loopback
let port = 8730

let on_initialize (_args : Initialize_command.Arguments.t) =
  let configuration : Capabilities.t =
    let t = Some true in
    let f = Some false in
    {
      supports_configuration_done_request = t;
      supports_function_breakpoints = t;
      supports_conditional_breakpoints = t;
      supports_hit_conditional_breakpoints = t;
      supports_evaluate_for_hovers = f (* ?? *);
      exception_breakpoint_filters = Some [];
      supports_step_back = t;
      supports_set_variable = f;
      supports_restart_frame = f;
      supports_goto_targets_request = t;
      supports_step_in_targets_request = f;
      supports_completions_request = f;
      completion_trigger_characters = None;
      supports_modules_request = f;
      additional_module_columns = None;
      supported_checksum_algorithms = None;
      supports_restart_request = f;
      supports_exception_options = None;
      supports_value_formatting_options = f;
      supports_exception_info_request = f (* t? *);
      support_terminate_debuggee = f;
      supports_delayed_stack_trace_loading = f;
      supports_loaded_sources_request = f;
      supports_log_points = f;
      supports_terminate_threads_request = f;
      supports_set_expression = f;
      supports_terminate_request = t;
      supports_data_breakpoints = t;
      supports_read_memory_request = f;
      supports_disassemble_request = f;
      supports_cancel_request = f;
      supports_breakpoint_locations_request = t;
      supports_clipboard_context = f (* t ? *);
      supports_stepping_granularity = t;
      supports_instruction_breakpoints = f;
      supports_exception_filter_options = f;
    }
  in
  Lwt.return configuration

let set_handlers rpc =
  (* Mandatory requests *)
  Debug_rpc.set_command_handler rpc (module Initialize_command) on_initialize;
  Debug_rpc.set_command_handler rpc
    (module Attach_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Disconnect_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Pause_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Launch_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Configuration_done_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Stack_trace_command)
    (fun _ ->
      Lwt.return
        { Stack_trace_command.Result.stack_frames = []; total_frames = None });
  (* Optional requests *)
  Debug_rpc.set_command_handler rpc
    (module Set_function_breakpoints_command)
    (fun _ ->
      Lwt.return @@ Set_function_breakpoints_command.Result.{ breakpoints = [] });
  Debug_rpc.set_command_handler rpc
    (module Set_data_breakpoints_command)
    (fun _ ->
      Lwt.return @@ Set_data_breakpoints_command.Result.{ breakpoints = [] });
  Debug_rpc.set_command_handler rpc
    (module Step_back_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Goto_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Terminate_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Data_breakpoint_info_command)
    (fun _ ->
      Lwt.return
      @@ {
           Data_breakpoint_info_command.Result.data_id = None;
           description = "";
           access_types = None;
           can_persist = None;
         });
  Debug_rpc.set_command_handler rpc
    (module Breakpoint_locations_command)
    (fun _ ->
      Lwt.return @@ { Breakpoint_locations_command.Result.breakpoints = [] });
  (* For some reason, these requests are sent anyway, ignore them. *)
  Debug_rpc.set_command_handler rpc
    (module Set_exception_breakpoints_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Threads_command)
    (fun _ ->
      Lwt.return
        {
          Threads_command.Result.threads =
            [Thread.{ id = 1; name = "threadcaca" }];
        })

let launch () =
  let open Lwt.Syntax in
  let addr = Unix.ADDR_INET (address, port) in
  let on_connection client_addr (in_, out) =
    Log.debug (fun m ->
        m "Debug connection established with %s"
          (match client_addr with
          | Unix.ADDR_UNIX s -> s
          | ADDR_INET (addr, p) ->
            Format.sprintf "%s:%i" (Unix.string_of_inet_addr addr) p));
    let rpc = Debug_rpc.create ~in_ ~out () in
    Log.debug (fun m -> m "bla");
    set_handlers rpc;
    Lwt.finalize
      (fun () ->
        Lwt.catch
          (fun () ->
            let prpc = Debug_rpc.start rpc in
            let* () = Lwt_unix.sleep 1. in
            let* () = Debug_rpc.send_event rpc (module Initialized_event) () in
            let* () =
              Debug_rpc.send_event rpc
                (module Output_event)
                (Output_event.Payload.make ~output:"prout" ())
            in
            let* () = Lwt_unix.sleep 1. in
            let* () =
              Debug_rpc.send_event rpc
                (module Stopped_event)
                (Stopped_event.Payload.make ~thread_id:(Some 1)
                   ~text:(Some "blabla") ~description:(Some "idk")
                   ~reason:Breakpoint () ~all_threads_stopped:(Some true))
            in
            (* let* () = *)
            (*   Debug_rpc.send_event rpc *)
            (*     (module Thread_event) *)
            (*     (Thread_event.Payload.make ~reason:Exited ~thread_id:1) *)
            (* in *)
            (* let* () = *)
            (*   Debug_rpc.send_event rpc *)
            (*     (module Exited_event) *)
            (*     { Exited_event.Payload.exit_code = 0 } *)
            (* in *)
            prpc)
          (fun exn ->
            Log.err (fun m ->
                m "Error at debug client launch: %s" (Printexc.to_string exn));
            Lwt.return_unit))
      (fun () ->
        Log.debug (fun m -> m "Debug connection ended");
        Lwt.return_unit)
  in
  Log.debug (fun m -> m "Launching debugging server");
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let* _s =
            Lwt_io.establish_server_with_client_address addr on_connection
          in
          Lwt.return_unit)
        (fun e ->
          Log.err (fun m ->
              m "Error at debug server launch: %s" (Printexc.to_string e));
          Lwt.return_unit));
  Log.debug (fun m -> m "Debugging server listening");
  ()
