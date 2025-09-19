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

module StdMap = Stdlib.Map
open Catala_utils
open Server_types
open Shared_ast
open Debug_protocol
open Patched_dap
open Lwt.Syntax
module DE = Debug_evaluation
module Message = Catala_utils.Message

let t = Some true
let f = Some false

let protect ?(r = Lwt.return_unit) logger f =
  Lwt.catch f (function
    | Message.CompilerError content ->
      let bt = Printexc.get_raw_backtrace () in
      let msg =
        Format.asprintf "%t@\n%s"
          (fun ppf -> Message.Content.emit ~ppf content Error)
          (Printexc.raw_backtrace_to_string bt)
      in
      let* () = logger msg in
      r
    | Message.CompilerErrors contents ->
      let bt = Printexc.get_raw_backtrace () in
      let msg =
        Format.asprintf "%t@\n%s"
          (fun ppf -> Message.Content.emit_n ~ppf contents Error)
          (Printexc.raw_backtrace_to_string bt)
      in
      let* () = logger msg in
      r
    | Failure msg ->
      let* () = logger msg in
      r
    | Sys_error msg ->
      let* () = logger ("System error: " ^ msg) in
      Lwt.fail_with ("System error: " ^ msg)
    | e ->
      let bt = Printexc.get_raw_backtrace () in
      let* () =
        Format.ksprintf logger "Unexpected error: %s@\n%s"
          (Printexc.to_string e)
          (Printexc.raw_backtrace_to_string bt)
      in
      r)

let on_initialize (_args : Initialize_command.Arguments.t) =
  let configuration : Patched_capabilities.t =
    {
      supports_configuration_done_request = t;
      supports_function_breakpoints = f;
      supports_conditional_breakpoints = f;
      supports_hit_conditional_breakpoints = f;
      supports_evaluate_for_hovers = f;
      exception_breakpoint_filters = None;
      supports_step_back = t;
      supports_set_variable = f;
      supports_restart_frame = f;
      supports_goto_targets_request = f;
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
      supports_loaded_sources_request = t;
      supports_log_points = f;
      supports_terminate_threads_request = f;
      supports_set_expression = f;
      supports_terminate_request = t;
      supports_data_breakpoints = f;
      supports_read_memory_request = f;
      supports_disassemble_request = f;
      supports_cancel_request = f;
      supports_breakpoint_locations_request = t;
      supports_clipboard_context = f (* t ? *);
      supports_stepping_granularity = f;
      supports_instruction_breakpoints = f;
      supports_exception_filter_options = t;
      (* MISSING FIELDS: *)
      support_suspend_debuggee = f;
      supports_write_memory_request = f;
      supports_single_thread_execution_requests = t;
      supports_data_breakpoint_bytes = f;
      breakpoint_modes = None;
      supports_ansi_styling = t;
    }
  in
  Lwt.return configuration

let state = ref None

let use_state f =
  match !state with
  | None ->
    Log.err (fun m -> m "Not debugger running - call launch first");
    assert false
  | Some (r : Debug_evaluation.debugger_state) -> f r

let get_timestamp () =
  let open Ptime in
  let now = Ptime_clock.now () in
  let _, ((hh, ss, mm), _tz_offset_s) = to_date_time ~tz_offset_s:0 now in
  Format.sprintf "[%02d:%02d:%02d] " hh ss mm

type dcalc_expr = DE.dcalc_expr

type env_tree =
  | Node of
      int
      * string
      * dcalc_expr
      * [ `Named of env_tree list | `Indexed of env_tree list ]
  | Leaf of string * dcalc_expr

let ( (current_env : env_tree list ref),
      (current_env_lookup : (int -> env_tree) ref) ) =
  ref [], ref (fun _ -> assert false)

let update_env env =
  let cpt = ref 2 in
  let (module M : StdMap.S with type key = int) = (module StdMap.Make (Int)) in
  let m = ref M.empty in
  let rec children parent_name (e : dcalc_expr) :
      ([ `N | `I ] * (string * dcalc_expr) list) option =
    match Mark.remove e with
    | EArray [] -> None
    | EArray l ->
      Option.some
        ( `I,
          List.mapi
            (fun i e ->
              let name = Format.sprintf "%s#%d" parent_name i in
              name, e)
            l )
    | EStruct { name = _; fields } ->
      if StructField.Map.is_empty fields then None
      else
        Option.some
          ( `N,
            StructField.Map.bindings fields
            |> List.map (fun (sf, e) -> StructField.to_string sf, e) )
    | ETuple [] -> None
    | ETuple l ->
      Option.some
        ( `N,
          List.mapi
            (fun i e ->
              let name = Format.sprintf "%s.%d" parent_name i in
              name, e)
            l )
    | EInj { name = _; e; cons } -> children (EnumConstructor.to_string cons) e
    | _ -> None
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | (_, (EStruct { fields; _ }, _)) :: t when StructField.Map.is_empty fields
      ->
      (* Skip empty structures: they are irrelevant *)
      loop acc t
    | (_, (EAbs _, _)) :: t ->
      (* Skip lambdas *)
      loop acc t
    | (s, e) :: t -> begin
      match children s e with
      | None ->
        let id = !cpt in
        incr cpt;
        let v = Leaf (s, e) in
        m := M.add id v !m;
        loop (v :: acc) t
      | Some (_, []) -> assert false
      | Some (k, children) ->
        let id = !cpt in
        incr cpt;
        let children = loop [] children in
        let children =
          match k with `I -> `Indexed children | `N -> `Named children
        in
        let v = Node (id, s, e, children) in
        m := M.add id v !m;
        loop (v :: acc) t
    end
  in
  let env : (string * dcalc_expr) list =
    Var.Map.bindings env |> List.map (fun (v, (e, _)) -> Bindlib.name_of v, e)
  in
  current_env := loop [] env;
  current_env_lookup := fun i -> M.find i !m

let rec pp_expr lang fmt (e : dcalc_expr) =
  match Mark.remove e with
  | EStruct { name; fields } ->
    Format.fprintf fmt "@[<h>%a@ {@ %a@ }@]" StructName.format name
      (StructField.Map.format_bindings ~pp_sep:Format.pp_print_space
         (fun fmt pp_field_name field_expr ->
           Format.fprintf fmt "@[<hv 2>%t: %a;@]" pp_field_name (pp_expr lang)
             field_expr))
      fields
  | EArray el ->
    let typ = DE.get_typ e in
    Format.fprintf fmt "@[<h>%a (size=%d)@]" Print.typ typ (List.length el)
  | _ -> Print.UserFacing.expr lang fmt e

let env_to_variable lang (t : env_tree) : Patched_variable.t =
  let name, e, variables_reference, named_variables, indexed_variables =
    match t with
    | Leaf (name, e) -> name, e, 0, None, None
    | Node (i, name, e, `Indexed l) -> name, e, i, None, Some (List.length l)
    | Node (i, name, e, `Named l) -> name, e, i, Some (List.length l), None
  in
  let ty = DE.get_typ e in
  {
    name;
    value = Format.asprintf "%a" (pp_expr lang) e;
    type_ = Some (Format.asprintf "%a" Print.typ ty);
    presentation_hint = None;
    evaluate_name = None;
    variables_reference;
    named_variables;
    indexed_variables;
    memory_reference = None;
    declaration_location_reference =
      (* does nothing ? *)
      (if variables_reference = 0 then None else Some variables_reference);
    __vscode_variable_menu_context = None;
  }

let handle_evaluate logger rpc :
    [< `Start | `B of DE.step | `Ok of DE.step | `End | `Exn of DE.step ] ->
    unit Lwt.t =
  let open Stopped_event in
  let open Payload in
  let send_stop ?description reason =
    Debug_rpc.send_event rpc
      (module Stopped_event)
      Stopped_event.Payload.
        {
          reason;
          description;
          thread_id = Some 1;
          preserve_focus_hint = None;
          text = None;
          all_threads_stopped = Some true;
        }
  in
  function
  | `Start ->
    let* () = logger "Start of program reached" in
    send_stop ~description:"*Start of the program reached*" Reason.Entry
  | `Ok { DE.value = Expr expr; _ } ->
    let pos = DE.get_pos expr in
    Log.debug (fun m ->
        m "Step done on: %s - %a"
          (Pos.to_string_shorter pos)
          (Print.expr ()) expr);
    send_stop ~description:"Step" Reason.Step
  | `Exn { DE.value = Expr _; _ } -> assert false
  | `Ok { DE.value = Exn { pos; exn = Runtime (error, pl) }; _ }
  | `Exn { DE.value = Exn { pos; exn = Runtime (error, pl) }; _ } ->
    let* () =
      Format.ksprintf logger "Exception on %s" (Pos.to_string_shorter pos)
    in
    let* () =
      Format.kasprintf logger "@[<v 2>Message:@\n%s@]"
        (Printexc.to_string (Catala_runtime.Error (error, pl)))
    in
    send_stop ~description:"Exception" Reason.Exception
  | `Ok { DE.value = Exn { pos; exn = Internal pp }; _ }
  | `Exn { DE.value = Exn { pos; exn = Internal pp }; _ } ->
    let* () =
      Format.ksprintf logger "Internal error on %s" (Pos.to_string_shorter pos)
    in
    let* () = Format.kasprintf logger "@[<v 2>Message:@\n%t@]" pp in
    send_stop ~description:"Exception" Reason.Exception
  | `B _step -> send_stop ~description:"Breakpoint reached" Reason.Breakpoint
  | `End ->
    let* () = logger "End of program reached - Pausing" in
    (* Cannot send a terminate event otherwise the debugger would exit but with
       a pause, the step/continue buttons are still active which might lead to
       confusion... *)
    send_stop ~description:"*End of the program reached*" Reason.Pause

let doc_id_to_source (doc_id : Doc_id.t) : Source.t =
  let path = Some (doc_id :> string) in
  {
    Source.name = None;
    path;
    source_reference = None;
    presentation_hint = Some Normal;
    origin = None;
    sources = None;
    adapter_data = None;
    checksums = None;
  }

let set_handlers rpc =
  let logger =
    let buf = Buffer.create 256 in
    fun output ->
      let len = String.length output in
      let* () =
        if len > 0 then begin
          let timestamp = get_timestamp () in
          Buffer.add_string buf timestamp;
          String.iter
            (function
              | '\n' ->
                Buffer.add_string buf "\n";
                Buffer.add_string buf timestamp
              | c -> Buffer.add_char buf c)
            output;
          let () =
            if output.[len - 1] <> '\n' then Buffer.add_string buf "\n"
          in
          Debug_rpc.send_event rpc
            (module Output_event)
            (Output_event.Payload.make ~output:(Buffer.contents buf) ())
        end
        else Lwt.return_unit
      in
      Log.debug (fun m -> m "(LOG) %s" (String.trim_end (Buffer.contents buf)));
      Buffer.clear buf;
      Lwt.return_unit
  in
  (* Mandatory requests *)
  Debug_rpc.set_command_handler rpc (module Custom_initialize) on_initialize;
  Debug_rpc.set_command_handler rpc
    (module Disconnect_command)
    (fun _ ->
      (* Nothing to do ? *)
      Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Pause_command)
    (fun _ ->
      (* Not really running but just tell it we stopped.. *)
      Debug_rpc.send_event rpc
        (module Stopped_event)
        {
          reason = Pause;
          description = Some "Pause requested";
          thread_id = Some 1;
          preserve_focus_hint = None;
          text = None;
          all_threads_stopped = Some true;
        });
  Debug_rpc.set_command_handler rpc
    (module Custom_launch)
    (fun { args; stop_on_entry } ->
      protect logger
      @@ fun () ->
      match args with
      | None ->
        Lwt.fail_with
          "Unexpected arguments provided in the launch configuration"
      | Some { uri; scope } ->
        let* final_state = DE.run_debugger rpc ~file:uri ~scope logger in
        state := Some final_state;
        let* () = Debug_rpc.send_event rpc (module Initialized_event) () in
        let* () =
          match stop_on_entry with
          | None | Some false ->
            use_state
            @@ fun state ->
            DE.continue_until_breakpoint state |> handle_evaluate logger rpc
          | Some true ->
            let* () = logger "Waiting for a command to continue..." in
            Debug_rpc.send_event rpc
              (module Stopped_event)
              Stopped_event.Payload.
                {
                  reason = Reason.Pause;
                  description = Some "Pause on start";
                  thread_id = Some 1;
                  preserve_focus_hint = None;
                  text = None;
                  all_threads_stopped = Some true;
                }
        in
        Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Continue_command)
    (fun _ ->
      let* () = logger "Continuing" in
      let* () =
        use_state
        @@ fun s -> DE.continue_until_breakpoint s |> handle_evaluate logger rpc
      in
      Lwt.return { Continue_command.Result.all_threads_continued = t });
  Debug_rpc.set_command_handler rpc
    (module Configuration_done_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Stack_trace_command)
    (fun { thread_id = _; start_frame = _; levels = _; format = _ } ->
      use_state
      @@ fun s ->
      let step = DE.current_step s in
      let pos = DE.get_step_pos step in
      let path, line, column, end_line, end_column =
        if pos == Pos.void then Some (s.file :> string), 1, 1, None, None
        else
          ( Some (Pos.get_file pos),
            Pos.get_start_line pos,
            Pos.get_start_column pos,
            Some (Pos.get_end_line pos),
            Some (Pos.get_end_column pos) )
      in
      let source =
        Some
          {
            Source.name = None;
            path;
            source_reference = None;
            presentation_hint = Some Normal;
            origin = None;
            sources = None;
            adapter_data = None;
            checksums = None;
          }
      in
      let stack_frame : Stack_frame.t =
        {
          id = 1;
          name = "main";
          source;
          line;
          column;
          end_line;
          end_column;
          instruction_pointer_reference = None;
          module_id = None;
          presentation_hint = Some Normal;
        }
      in
      Lwt.return
        {
          Stack_trace_command.Result.stack_frames = [stack_frame];
          total_frames = Some 1;
        });
  Debug_rpc.set_command_handler rpc
    (module Set_breakpoints_command)
    (fun {
           source = { path; _ };
           breakpoints;
           lines = _ (* deprecated apparently *);
           source_modified;
         } ->
      let open Set_breakpoints_command.Result in
      let no_breakpoints = { breakpoints = [] } in
      if path = None then
        let () =
          Log.debug (fun m -> m "Set_breakpoints: no path provided, ignoring")
        in
        Lwt.return no_breakpoints
      else
        let path = Doc_id.of_file (Option.get path) in
        match source_modified with
        | Some true ->
          let* () =
            logger
              "Source modified: cannot set breakpoint - Please reload the \
               debug session"
          in
          Lwt.return no_breakpoints
        | _ ->
          if breakpoints = None then Lwt.return no_breakpoints
          else
            use_state
            @@ fun s ->
            let breakpoints =
              DE.set_breakpoints s path (Option.get breakpoints)
            in
            let* () = logger "Breakpoints set" in
            Lwt.return { breakpoints });
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
    (fun _ ->
      let* () = logger "Stepping back" in
      use_state @@ fun s -> DE.step_back s |> handle_evaluate logger rpc);
  Debug_rpc.set_command_handler rpc
    (module Reverse_continue_command)
    (fun _ ->
      let* () = logger "Reverse continue" in
      use_state
      @@ fun s ->
      DE.continue_back_until_breakpoint s |> handle_evaluate logger rpc);
  Debug_rpc.set_command_handler rpc
    (module Next_command)
    (fun _ ->
      use_state
      @@ fun s ->
      let* () = logger "Step forward" in
      DE.step s |> handle_evaluate logger rpc);
  Debug_rpc.set_command_handler rpc
    (module Goto_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Terminate_command)
    (fun _ ->
      (* Nothing to do ? *)
      state := None;
      Debug_rpc.send_event rpc
        (module Debug_protocol.Terminated_event)
        { restart = None });
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
    (fun { source; line; _ } ->
      let open Breakpoint_locations_command.Result in
      use_state
      @@ fun s ->
      match source.path with
      | None -> Lwt.return { breakpoints = [] }
      | Some file ->
        Lwt.return
          { breakpoints = DE.possible_breakpoints s (Doc_id.of_file file) line });
  Debug_rpc.set_command_handler rpc
    (module Set_exception_breakpoints_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Threads_command)
    (fun () ->
      Lwt.return
        { Threads_command.Result.threads = [Thread.{ id = 1; name = "main" }] });
  Debug_rpc.set_command_handler rpc
    (module Scopes_command)
    (fun { frame_id = _ } ->
      use_state
      @@ fun s ->
      let { DE.env = Env env; _ } = DE.current_step s in
      let variables_reference, named_variables =
        let n = Shared_ast.Var.Map.cardinal env in
        if n = 0 then 0, None else 1 (* toplevel *), Some n
      in
      (* Update env expecting following VariableRequest *)
      update_env env;
      let scope =
        {
          Scope.name = Format.sprintf "Environment: step #%d" s.index;
          presentation_hint = None;
          variables_reference;
          named_variables;
          indexed_variables = None;
          expensive = false;
          source = None;
          line = None;
          column = None;
          end_line = None;
          end_column = None;
        }
      in
      Lwt.return { Scopes_command.Result.scopes = [scope] });
  Debug_rpc.set_command_handler rpc
    (module Patched_variables_command)
    (fun { variables_reference; _ } ->
      use_state
      @@ fun s ->
      let lang = s.program.lang in
      let variables =
        if variables_reference = 1 then !current_env
        else
          match !current_env_lookup variables_reference with
          | Leaf _ as e -> [e]
          | Node (_, _, _, (`Indexed children | `Named children)) -> children
      in
      let variables = List.map (env_to_variable lang) variables in
      Lwt.return { Patched_variables_command.Result.variables });
  Debug_rpc.set_command_handler rpc
    (module Exception_info_command)
    (fun { thread_id = _ } ->
      use_state
      @@ fun s ->
      match DE.current_step s with
      | { value = Expr _; _ } -> assert false
      | { value = Exn { pos = _; exn = Internal pp }; _ } ->
        let description = Format.kasprintf Option.some "%t" pp in
        Lwt.return
          {
            Exception_info_command.Result.exception_id = "Internal";
            description;
            break_mode = Always;
            details = None;
          }
      | { value = Exn { pos = _; exn = Runtime (error, spl) }; _ } ->
        let full_message =
          try
            Message.error
              ~extra_pos:(List.map (fun rp -> "", Expr.runtime_to_pos rp) spl)
              "During evaluation: %a." Format.pp_print_text
              (Catala_runtime.error_message error)
          with Message.CompilerError content ->
            Format.asprintf "%t" (fun ppf ->
                Message.Content.emit ~ppf content Error)
        in
        let* () = logger full_message in
        let exception_id = Catala_runtime.error_to_string error in
        let description = Some full_message in
        let break_mode : Exception_break_mode.t = Always in
        let details : Exception_details.t option =
          Some
            {
              message = None;
              type_name = Some (Catala_runtime.error_to_string error);
              full_type_name = Some (Catala_runtime.error_to_string error);
              evaluate_name = None;
              stack_trace = None;
              inner_exception = None;
            }
        in
        Lwt.return
          {
            Exception_info_command.Result.exception_id;
            description;
            break_mode;
            details;
          });
  Debug_rpc.set_command_handler rpc
    (module Loaded_sources_command)
    (fun () ->
      use_state
      @@ fun s ->
      let doc_ids = Doc_id.Map.keys s.pmap in
      let sources : Source.t list = List.map doc_id_to_source doc_ids in
      Lwt.return { Loaded_sources_command.Result.sources })

let launch rpc =
  set_handlers rpc;
  Lwt.finalize
    (fun () ->
      Lwt.catch
        (fun () -> Debug_rpc.start rpc)
        (fun exn ->
          Log.err (fun m ->
              m "Error at debug client launch: %s" (Printexc.to_string exn));
          Lwt.return_unit))
    (fun () ->
      Log.debug (fun m -> m "Debug connection ended");
      Lwt.return_unit)

let () =
  let _options =
    Catala_utils.Global.enforce_options
    (* FIXME: this still prints warnings on stderr due to [Message] hard-wiring
       [ifprintf] formatters *)
      ~disable_warnings:false ()
  in
  let err_std =
    let pp_header ppf (l, h) =
      match h with
      | None ->
        if l = Logs.App then Format.fprintf ppf "[CDBG] "
        else Format.fprintf ppf "[CDBG|%a] " Logs.pp_level l
      | Some h -> Format.fprintf ppf "[CDBG|%s] " h
    in
    Logs.format_reporter ~pp_header ~app:Format.err_formatter
      ~dst:Format.err_formatter ()
  in
  Logs.set_reporter err_std;
  Logs.set_level (Some Debug);
  ()

type Catala_utils.Pos.attr += Nil

let () =
  (* Dummy registration *)
  Driver.Plugin.register_subcommands "testcase" ~doc:"" ~man:[] [];
  Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["uid"]
    ~contexts:[Desugared.Name_resolution.Expression] (fun ~pos:_ _ -> Some Nil);
  Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["test_description"]
    ~contexts:[Desugared.Name_resolution.ScopeDecl] (fun ~pos:_ _ -> Some Nil)

let main () =
  let rpc = Debug_rpc.create ~in_:Lwt_io.stdin ~out:Lwt_io.stdout () in
  Lwt_main.run (launch rpc)

let () = main ()
