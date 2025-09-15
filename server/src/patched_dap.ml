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

module Patched_capabilities = struct
  type breakpoint_mode = {
    mode : string;
    label : string;
    description : string option; [@default None]
    appliesTo : string list; [@default []]
  }
  [@@deriving make, yojson { strict = false }]

  type t = {
    supports_configuration_done_request : bool option;
        [@key "supportsConfigurationDoneRequest"] [@default None]
        (** The debug adapter supports the 'configurationDone' request. *)
    supports_function_breakpoints : bool option;
        [@key "supportsFunctionBreakpoints"] [@default None]
        (** The debug adapter supports function breakpoints. *)
    supports_conditional_breakpoints : bool option;
        [@key "supportsConditionalBreakpoints"] [@default None]
        (** The debug adapter supports conditional breakpoints. *)
    supports_hit_conditional_breakpoints : bool option;
        [@key "supportsHitConditionalBreakpoints"] [@default None]
        (** The debug adapter supports breakpoints that break execution after a
            specified number of hits. *)
    supports_evaluate_for_hovers : bool option;
        [@key "supportsEvaluateForHovers"] [@default None]
        (** The debug adapter supports a (side effect free) evaluate request for
            data hovers. *)
    exception_breakpoint_filters : Exception_breakpoints_filter.t list option;
        [@key "exceptionBreakpointFilters"] [@default None]
        (** Available exception filter options for the 'setExceptionBreakpoints'
            request. *)
    supports_step_back : bool option; [@key "supportsStepBack"] [@default None]
        (** The debug adapter supports stepping back via the 'stepBack' and
            'reverseContinue' requests. *)
    supports_set_variable : bool option;
        [@key "supportsSetVariable"] [@default None]
        (** The debug adapter supports setting a variable to a value. *)
    supports_restart_frame : bool option;
        [@key "supportsRestartFrame"] [@default None]
        (** The debug adapter supports restarting a frame. *)
    supports_goto_targets_request : bool option;
        [@key "supportsGotoTargetsRequest"] [@default None]
        (** The debug adapter supports the 'gotoTargets' request. *)
    supports_step_in_targets_request : bool option;
        [@key "supportsStepInTargetsRequest"] [@default None]
        (** The debug adapter supports the 'stepInTargets' request. *)
    supports_completions_request : bool option;
        [@key "supportsCompletionsRequest"] [@default None]
        (** The debug adapter supports the 'completions' request. *)
    completion_trigger_characters : string list option;
        [@key "completionTriggerCharacters"] [@default None]
        (** The set of characters that should trigger completion in a REPL. If
            not specified, the UI should assume the '.' character. *)
    supports_modules_request : bool option;
        [@key "supportsModulesRequest"] [@default None]
        (** The debug adapter supports the 'modules' request. *)
    additional_module_columns : Column_descriptor.t list option;
        [@key "additionalModuleColumns"] [@default None]
        (** The set of additional module information exposed by the debug
            adapter. *)
    supported_checksum_algorithms : Checksum_algorithm.t list option;
        [@key "supportedChecksumAlgorithms"] [@default None]
        (** Checksum algorithms supported by the debug adapter. *)
    supports_restart_request : bool option;
        [@key "supportsRestartRequest"] [@default None]
        (** The debug adapter supports the 'restart' request. In this case a
            client should not implement 'restart' by terminating and relaunching
            the adapter but by calling the RestartRequest. *)
    supports_exception_options : bool option;
        [@key "supportsExceptionOptions"] [@default None]
        (** The debug adapter supports 'exceptionOptions' on the
            setExceptionBreakpoints request. *)
    supports_value_formatting_options : bool option;
        [@key "supportsValueFormattingOptions"] [@default None]
        (** The debug adapter supports a 'format' attribute on the
            stackTraceRequest, variablesRequest, and evaluateRequest. *)
    supports_exception_info_request : bool option;
        [@key "supportsExceptionInfoRequest"] [@default None]
        (** The debug adapter supports the 'exceptionInfo' request. *)
    support_terminate_debuggee : bool option;
        [@key "supportTerminateDebuggee"] [@default None]
        (** The debug adapter supports the 'terminateDebuggee' attribute on the
            'disconnect' request. *)
    supports_delayed_stack_trace_loading : bool option;
        [@key "supportsDelayedStackTraceLoading"] [@default None]
        (** The debug adapter supports the delayed loading of parts of the
            stack, which requires that both the 'startFrame' and 'levels'
            arguments and an optional 'totalFrames' result of the 'StackTrace'
            request are supported. *)
    supports_loaded_sources_request : bool option;
        [@key "supportsLoadedSourcesRequest"] [@default None]
        (** The debug adapter supports the 'loadedSources' request. *)
    supports_log_points : bool option;
        [@key "supportsLogPoints"] [@default None]
        (** The debug adapter supports logpoints by interpreting the
            'logMessage' attribute of the SourceBreakpoint. *)
    supports_terminate_threads_request : bool option;
        [@key "supportsTerminateThreadsRequest"] [@default None]
        (** The debug adapter supports the 'terminateThreads' request. *)
    supports_set_expression : bool option;
        [@key "supportsSetExpression"] [@default None]
        (** The debug adapter supports the 'setExpression' request. *)
    supports_terminate_request : bool option;
        [@key "supportsTerminateRequest"] [@default None]
        (** The debug adapter supports the 'terminate' request. *)
    supports_data_breakpoints : bool option;
        [@key "supportsDataBreakpoints"] [@default None]
        (** The debug adapter supports data breakpoints. *)
    supports_read_memory_request : bool option;
        [@key "supportsReadMemoryRequest"] [@default None]
        (** The debug adapter supports the 'readMemory' request. *)
    supports_disassemble_request : bool option;
        [@key "supportsDisassembleRequest"] [@default None]
        (** The debug adapter supports the 'disassemble' request. *)
    supports_cancel_request : bool option;
        [@key "supportsCancelRequest"] [@default None]
        (** The debug adapter supports the 'cancel' request. *)
    supports_breakpoint_locations_request : bool option;
        [@key "supportsBreakpointLocationsRequest"] [@default None]
        (** The debug adapter supports the 'breakpointLocations' request. *)
    supports_clipboard_context : bool option;
        [@key "supportsClipboardContext"] [@default None]
        (** The debug adapter supports the 'clipboard' context value in the
            'evaluate' request. *)
    supports_stepping_granularity : bool option;
        [@key "supportsSteppingGranularity"] [@default None]
        (** The debug adapter supports stepping granularities (argument
            'granularity') for the stepping requests. *)
    supports_instruction_breakpoints : bool option;
        [@key "supportsInstructionBreakpoints"] [@default None]
        (** The debug adapter supports adding breakpoints based on instruction
            references. *)
    supports_exception_filter_options : bool option;
        [@key "supportsExceptionFilterOptions"] [@default None]
        (** The debug adapter supports 'filterOptions' as an argument on the
            'setExceptionBreakpoints' request. *)
    (* MISSING FIELDS: *)
    support_suspend_debuggee : bool option;
        [@key "supportSuspendDebuggee"] [@default None]
    supports_write_memory_request : bool option;
        [@key "supportsWriteMemoryRequest"]
    supports_single_thread_execution_requests : bool option;
        [@key "supportsSingleThreadExecutionRequests"] [@default None]
    supports_data_breakpoint_bytes : bool option;
        [@key "supportsDataBreakpointBytes"] [@default None]
    breakpoint_modes : breakpoint_mode list option;
        [@key "breakpointModes"] [@default None]
    supports_ansi_styling : bool option;
        [@key "supportsANSIStyling"] [@default None]
  }
  [@@deriving make, yojson { strict = false }]
  (** Information about the capabilities of a debug adapter. *)
end

module Custom_initialize = struct
  include Initialize_command

  module Result = struct
    type t = Patched_capabilities.t [@@deriving yojson]
  end
end

module Custom_attach = struct
  let type_ = "attach"

  type payload = { uri : string; scope : string }
  [@@deriving yojson { strict = true }]

  module Arguments = struct
    type t = { args : payload option [@default None] }
    [@@deriving yojson { strict = false }]
  end

  module Result = struct
    type t = Empty_dict.t [@@deriving yojson]
  end
end

module Custom_launch = struct
  let type_ = "launch"

  type payload = { uri : string; scope : string }
  [@@deriving yojson { strict = true }]

  module Arguments = struct
    type t = {
      stop_on_entry : bool option; [@key "stopOnEntry"] [@default None]
      args : payload option; [@default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Result = struct
    type t = Empty_dict.t [@@deriving yojson]
  end
end

module Patched_variable = struct
  type t = {
    name : string;
    value : string;
    type_ : string option; [@key "type"] [@default None]
    presentation_hint : Variable_presentation_hint.t option;
        [@key "presentationHint"] [@default None]
    evaluate_name : string option; [@key "evaluateName"] [@default None]
    variables_reference : int; [@key "variablesReference"]
    named_variables : int option; [@key "namedVariables"] [@default None]
    indexed_variables : int option; [@key "indexedVariables"] [@default None]
    memory_reference : string option; [@key "memoryReference"] [@default None]
    declaration_location_reference : int option;
        [@key "declarationLocationReference"] [@default None]
    __vscode_variable_menu_context : string option;
        [@key "__vscodeVariableMenuContext"] [@default None]
  }
  [@@deriving make, yojson { strict = false }]
end

module Patched_variables_command = struct
  include Variables_command

  module Result = struct
    type t = {
      variables : Patched_variable.t list;
          (** All (or a range) of variables for the given variable reference. *)
    }
    [@@deriving make, yojson { strict = false }]
  end
end
