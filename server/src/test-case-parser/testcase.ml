(* This file is part of the Catala project. Copyright (C) 2026 Inria.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Cmdliner
open Test_case_parser_lib
open Testcase_parser
open Testcase_cli

let main_cmd = Cmd.group info commands

let main () =
  register_attributes ();
  let argv = Array.copy Sys.argv in
  (* Our command names (first argument) are case-insensitive *)
  let open Cmdliner in
  let[@inline] exit_with_error excode fcontent =
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit (fcontent ()) Error;
    if Global.options.debug then Printexc.print_raw_backtrace stderr bt;
    exit excode
  in
  let eval_cmd () =
    let r = Cmd.eval ~catch:false ~argv main_cmd in
    Message.report_delayed_errors_if_any ();
    r
  in
  match exit (eval_cmd ()) with
  | Ok _ -> exit Cmd.Exit.ok
  | Error _ -> exit Cmd.Exit.cli_error
  | exception Cli.Exit_with n -> exit n
  | exception Message.CompilerErrors contents ->
    Message.Content.emit_n contents Error;
    exit Cmd.Exit.some_error
  | exception Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    let contents = Message.combine_with_pending_errors content bt in
    Message.Content.emit_n contents Error;
    exit Cmd.Exit.some_error
  | exception Failure msg ->
    exit_with_error Cmd.Exit.some_error
    @@ fun () -> Message.Content.of_string msg
  | exception Sys_error msg ->
    exit_with_error Cmd.Exit.internal_error
    @@ fun () -> Message.Content.of_string ("System error: " ^ msg)
  | exception e ->
    exit_with_error Cmd.Exit.internal_error
    @@ fun () ->
    Message.Content.of_string ("Unexpected error: " ^ Printexc.to_string e)

let () = main ()
