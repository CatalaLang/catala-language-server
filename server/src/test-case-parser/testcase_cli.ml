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
module Lib = Testcase_parser

let buffer_path =
  let arg =
    Arg.(
      value
      & opt (some string) None
      & info ["bp"; "buffer-path"] ~docv:"FILEPATH"
          ~env:(Cmd.Env.info "TESTCASE_BUFFER_PATH")
          ~doc:"Enforce a filename when a program is provided through stdin.")
  in
  arg

let with_defaults =
  Arg.(
    value
    & flag
    & info ["default-values"] ~doc:"Generate default values for the program.")

let enforce_module =
  Arg.(
    value
    & flag
    & info ["enforce-module"]
        ~doc:"Ensure that the tested scope is part of a Catala module.")

let cmd_generate =
  let run
      tested_scope
      ?testing_scope
      include_dirs
      options
      with_default_values
      enforce_module =
    Lib.generate_test ~with_default_values tested_scope ?testing_scope
      include_dirs options ~enforce_module
  in
  Cmd.v
    Cmd.(
      info "generate"
        ~doc:
          "Generate the test structure from the given scope in the given \
           program, and print it to stdout in JSON.")
    Term.(
      const (run ?testing_scope:None)
      $ Cli.Flags.ex_scope
      $ Cli.Flags.include_dirs
      $ Cli.Flags.Global.options
      $ with_defaults
      $ enforce_module
      |> map Lib.to_stdout)

let cmd_read =
  Cmd.v
    Cmd.(
      info "read"
        ~doc:
          "Read the existing tests from the given catala test file, and print \
           them to stdout in JSON.")
    Term.(
      const Lib.read_test
      $ Cli.Flags.include_dirs
      $ Cli.Flags.Global.options
      $ buffer_path
      |> map Lib.to_stdout)

let cmd_run =
  Cmd.v
    Cmd.(
      info "run"
        ~doc:
          "Read and runs the specified test from the given catala test file, \
           and prints the actual results as JSON to stdout (in the same format \
           as $(b,read)). Exits with 1 in case the test results differ from \
           what was expected, 10 if the test could not be run.")
    Term.(
      const Lib.run_test_cmd
      $ Cli.Flags.include_dirs
      $ Cli.Flags.Global.options
      $ Cli.Flags.ex_scope
      $ Cli.Flags.scope_input
      |> map Lib.to_stdout)

let cmd_write =
  let run flags output =
    let tests =
      Lib.J.read_test_list (Yojson.init_lexer ()) (Lexing.from_channel stdin)
    in
    let _fname, with_out =
      File.get_main_out_formatter () ~source_file:(Global.Stdin "")
        ~output_file:(Option.map Global.options.Global.path_rewrite output)
    in
    let writer = Lib.write_catala flags tests in
    with_out
    @@ fun ppf ->
    let buf = Buffer.create 1024 in
    writer buf;
    Format.pp_print_string ppf (Buffer.contents buf)
  in
  Cmd.v
    Cmd.(
      info "write"
        ~doc:
          "Read a test structure in JSON from stdin, and output a \
           corresponding Catala file.")
    Term.(const run $ Cli.Flags.Global.flags $ Cli.Flags.output)

let cmd_list_scopes =
  Cmd.v
    Cmd.(
      info "list-scopes"
        ~doc:"List the scopes exposed of a module for a given Catala file.")
    Term.(
      const Lib.list_scopes
      $ Cli.Flags.include_dirs
      $ Cli.Flags.Global.options
      |> map Lib.to_stdout)

let cmd_serialize_inputs =
  let run scope_input_opt =
    let scope_input =
      match scope_input_opt with
      | None -> failwith "serialize-inputs command requires --input argument"
      | Some i -> i
    in
    let inputs =
      Lexing.from_string (Yojson.Safe.to_string scope_input)
      |> Catala_types_atd.Catala_types_j.read_test_inputs (Yojson.init_lexer ())
    in
    Lib.serialize_inputs inputs
  in
  Cmd.v
    Cmd.(
      info "serialize-inputs"
        ~doc:"Returns the normalized JSON of the given inputs.")
    Term.(
      const run
      $ Cli.Flags.scope_input
      |> map (fun json ->
          let open Format in
          fprintf std_formatter "%a@." (Yojson.Safe.pretty_print ~std:true) json))

let man =
  [
    `S Manpage.s_description;
    `P
      "This plugin provides facilities to generate, read, write and run tests \
       for Catala scopes.";
    `P "The test input-output is done through stdin/stdout in JSON format.";
  ]

let info =
  let doc = "Testcase parser" in
  let exits = Cmd.Exit.defaults @ [Cmd.Exit.info ~doc:"on error." 1] in
  Cmd.info "testcase" ~version:Catala_utils.Cli.version ~doc ~exits ~man

let commands =
  [
    cmd_generate;
    cmd_read;
    cmd_run;
    cmd_write;
    cmd_list_scopes;
    cmd_serialize_inputs;
  ]
