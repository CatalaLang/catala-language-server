open Catala_utils

module Lib = Test_case_parser_lib_atd

let command_arg =
  let open Cmdliner.Arg in
  let docs = "MAIN COMMANDS" in
  let cmd_generate =
    info ~docs ["generate"] ~doc:
      "Generate the test structure from the given scope in the given program, and print it to stdout in JSON."
  in
  let cmd_read =
    info ~docs ["read"] ~doc:
      "Read the existing tests from the given catala test file, and print them to stdout in JSON."
  in
  let cmd_write =
    info ~docs ["write"] ~doc:
      "Read a test structure in JSON from stdin, and output a corresponding Catala file to stdout."
  in
  required (vflag None [
      Some `Generate, cmd_generate;
      Some `Read, cmd_read;
      Some `Write, cmd_write
    ])

let term =
  let main command include_dirs scope options =
    match command with
    | `Generate -> Lib.generate_test scope include_dirs options
    | `Read -> Lib.read_test include_dirs options
    | `Write -> Lib.write_catala ()
  in
  Cmdliner.Term.(const main $ command_arg $ Cli.Flags.include_dirs $ Cli.Flags.ex_scope )

(* For now, can be invoked through `catala test-case-parser --plugin-dir
   _build/default/ <FILE>` but we need to figure out distribution through vscode
   or otherwise. *)
let () = Driver.Plugin.register "testcase" term
