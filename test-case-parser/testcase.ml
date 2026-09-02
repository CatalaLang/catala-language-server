open Catala_utils
open Cmdliner
module Lib = Test_case_parser_lib

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

let retarget =
  Arg.(
    value
    & opt (some string) None
    & info ["s"; "scope"] ~docv:"SCOPE"
        ~doc:
          "Rebuild against this scope rather than the one the test declares: \
           $(b,SCOPE) in the tested module, or $(b,MODULE.SCOPE) anywhere in \
           the project. For a test whose scope, or module, was renamed.")

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
  Cmd.v
    Cmd.(
      info "generate"
        ~doc:
          "Generate the test structure from the given scope in the given \
           program, and print it to stdout in JSON.")
    Term.(
      const (Lib.generate_cmd ?testing_scope:None)
      $ Cli.Flags.ex_scope
      $ Cli.Flags.include_dirs
      $ Cli.Flags.Global.options
      $ with_defaults
      $ enforce_module)

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
      $ buffer_path)

let cmd_rebuild =
  Cmd.v
    Cmd.(
      info "rebuild"
        ~doc:
          "For a test that no longer fits the scope it targets: print what was \
           authored, the same tests against the live signature with whatever \
           could be carried across, and what could not be done — as JSON. This \
           is what the editor's recovery view renders.")
    Term.(const Lib.rebuild_broken_test $ Cli.Flags.Global.options $ retarget)

let cmd_partial_read =
  Cmd.v
    Cmd.(
      info "partial-read"
        ~doc:
          "Read the tests of a file from its surface syntax alone, without \
           typechecking: types are inferred from each literal, so the result \
           is what the file itself proves and no more. This is the reader \
           that still works when the scope under test has changed underneath \
           the file, and it must agree with $(b,read) wherever both succeed.")
    Term.(const Lib.read_partial_test $ Cli.Flags.Global.options)

let cmd_run =
  Cmd.v
    Cmd.(
      info "run"
        ~doc:
          "Read and runs the specified test from the given catala test file, \
           and prints the actual results as JSON to stdout (in the same format \
           as $(b,read)). The exit code says whether the test could be RUN, \
           not whether it passed: it is 0 even when assertions fail -- the \
           result's $(b,assert_failures) and $(b,diffs) say so -- and non-zero \
           only when the test could not be run at all.")
    Term.(
      const Lib.run_test_cmd
      $ Cli.Flags.include_dirs
      $ Cli.Flags.Global.options
      $ Cli.Flags.ex_scope
      $ Cli.Flags.scope_input
      $ buffer_path)

let cmd_write =
  Cmd.v
    Cmd.(
      info "write"
        ~doc:
          "Read a test structure in JSON from stdin, and output a \
           corresponding Catala file.")
    Term.(const Lib.write_catala $ Cli.Flags.Global.flags $ Cli.Flags.output)

let cmd_list_scopes =
  Cmd.v
    Cmd.(
      info "list-scopes"
        ~doc:"List the scopes exposed of a module for a given Catala file.")
    Term.(
      const Lib.list_scopes $ Cli.Flags.include_dirs $ Cli.Flags.Global.options)

let cmd_serialize_inputs =
  Cmd.v
    Cmd.(
      info "serialize-inputs"
        ~doc:"Returns the normalized JSON of the given inputs.")
    Term.(const Lib.serialize_inputs $ Cli.Flags.scope_input)

let man =
  [
    `S Manpage.s_description;
    `P
      "This plugin provides facilities to generate, read, write and run tests \
       for Catala scopes.";
    `P "The test input-output is done through stdin/stdout in JSON format.";
  ]

let register () =
  Driver.Plugin.register_subcommands "testcase"
    ~doc:"Catala plugin for the handling of scope test cases" ~man
    [
      cmd_generate;
      cmd_read;
      cmd_partial_read;
      cmd_rebuild;
      cmd_run;
      cmd_write;
      cmd_list_scopes;
      cmd_serialize_inputs;
    ];
  (Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["uid"]
     ~contexts:(function
     | Desugared.Name_resolution.Expression _ -> true
     | _ -> false)
  @@ fun ~pos:_ value ->
  match value with
  | Shared_ast.String (s, _pos) -> Some (Test_case_parser_lib.Uid s)
  | _ -> failwith "unexpected UID value");
  (Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["testui"]
     ~contexts:(function
     | Desugared.Name_resolution.ScopeDecl -> true
     | _ -> false)
  @@ fun ~pos:_ value ->
  match value with _ -> Some Test_case_parser_lib.TestUi);
  (Driver.Plugin.register_attribute ~plugin:"testcase"
     ~path:["test_description"] ~contexts:(function
     | Desugared.Name_resolution.ScopeDecl -> true
     | _ -> false)
  @@ fun ~pos:_ value ->
  match value with
  | Shared_ast.String (s, _pos) -> Some (Test_case_parser_lib.TestDescription s)
  | _ -> failwith "unexpected test description");

  (Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["test_title"]
     ~contexts:(function
     | Desugared.Name_resolution.ScopeDecl -> true
     | _ -> false)
  @@ fun ~pos:_ value ->
  match value with
  | Shared_ast.String (s, _pos) -> Some (Test_case_parser_lib.TestTitle s)
  | _ -> failwith "unexpected test title");
  Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["array_item_label"]
    ~contexts:(function
    | Desugared.Name_resolution.Expression _ -> true
    | _ -> false)
  @@ fun ~pos:_ value ->
  match value with
  | Shared_ast.String (s, _pos) -> Some (Test_case_parser_lib.ArrayItemLabel s)
  | _ -> failwith "unexpected array item label"

let () = register ()
