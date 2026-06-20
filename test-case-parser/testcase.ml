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
      $ Cli.Flags.scope_input)

let sig_dir =
  Arg.(
    value
    & opt (some string) None
    & info ["sig-dir"] ~docv:"DIR"
        ~doc:
          "If set, write a committed signature snapshot (content-addressed \
           scope_def JSON) for each test's tested scope into this directory.")

let cmd_write =
  Cmd.v
    Cmd.(
      info "write"
        ~doc:
          "Read a test structure in JSON from stdin, and output a \
           corresponding Catala file.")
    Term.(
      const Lib.write_catala
      $ sig_dir
      $ Cli.Flags.Global.flags
      $ Cli.Flags.output)

let with_canonical =
  Arg.(
    value
    & flag
    & info ["show-canonical"]
        ~doc:"Also print the canonical projection text used for the hash.")

let cmd_sig_hash =
  Cmd.v
    Cmd.(
      info "sig-hash"
        ~doc:
          "Read a scope_def_list as JSON from stdin (e.g. the output of \
           list-scopes) and print a '<module>.<name>\\t<hash>' line per scope. \
           The hash is the canonical signature projection used for migration \
           drift detection.")
    Term.(const Lib.sig_hash $ with_canonical)

let out_dir =
  Arg.(
    required
    & opt (some string) None
    & info ["output-dir"] ~docv:"DIR"
        ~doc:"Directory to write synthesized stub modules into.")

let cmd_stub =
  Cmd.v
    Cmd.(
      info "stub"
        ~doc:
          "Read a scope_def_list as JSON from stdin and write synthesized stub \
           Catala modules for the first scope into --output-dir (migration \
           value-recovery helper).")
    Term.(const Lib.stub_cmd $ out_dir $ Cli.Flags.Global.flags)

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
      cmd_run;
      cmd_write;
      cmd_list_scopes;
      cmd_serialize_inputs;
      cmd_sig_hash;
      cmd_stub;
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
  (Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["array_item_label"]
     ~contexts:(function
     | Desugared.Name_resolution.Expression _ -> true
     | _ -> false)
  @@ fun ~pos:_ value ->
  match value with
  | Shared_ast.String (s, _pos) -> Some (Test_case_parser_lib.ArrayItemLabel s)
  | _ -> failwith "unexpected array item label");
  (Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["sig"]
     ~contexts:(function
     | Desugared.Name_resolution.ScopeDecl -> true
     | _ -> false)
  @@ fun ~pos:_ value ->
  match value with
  | Shared_ast.String (s, _pos) -> Some (Test_case_parser_lib.SigPin s)
  | _ -> failwith "unexpected signature pin")

let () = register ()
