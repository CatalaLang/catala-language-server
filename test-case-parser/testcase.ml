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

let sig_hash_file =
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"FILE"
        ~doc:
          "Optional JSON file to read scope_def(s) from (a list-scopes output or \
           a single snapshot file); reads stdin if omitted.")

let cmd_sig_hash =
  Cmd.v
    Cmd.(
      info "sig-hash"
        ~doc:
          "Read scope_def(s) as JSON from the given FILE or stdin (a \
           scope_def_list like the output of list-scopes, or a single scope_def \
           like a snapshot file) and print a '<module>.<name>\\t<hash>' line per \
           scope. The hash is the canonical signature projection used for \
           migration drift detection.")
    Term.(const Lib.sig_hash $ with_canonical $ sig_hash_file)

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

let with_json =
  Arg.(
    value & flag & info ["json"] ~doc:"Output machine-readable JSON instead of a report.")

let with_check =
  Arg.(
    value
    & flag
    & info ["check"]
        ~doc:
          "Gate mode: exit non-zero if any test is stale or blocked (drift or \
           corruption). Unpinned (unknown) tests only warn. Intended as a CI \
           freshness gate.")

let migrate_path =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"PATH" ~doc:"Test file or directory to triage.")

let status_plan =
  Arg.(
    value
    & opt (some string) None
    & info ["plan"] ~docv:"FILE"
        ~doc:
          "Instead of drift triage, report resolution progress against the \
           given migration plan (as produced by $(b,migrate plan)). With \
           --check, exit non-zero while any fill/transform is still pending.")

let cmd_migrate_status =
  Cmd.v
    Cmd.(
      info "status"
        ~doc:
          "Triage the tests under the given file or directory by signature \
           drift, bucketing each into fresh / stale / unknown / blocked. With \
           --plan FILE, report progress against a migration plan instead.")
    Term.(
      const Test_migration.migrate_status_cmd
      $ with_check
      $ with_json
      $ status_plan
      $ sig_dir
      $ migrate_path
      $ Cli.Flags.Global.flags)

let cmd_migrate_init =
  Cmd.v
    Cmd.(
      info "init"
        ~doc:
          "Seed a #[testcase.sig] pin (and snapshot) onto unpinned tests under \
           the given file or directory, for tests that still typecheck against \
           the live module. Tests that do not typecheck are left untouched.")
    Term.(
      const Test_migration.migrate_init $ sig_dir $ migrate_path $ Cli.Flags.Global.flags)

let diff_old =
  Arg.(
    value
    & opt (some string) None
    & info ["old"] ~docv:"FILE"
        ~doc:"Old scope_def JSON file (use with --new for an explicit diff).")

let diff_new =
  Arg.(
    value
    & opt (some string) None
    & info ["new"] ~docv:"FILE"
        ~doc:"New scope_def JSON file (use with --old for an explicit diff).")

let diff_path =
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"PATH"
        ~doc:
          "Test file or directory: for each pinned test, diff its committed \
           snapshot against the live signature.")

let cmd_migrate_diff =
  Cmd.v
    Cmd.(
      info "diff"
        ~doc:
          "Show the canonical signature difference. Given a PATH, diffs each \
           pinned test's snapshot against the live signature; or pass --old and \
           --new to compare two scope_def JSON files directly.")
    Term.(
      const Test_migration.migrate_diff
      $ diff_old
      $ diff_new
      $ sig_dir
      $ diff_path
      $ Cli.Flags.Global.flags)

let with_dry_run =
  Arg.(
    value
    & flag
    & info ["dry-run"]
        ~doc:
          "Report the migration plan without writing anything (recover + rewrite \
           + show the per-field steps).")

let apply_path =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"PATH" ~doc:"Test file or directory to migrate.")

let apply_plan =
  Arg.(
    value
    & opt (some string) None
    & info ["plan"] ~docv:"FILE"
        ~doc:
          "Consume a migration plan (from $(b,migrate plan)): apply its \
           confirmed renames and write its filled-in input values, instead of \
           the auto defaults. Slots left as holes stay #[testcase.todo]; tests \
           still needing a transform are left stale.")

let with_rerun =
  Arg.(
    value
    & flag
    & info ["rerun"]
        ~doc:
          "After migrating a test (and only if no holes remain), RUN it and show \
           where the asserted outputs diverge from the freshly-computed ones. \
           Read-only diagnostic: it never rewrites an output (recomputing \
           expectations would make tests tautological). Interpreting needs the \
           project's built runtime.")

let cmd_migrate_apply =
  Cmd.v
    Cmd.(
      info "apply"
        ~doc:
          "Migrate stale tests to the live signature: recover the old values \
           from the committed snapshot, rewrite them old->new, re-pin, and \
           verify the result reads back against the live module. Holes the \
           migration cannot fill (added inputs, non-mechanical changes) are left \
           as `impossible` and reported. With --plan, take the human's \
           decisions from a plan file. Use --dry-run to preview, --rerun for the \
           read-only output diagnostic.")
    Term.(
      const Test_migration.migrate_apply
      $ with_dry_run
      $ with_rerun
      $ apply_plan
      $ sig_dir
      $ apply_path
      $ Cli.Flags.Global.flags)

let plan_out =
  Arg.(
    value
    & opt (some string) None
    & info ["o"; "output"] ~docv:"FILE"
        ~doc:"Write the plan to FILE (default: stdout).")

let cmd_migrate_plan =
  Cmd.v
    Cmd.(
      info "plan"
        ~doc:
          "Emit an editable migration plan (TOML) for the stale tests under \
           PATH: one cluster per drifted scope, listing the automatic changes \
           (review only), the suggested renames (confirm/reject), the new \
           inputs to fill, and the changes needing a Catala transform. The \
           plan is the durable progress ledger — edit it, track it with \
           $(b,migrate status --plan), and (later) apply it.")
    Term.(
      const Test_migration.migrate_plan
      $ plan_out
      $ sig_dir
      $ apply_path
      $ Cli.Flags.Global.flags)

let cmd_migrate =
  Cmd.group
    Cmd.(
      info "migrate"
        ~doc:"Signature-drift migration pipeline for testcases.")
    [
      cmd_migrate_status;
      cmd_migrate_init;
      cmd_migrate_diff;
      cmd_migrate_plan;
      cmd_migrate_apply;
    ]

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
      cmd_migrate;
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
  | _ -> failwith "unexpected signature pin");
  (* Value-less marker on a value: migration left this slot to fill. *)
  (Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["todo"]
     ~contexts:(function
     | Desugared.Name_resolution.Expression _ -> true
     | _ -> false)
  @@ fun ~pos:_ _value -> Some Test_case_parser_lib.Todo)

let () = register ()
