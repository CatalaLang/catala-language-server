(* Unit tests for the pure migration logic: the canonical signature projection
   (and its drift invariants) and the textual test-file parser. These need no
   compiler / clerk, so they live here rather than in the round_trip shell
   harness, which is reserved for compiler-level integration. *)

module L = Test_case_parser_lib
module O = Test_case_parser_lib.O

let check name cond = if not cond then failwith ("check failed: " ^ name)

let check_eq name ~expected ~actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s:\n  expected: %S\n  actual:   %S" name expected actual)

(* ---- canonical signature projection ------------------------------------- *)

let pair_struct : O.typ =
  O.TStruct
    { struct_name = "Pair"; fields = [ "first", O.TInt; "second", O.TMoney ] }

let choice_enum : O.typ =
  O.TEnum
    {
      enum_name = "Choice";
      constructors = [ "Yes", Some O.TRat; "No", None ];
      ctor_attrs = [];
    }

(* A scope exercising option / tuple / nested struct+enum in I/O. *)
let sd_base : O.scope_def =
  {
    name = "Calc";
    module_name = "M";
    inputs =
      [
        "maybe_amount", { O.typ = O.TOption O.TMoney; is_context = false };
        "pair", { O.typ = O.TTuple [ O.TInt; O.TMoney ]; is_context = false };
        "flag", { O.typ = O.TBool; is_context = true };
      ];
    outputs = [ "combo", O.TTuple [ choice_enum; O.TOption pair_struct ] ];
    module_deps = [ "Dep" ];
  }

let golden =
  "sig/v1\n\
   scope M.Calc\n\
   in flag ctx bool\n\
   in maybe_amount inp option(money)\n\
   in pair inp tuple(int,money)\n\
   out combo tuple(@M.Choice,option(@M.Pair))\n\
   def enum M.Choice\n\
  \  ctor No -\n\
  \  ctor Yes rat\n\
   def struct M.Pair\n\
  \  field first int\n\
  \  field second money\n"

let test_canonical_golden () =
  check_eq "canonical golden" ~expected:golden
    ~actual:(L.scope_signature_canonical sd_base)

let h sd = L.scope_signature_hash sd

(* Cosmetic changes must NOT move the hash; contract changes MUST. *)
let test_invariants () =
  let base = h sd_base in
  let same name sd =
    check ("same: " ^ name) (String.equal (h sd) base)
  in
  let diff name sd = check ("diff: " ^ name) (not (String.equal (h sd) base)) in
  (* reorder inputs: declaration order is cosmetic *)
  same "reorder inputs" { sd_base with inputs = List.rev sd_base.inputs };
  (* module_deps excluded from the contract *)
  same "module_deps" { sd_base with module_deps = [ "Z"; "A" ] };
  (* is_context flip is a contract distinction *)
  diff "is_context"
    {
      sd_base with
      inputs =
        List.map
          (fun (n, (si : O.scope_input)) ->
            if String.equal n "flag" then n, { si with is_context = false }
            else n, si)
          sd_base.inputs;
    };
  (* tuple element order is positional *)
  diff "tuple order"
    {
      sd_base with
      inputs =
        List.map
          (fun (n, (si : O.scope_input)) ->
            if String.equal n "pair" then
              n, { si with typ = O.TTuple [ O.TMoney; O.TInt ] }
            else n, si)
          sd_base.inputs;
    };
  (* retyping an option payload changes the contract *)
  diff "retype option payload"
    {
      sd_base with
      inputs =
        List.map
          (fun (n, (si : O.scope_input)) ->
            if String.equal n "maybe_amount" then
              n, { si with typ = O.TOption O.TInt }
            else n, si)
          sd_base.inputs;
    };
  (* renaming an input changes the contract *)
  diff "rename input"
    {
      sd_base with
      inputs =
        List.map
          (fun (n, si) -> if String.equal n "flag" then "flag2", si else n, si)
          sd_base.inputs;
    };
  (* adding an output changes the contract (assertions ride on outputs) *)
  diff "add output"
    { sd_base with outputs = ("extra", O.TBool) :: sd_base.outputs }

(* Qualification quirk: a scope's OWN local types are unqualified from
   list-scopes but qualified from a test import. Both must hash identically. *)
let test_qualification () =
  let unqualified = sd_base in
  let qualified =
    {
      sd_base with
      outputs =
        [
          ( "combo",
            O.TTuple
              [
                O.TEnum
                  {
                    enum_name = "M.Choice";
                    constructors = [ "Yes", Some O.TRat; "No", None ];
                    ctor_attrs = [];
                  };
                O.TOption
                  (O.TStruct
                     {
                       struct_name = "M.Pair";
                       fields = [ "first", O.TInt; "second", O.TMoney ];
                     });
              ] );
        ];
    }
  in
  check "local-type qualification is hash-stable"
    (String.equal (h unqualified) (h qualified))

let test_pin () =
  check_eq "pin string" ~expected:"M.Calc@4e2f"
    ~actual:(Printf.sprintf "M.Calc@%s" "4e2f");
  check_eq "scope_signature_pin"
    ~expected:(Printf.sprintf "M.Calc@%s" (h sd_base))
    ~actual:(L.scope_signature_pin sd_base)

(* ---- textual parser ----------------------------------------------------- *)

let sample =
  "> Using M\n\n\
   ```catala-metadata\n\
   #[test]\n\
   #[testcase.testui]\n\
   #[testcase.test_title = \"first\"]\n\
   #[testcase.sig = \"M.Calc@abc123\"]\n\
   declaration scope T1:\n\
  \  output c scope M.Calc\n\
   ```\n\n\
   ```catala-metadata\n\
   #[test]\n\
   #[testcase.testui]\n\
   declaration scope T2:\n\
  \  output c scope M.Other\n\
   ```\n"

let test_parse () =
  let ts = L.parse_tests_textually sample in
  check "two test scopes" (List.length ts = 2);
  let t1 = List.nth ts 0 and t2 = List.nth ts 1 in
  check_eq "t1 scope" ~expected:"T1" ~actual:t1.L.pt_test_scope;
  check_eq "t1 target" ~expected:"M.Calc"
    ~actual:(Option.value ~default:"?" t1.L.pt_target);
  check_eq "t1 pin" ~expected:"M.Calc@abc123"
    ~actual:(Option.value ~default:"" t1.L.pt_pin);
  check_eq "t2 scope" ~expected:"T2" ~actual:t2.L.pt_test_scope;
  check "t2 has no pin" (t2.L.pt_pin = None);
  (* unpinned test still recovers its tested scope textually *)
  check_eq "t2 target" ~expected:"M.Other"
    ~actual:(Option.value ~default:"?" t2.L.pt_target)

(* a non-test scope (no testui marker) must be ignored *)
let test_parse_ignores_non_test () =
  let src =
    "```catala-metadata\n\
     declaration scope Helper:\n\
    \  output x content integer\n\
     ```\n"
  in
  check "no test scopes" (L.parse_tests_textually src = [])

let test_helpers () =
  check_eq "first_quoted" ~expected:"hi"
    ~actual:(Option.value ~default:"" (L.first_quoted "x = \"hi\" y"));
  check_eq "qualified_scope_token" ~expected:"M.Calc"
    ~actual:
      (Option.value ~default:"?"
         (L.qualified_scope_token "  output c scope M.Calc"));
  check "qualified rejects single ident"
    (L.qualified_scope_token "output c content integer" = None);
  check_eq "pin_hash" ~expected:"abc123"
    ~actual:(Option.value ~default:"" (L.pin_hash "M.Calc@abc123"))

(* ---- classification decision table (the corner cases) ------------------- *)

(* Bucketing is pure given (pin hash, live resolution, snapshot present). The
   corner cases we care about reduce to rows of this table:
   - #1 sibling renamed, type still in sig -> live resolves to a *different* hash,
        snapshot present                                                 -> Stale
   - #2 sibling deleted, type still in sig -> tested module can't compile,
        live = Error                                                     -> Blocked
   - #3 refactor removed the type from the sig -> different hash, snapshot present
                                                                         -> Stale
   - #4 junk import, sig unchanged -> live == pin                        -> Fresh
   plus: unpinned -> Unknown; drift with no snapshot -> Blocked. *)
let test_classify () =
  let state ?(snap = false) pin live =
    fst
      (L.classify_decision ~pin_hash:pin ~live ~snapshot_present:snap
         ~snapshot_basename:"M.S@h.sig.json")
  in
  check "unknown (no pin)" (state None (Ok "h") = `Unknown);
  check "fresh (#4 junk import: pin == live)" (state (Some "h") (Ok "h") = `Fresh);
  check "stale (#1/#3 drift, snapshot present)"
    (state ~snap:true (Some "h") (Ok "h2") = `Stale);
  check "blocked (drift, snapshot missing)"
    (state ~snap:false (Some "h") (Ok "h2") = `Blocked);
  check "blocked (#2 live unresolved)"
    (state (Some "h") (Error "live module Tax is missing or does not compile")
    = `Blocked);
  (* the live-resolution reason is passed through verbatim on Blocked *)
  let _, reason =
    L.classify_decision ~pin_hash:(Some "h") ~live:(Error "boom")
      ~snapshot_present:false ~snapshot_basename:"x"
  in
  check_eq "blocked reason passthrough" ~expected:"boom"
    ~actual:(Option.value ~default:"" reason);
  (* missing-snapshot reason names the snapshot file *)
  let _, reason2 =
    L.classify_decision ~pin_hash:(Some "h") ~live:(Ok "h2")
      ~snapshot_present:false ~snapshot_basename:"M.S@h.sig.json"
  in
  check_eq "missing-snapshot reason" ~expected:"missing snapshot M.S@h.sig.json"
    ~actual:(Option.value ~default:"" reason2)

(* the isolation helper: keep only the tested module's import line *)
let test_line_mentions_module () =
  check "plain import"
    (L.line_mentions_module ~module_name:"OptTup" "> Using OptTup");
  check "aliased import"
    (L.line_mentions_module ~module_name:"OptTup" "> Using OptTup as O");
  check "not a substring match"
    (not (L.line_mentions_module ~module_name:"Tax" "> Using TaxHelper"));
  check "unrelated import"
    (not (L.line_mentions_module ~module_name:"Tax" "> Using Helper"))

let () =
  let reg title f =
    Tezt.Test.register ~__FILE__ ~title ~tags:[ "unit"; "migration" ] (fun () ->
        f ();
        Lwt.return_unit)
  in
  reg "canonical projection golden" test_canonical_golden;
  reg "projection drift invariants" test_invariants;
  reg "local-type qualification" test_qualification;
  reg "signature pin formatting" test_pin;
  reg "textual test parser" test_parse;
  reg "parser ignores non-test scopes" test_parse_ignores_non_test;
  reg "textual helpers" test_helpers;
  reg "classification decision table" test_classify;
  reg "import-line module matching" test_line_mentions_module;
  Tezt.Test.run ()
