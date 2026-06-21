(* Unit tests for the pure migration logic: the canonical signature projection
   (and its drift invariants) and the textual test-file parser. These need no
   compiler / clerk, so they live here rather than in the round_trip shell
   harness, which is reserved for compiler-level integration. *)

module L = Test_case_parser_lib
module O = Test_case_parser_lib.O
module M = Test_migration

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
  let ts = M.parse_tests_textually sample in
  check "two test scopes" (List.length ts = 2);
  let t1 = List.nth ts 0 and t2 = List.nth ts 1 in
  check_eq "t1 scope" ~expected:"T1" ~actual:t1.M.pt_test_scope;
  check_eq "t1 target" ~expected:"M.Calc"
    ~actual:(Option.value ~default:"?" t1.M.pt_target);
  check_eq "t1 pin" ~expected:"M.Calc@abc123"
    ~actual:(Option.value ~default:"" t1.M.pt_pin);
  check_eq "t2 scope" ~expected:"T2" ~actual:t2.M.pt_test_scope;
  check "t2 has no pin" (t2.M.pt_pin = None);
  (* unpinned test still recovers its tested scope textually *)
  check_eq "t2 target" ~expected:"M.Other"
    ~actual:(Option.value ~default:"?" t2.M.pt_target)

(* a non-test scope (no testui marker) must be ignored *)
let test_parse_ignores_non_test () =
  let src =
    "```catala-metadata\n\
     declaration scope Helper:\n\
    \  output x content integer\n\
     ```\n"
  in
  check "no test scopes" (M.parse_tests_textually src = [])

let test_helpers () =
  check_eq "first_quoted" ~expected:"hi"
    ~actual:(Option.value ~default:"" (M.first_quoted "x = \"hi\" y"));
  check_eq "qualified_scope_token" ~expected:"M.Calc"
    ~actual:
      (Option.value ~default:"?"
         (M.qualified_scope_token "  output c scope M.Calc"));
  check "qualified rejects single ident"
    (M.qualified_scope_token "output c content integer" = None);
  check_eq "pin_hash" ~expected:"abc123"
    ~actual:(Option.value ~default:"" (M.pin_hash "M.Calc@abc123"))

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
      (M.classify_decision ~pin_hash:pin ~live ~snapshot_present:snap
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
    M.classify_decision ~pin_hash:(Some "h") ~live:(Error "boom")
      ~snapshot_present:false ~snapshot_basename:"x"
  in
  check_eq "blocked reason passthrough" ~expected:"boom"
    ~actual:(Option.value ~default:"" reason);
  (* missing-snapshot reason names the snapshot file *)
  let _, reason2 =
    M.classify_decision ~pin_hash:(Some "h") ~live:(Ok "h2")
      ~snapshot_present:false ~snapshot_basename:"M.S@h.sig.json"
  in
  check_eq "missing-snapshot reason" ~expected:"missing snapshot M.S@h.sig.json"
    ~actual:(Option.value ~default:"" reason2)

(* ---- structured signature diff ------------------------------------------ *)

let cm = L.canonical_model
let sdiff a b = M.sig_diff (cm a) (cm b)
let ppdiff a b = M.pp_sig_diff (cm a) (cm b) (sdiff a b)

(* a scope_def variant helper: map over inputs by name *)
let map_input name f sd =
  {
    sd with
    O.inputs =
      List.map (fun (n, si) -> if String.equal n name then n, f si else n, si)
        sd.O.inputs;
  }

let pair2_struct : O.typ =
  O.TStruct
    { struct_name = "Pair2"; fields = [ "first", O.TInt; "second", O.TMoney ] }

(* same as sd_base but the Pair struct is renamed to Pair2 everywhere *)
let sd_rename =
  { sd_base with outputs = [ "combo", O.TTuple [ choice_enum; O.TOption pair2_struct ] ] }

let sd_add_input =
  { sd_base with inputs = ("extra", { O.typ = O.TBool; is_context = false }) :: sd_base.inputs }

let sd_drop_input =
  { sd_base with inputs = List.filter (fun (n, _) -> n <> "flag") sd_base.inputs }

let sd_retype =
  map_input "maybe_amount" (fun si -> { si with O.typ = O.TOption O.TInt }) sd_base

let sd_ctxflip = map_input "flag" (fun si -> { si with O.is_context = false }) sd_base

let test_sig_diff_invariant () =
  (* THE lock: sig_diff is empty iff the hashes are equal. Cover identity and
     every change kind, including a mixed rename+add. *)
  let sd_mixed = { sd_add_input with outputs = sd_rename.outputs } in
  let pairs =
    [
      "identity", sd_base, sd_base;
      "rename", sd_base, sd_rename;
      "add input", sd_base, sd_add_input;
      "drop input", sd_base, sd_drop_input;
      "retype", sd_base, sd_retype;
      "ctx flip", sd_base, sd_ctxflip;
      "mixed rename+add", sd_base, sd_mixed;
    ]
  in
  List.iter
    (fun (name, a, b) ->
      let empty = sdiff a b = [] in
      let hash_eq = String.equal (h a) (h b) in
      check
        (Printf.sprintf "invariant (%s): diff=[] <=> hash= (diff_empty=%b hash_eq=%b)"
           name empty hash_eq)
        (empty = hash_eq))
    pairs

let test_sig_diff_rename_collapses () =
  (* a struct rename touches a reference site AND the def, yet collapses to one
     advisory rename node — the whole point of the structured diff. *)
  check_eq "rename collapses to one line"
    ~expected:"rename struct M.Pair -> M.Pair2"
    ~actual:(String.concat " | " (ppdiff sd_base sd_rename));
  check "rename is exactly one node" (List.length (sdiff sd_base sd_rename) = 1)

let test_sig_diff_mechanical () =
  check_eq "add input" ~expected:"+ input extra: bool"
    ~actual:(String.concat " | " (ppdiff sd_base sd_add_input));
  check_eq "drop input" ~expected:"- input flag: bool (ctx)"
    ~actual:(String.concat " | " (ppdiff sd_base sd_drop_input));
  check_eq "retype input" ~expected:"~ input maybe_amount: option(money) -> option(int)"
    ~actual:(String.concat " | " (ppdiff sd_base sd_retype));
  check_eq "ctx flip" ~expected:"~ input flag: ctx -> inp"
    ~actual:(String.concat " | " (ppdiff sd_base sd_ctxflip))

(* ---- apply (slice a): structured value rewrite -------------------------- *)

let rvi n : O.runtime_value = { value = O.Integer n; attrs = [] }
let rvm n : O.runtime_value = { value = O.Money n; attrs = [] }
let sdecl name fields : O.struct_declaration = { struct_name = name; fields }
let some_money m : O.runtime_value =
  { value = O.Enum (L.mk_optional_enum_decl `En O.TMoney, ("Present", Some (rvm m))); attrs = [] }
let none_money () : O.runtime_value =
  { value = O.Enum (L.mk_optional_enum_decl `En O.TMoney, ("Absent", None)); attrs = [] }

let mv ?(renames = []) old_typ new_typ v =
  M.migrate_value `En renames ~old_typ ~new_typ ~path:"x" v

let has out ns = List.exists (fun (n : M.step) -> n.M.outcome = out) ns
let has_rename ns =
  List.exists
    (fun (n : M.step) ->
      match n.M.outcome with M.Resolved (M.Renamed _) -> true | _ -> false)
    ns
(* a hard block (needs a decision/transform), distinct from a mere NeedsValue *)
let has_decision ns =
  List.exists
    (fun (n : M.step) ->
      match n.M.outcome with
      | M.NeedsResolving (M.NeedsDecision _) -> true
      | _ -> false)
    ns

let test_apply_struct () =
  (* rename: relabel the struct, values preserved, not blocked *)
  let od = O.TStruct (sdecl "Pair" [ "first", O.TInt; "second", O.TMoney ]) in
  let nd = O.TStruct (sdecl "Cover" [ "first", O.TInt; "second", O.TMoney ]) in
  let value : O.runtime_value =
    { value = O.Struct (sdecl "Pair" [], [ "first", rvi 7; "second", rvm 300 ]); attrs = [] }
  in
  let v, ns = mv ~renames:[ "Pair", "Cover" ] od nd value in
  let expected : O.runtime_value =
    { value = O.Struct (sdecl "Cover" [ "first", O.TInt; "second", O.TMoney ],
                        [ "first", rvi 7; "second", rvm 300 ]); attrs = [] }
  in
  check "rename relabels + preserves values" (v = expected);
  check "rename not blocked" (not (M.needs_attention ns));
  check "rename emits relabel note" (has_rename ns);
  (* add field: synthesized default, flagged, not blocked *)
  let od = O.TStruct (sdecl "Pair" [ "first", O.TInt ]) in
  let nd = O.TStruct (sdecl "Pair" [ "first", O.TInt; "second", O.TMoney ]) in
  let value : O.runtime_value =
    { value = O.Struct (sdecl "Pair" [], [ "first", rvi 7 ]); attrs = [] }
  in
  let v, ns = mv od nd value in
  let expected : O.runtime_value =
    { value = O.Struct (sdecl "Pair" [ "first", O.TInt; "second", O.TMoney ],
                        [ "first", rvi 7; "second", { value = O.Unset; attrs = [ O.Todo ] } ]); attrs = [] }
  in
  check "add field left Unset (not fabricated)" (v = expected);
  check "add field flagged NeedsValue" (has (M.NeedsResolving M.NeedsValue) ns);
  check "add field needs no decision (just a value)" (not (has_decision ns));
  (* drop field: noted *)
  let od = O.TStruct (sdecl "Pair" [ "first", O.TInt; "second", O.TMoney ]) in
  let nd = O.TStruct (sdecl "Pair" [ "first", O.TInt ]) in
  let value : O.runtime_value =
    { value = O.Struct (sdecl "Pair" [], [ "first", rvi 7; "second", rvm 9 ]); attrs = [] }
  in
  let _, ns = mv od nd value in
  check "drop field flagged Dropped" (has (M.Resolved M.Dropped) ns)

let test_apply_option () =
  (* wrap T -> option T *)
  let v, ns = mv O.TMoney (O.TOption O.TMoney) (rvm 100) in
  check "wrap produces Present" (v = some_money 100);
  check "wrap flagged" (has (M.Resolved M.Wrapped) ns && not (M.needs_attention ns));
  (* unwrap a Present *)
  let v, ns = mv (O.TOption O.TMoney) O.TMoney (some_money 100) in
  check "unwrap Present yields payload" (v = rvm 100);
  check "unwrap flagged" (has (M.Resolved M.Unwrapped) ns && not (M.needs_attention ns));
  (* unwrap an Absent: no value to carry over -> a hole to fill, like an added
     field (NeedsValue, not a hard NeedsDecision) *)
  let v, ns = mv (O.TOption O.TMoney) O.TMoney (none_money ()) in
  check "unwrap Absent -> Unset hole (todo)" (v = { value = O.Unset; attrs = [ O.Todo ] });
  check "unwrap Absent needs a value, not a decision"
    (has (M.NeedsResolving M.NeedsValue) ns && not (has_decision ns));
  (* a deliberate `impossible` (Unset) is ⊥, valid at any type, so it is
     preserved verbatim across a type change — NOT wrapped to Present, NOT flagged *)
  let unset : O.runtime_value = { value = O.Unset; attrs = [] } in
  let v, ns = mv O.TMoney (O.TOption O.TMoney) unset in
  check "impossible (Unset) preserved verbatim across wrap, unflagged"
    (v = unset && ns = []);
  (* and NESTED: an impossible struct field survives a struct migration verbatim,
     contributing no worklist item (only the rename note remains) *)
  let od = O.TStruct (sdecl "Pair" [ "first", O.TInt; "second", O.TMoney ]) in
  let nd = O.TStruct (sdecl "Cover" [ "first", O.TInt; "second", O.TMoney ]) in
  let value : O.runtime_value =
    { value = O.Struct (sdecl "Pair" [], [ "first", rvi 7; "second", unset ]); attrs = [] }
  in
  let v, ns = mv ~renames:[ "Pair", "Cover" ] od nd value in
  let expected : O.runtime_value =
    { value = O.Struct (sdecl "Cover" [ "first", O.TInt; "second", O.TMoney ],
                        [ "first", rvi 7; "second", unset ]); attrs = [] }
  in
  check "nested impossible preserved through struct rename" (v = expected);
  check "nested impossible adds no worklist item" (not (M.needs_attention ns))

let test_apply_blocked () =
  (* scalar coerce is never auto *)
  let _, ns = mv O.TInt O.TMoney (rvi 5) in
  check "int->money coerce blocked" (M.needs_attention ns);
  (* a removed enum variant can't be auto-mapped *)
  let ctors = [ "Yes", Some O.TRat; "No", None ] in
  let oe = O.TEnum { enum_name = "Choice"; constructors = ctors; ctor_attrs = [] } in
  let ne = O.TEnum { enum_name = "Choice"; constructors = [ "No", None ]; ctor_attrs = [] } in
  let yesv : O.runtime_value =
    { value = O.Enum ({ enum_name = "Choice"; constructors = ctors; ctor_attrs = [] },
                      ("Yes", Some { value = O.Decimal 1.5; attrs = [] })); attrs = [] }
  in
  let _, ns = mv oe ne yesv in
  check "removed enum variant blocked" (M.needs_attention ns);
  (* a surviving variant is kept, not blocked *)
  let nov : O.runtime_value =
    { value = O.Enum ({ enum_name = "Choice"; constructors = ctors; ctor_attrs = [] }, ("No", None)); attrs = [] }
  in
  let _, ns = mv oe oe nov in
  check "surviving enum variant not blocked" (not (M.needs_attention ns))

let test_apply_record () =
  let nv, ns =
    M.migrate_record `En []
      ~old_fields:[ "a", O.TInt; "b", O.TMoney ]
      ~new_fields:[ "a", O.TInt; "c", O.TBool ]
      ~values:[ "a", rvi 1; "b", rvm 5 ]
      ()
  in
  check "record keeps surviving input" (List.assoc "a" nv = rvi 1);
  check "record adds new input as Unset (todo)" (List.assoc "c" nv = { O.value = O.Unset; attrs = [ O.Todo ] });
  check "record flags add + drop" (has (M.NeedsResolving M.NeedsValue) ns && has (M.Resolved M.Dropped) ns);
  check "record needs no decision (only an added value)" (not (has_decision ns));
  (* signature_renames feeds the rename map apply consumes *)
  check "signature_renames detects the struct rename"
    (M.signature_renames sd_base sd_rename = [ "M.Pair", "M.Pair2" ])

(* the isolation helper: keep only the tested module's import line *)
let test_line_mentions_module () =
  check "plain import"
    (M.line_mentions_module ~module_name:"OptTup" "> Using OptTup");
  check "aliased import"
    (M.line_mentions_module ~module_name:"OptTup" "> Using OptTup as O");
  check "not a substring match"
    (not (M.line_mentions_module ~module_name:"Tax" "> Using TaxHelper"));
  check "unrelated import"
    (not (M.line_mentions_module ~module_name:"Tax" "> Using Helper"))

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
  reg "sig_diff empty iff hash equal" test_sig_diff_invariant;
  reg "sig_diff rename collapses" test_sig_diff_rename_collapses;
  reg "sig_diff mechanical nodes" test_sig_diff_mechanical;
  reg "apply: struct rename/add/drop" test_apply_struct;
  reg "apply: option wrap/unwrap" test_apply_option;
  reg "apply: blocked (coerce, removed variant)" test_apply_blocked;
  reg "apply: record add/drop + rename map" test_apply_record;
  reg "import-line module matching" test_line_mentions_module;
  Tezt.Test.run ()
