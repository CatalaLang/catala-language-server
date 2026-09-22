(* What the plugin will not do is an answer that says why, never a crash. *)

open Harness

let hand_written =
  {|
```catala-metadata
#[test]
declaration scope HandWritten:
  output ok content boolean
```

```catala
scope HandWritten:
  definition ok equals true
```
|}

let table () =
  register ~__FILE__ ~title:"refusals name their reason" ~tags:["refusal"]
  @@ fun () ->
  let dir = project "refusals" in
  let* () = start dir in
  Lwt_list.iter_s
    (fun (cmd, file, reason) ->
      let* r = testcase dir [cmd; file] in
      says reason r;
      unit)
    [
      "partial-read", "test_flags.catala_en", "rule";
      "read", "test_local.catala_en", "names no module";
      "list-scopes", "fn.catala_en", "function type";
      "partial-read", "test_lists.catala_en", "mixing";
    ]

let write_misfits () =
  register ~__FILE__ ~title:"write refuses a value that does not fit"
    ~tags:["refusal"; "write"]
  @@ fun () ->
  let dir = project "refusals" in
  let* () = start dir in
  let* tests = read dir "test_moods.catala_en" in
  let on_first f = List.mapi (fun i t -> if i = 0 then f t else t) tests in
  let purple =
    on_first (fun t ->
        let shade = function
          | O.Enum (d, (_, payload)) -> O.Enum (d, ("Purple", payload))
          | v -> v
        in
        {
          t with
          test_inputs =
            update "shade"
              (fun io ->
                with_raw (Option.fold ~none:O.Unset ~some:shade (raw io)) io)
              t.test_inputs;
        })
  in
  let* r =
    testcase ~stdin:(J.string_of_test_list purple) dir ["write"; "-l"; "en"]
  in
  refuses "do not fit" r;
  (* a blank is not a misfit: the override is dropped *)
  let blank =
    on_first (fun t ->
        {
          t with
          test_outputs =
            update "is_red" (with_raw O.NotOverridden) t.test_outputs;
        })
  in
  let* text = write dir blank in
  if contains text "assertion" then
    Test.fail "a not-overridden output was written as an assertion";
  unit

(* Only a hand edit unqualifies the tested scope; guessing a module would
   rebuild against the wrong one. *)
let module_less () =
  register ~__FILE__ ~title:"a test that names no module"
    ~tags:["refusal"; "partial"]
  @@ fun () ->
  let dir = project "round_trip" in
  write_file
    (dir // "unqual.catala_en")
    ~contents:
      (subst "scope Optionals.Grant" ~by:"scope Grant"
         (read_file (dir // "test_optionals.catala_en")));
  Lwt_list.iter_s
    (fun cmd ->
      let* r = testcase dir [cmd; "unqual.catala_en"] in
      refuses "does not say which module" r;
      unit)
    ["partial-read"; "rebuild"]

(* A file mixing editor-owned and hand-written tests: the one the editor cannot
   represent would be deleted on the next save. *)
let ownership () =
  register ~__FILE__ ~title:"mixed ownership" ~tags:["refusal"; "ownership"]
  @@ fun () ->
  let dir = project "round_trip" in
  let* () = start dir in
  let* () =
    Lwt_list.iter_s
      (fun cmd ->
        let* r = testcase dir [cmd; "mixed.catala_en"] in
        refuses "Hand_written" r;
        unit)
      ["read"; "rebuild"]
  in
  (* ...but a partial read recovers what it honestly can, one test at a time *)
  let* r = testcase dir ["partial-read"; "mixed.catala_en"] in
  says "Hand_written" r;
  Check.((List.length (J.test_list_of_string r.out) = 1) int)
    ~error_msg:"partial read recovered %L tests, wanted %R";
  (* a drifted file never reaches read's check: rebuild makes its own *)
  let dir =
    project ~into:"drifted"
      ~only:["clerk.toml"; "details.catala_en"; "test_details.catala_en"]
      "round_trip"
  in
  edit dir "details.catala_en" (subst "  data stamp content date\n" ~by:"");
  edit dir "test_details.catala_en" (fun s -> s ^ hand_written);
  let* () = start dir in
  let* r = testcase dir ["rebuild"; "test_details.catala_en"] in
  refuses "HandWritten" r;
  unit

(* Kept, one assertion would silently shadow the other. *)
let asserted_twice () =
  register ~__FILE__ ~title:"a field asserted twice" ~tags:["refusal"]
  @@ fun () ->
  let dir = project "round_trip" in
  let* () = start dir in
  let line = "  assertion (calc.total = $1000.00)\n" in
  write_file (dir // "dup.catala_en")
    ~contents:
      (subst line ~by:(line ^ line)
         (read_file (dir // "test_optionals.catala_en")));
  let* r = testcase dir ["read"; "dup.catala_en"] in
  refuses "asserted twice" r;
  let* r = testcase dir ["partial-read"; "dup.catala_en"] in
  says "asserted twice" r;
  Check.((List.length (J.test_list_of_string r.out) = 1) int)
    ~error_msg:"partial read kept %L tests, wanted the %R clean one";
  unit

(* Partial read must not quietly slim a test: skipped silently, promoting the
   working copy would delete it. *)
let rich_assertion () =
  register ~__FILE__ ~title:"an assertion the pane cannot show"
    ~tags:["refusal"; "partial"]
  @@ fun () ->
  let dir = project "round_trip" in
  edit dir "test_details.catala_en"
    (subst "  assertion (calc.total = $12.00)"
       ~by:"  assertion (calc.total >= $12.00)");
  let* r = testcase dir ["partial-read"; "test_details.catala_en"] in
  refuses "Record_unordered" r;
  unit

let bad_literal () =
  register ~__FILE__ ~title:"a literal the reader cannot type is located"
    ~tags:["refusal"; "rebuild"]
  @@ fun () ->
  let dir =
    project
      ~only:["clerk.toml"; "details.catala_en"; "test_details.catala_en"]
      "round_trip"
  in
  edit dir "test_details.catala_en" (subst "|2026-01-01|" ~by:"|2026-02-30|");
  let* () = start dir in
  let* r = testcase dir ["rebuild"; "test_details.catala_en"] in
  says "test_details.catala_en:15" r;
  unit

let register () =
  table ();
  write_misfits ();
  module_less ();
  ownership ();
  asserted_twice ();
  rich_assertion ();
  bad_literal ()
