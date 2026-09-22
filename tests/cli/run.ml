(* Running a test, and what the run reports. *)

open Harness

(* The wire addresses the payload explicitly, so exactly one editor node owns
   each diff. *)
let diff_in_option () =
  register ~__FILE__ ~title:"a diff inside an optional names the payload"
    ~tags:["run"]
  @@ fun () ->
  let dir = project "round_trip" in
  let* () = start dir in
  let* t = run dir "test_opt_out.catala_en" ~scope:"Give_test" in
  Check.is_true t.assert_failures
    ~error_msg:"the wrong expectation did not fail";
  if
    not
      (List.exists
         (fun (d : O.diff) -> List.mem (`EnumPayload "Present") d.path)
         t.diffs)
  then Test.fail "no diff path names the payload";
  unit

(* The input filter drops an absent option, not every payload-less
   constructor. *)
let bare_input () =
  register ~__FILE__ ~title:"a bare constructor survives input serialization"
    ~tags:["run"]
  @@ fun () ->
  let dir = project "round_trip" in
  let* () = start dir in
  let* tests = read dir "test_tint.catala_en" in
  write_file (dir // "inputs.json")
    ~contents:(J.string_of_test_inputs (List.hd tests).test_inputs);
  let* r = testcase dir ["serialize-inputs"; "--input=inputs.json"] in
  if not (contains (succeed r) {|"tint": "Red"|}) then
    Test.fail "a bare user-enum input was dropped:\n%s" r.out;
  unit

(* The run prepares the modules with clerk itself, the editor's clerk when it
   names one; a failure must be about clerk, not the interpreter's hint. *)
let failed_preparation () =
  register ~__FILE__ ~title:"a failed preparation is reported" ~tags:["run"]
  @@ fun () ->
  let dir =
    project
      ~only:["clerk.toml"; "tint.catala_en"; "test_tint.catala_en"]
      "round_trip"
  in
  let* r =
    testcase dir
      ~env:["CATALA_CLERK_PATH", "/nonexistent/clerk"]
      ["run"; "--scope"; "Paint_tint"; "test_tint.catala_en"]
  in
  if not (contains r.err "Could not prepare") then
    Test.fail "the failed preparation was not reported:\n%s" r.err;
  unit

let register () =
  diff_in_option ();
  bare_input ();
  failed_preparation ()
