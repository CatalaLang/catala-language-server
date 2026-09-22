(* A test its scope drifted under: what the rebuild carries, and what it says
   when it cannot. Modules that do not compile are made here, not committed:
   they cannot sit in a project clerk scans. *)

open Harness

let optionals
    ?(into = "optionals")
    ?(only = ["clerk.toml"; "optionals.catala_en"; "test_optionals.catala_en"])
    () =
  project ~into ~only "round_trip"

let rebuilt_reads_back dir r =
  let* text = write dir (rebuilt r) in
  write_file (dir // "rebuilt.catala_en") ~contents:text;
  let* () = typecheck dir "rebuilt.catala_en" in
  let* _ = read dir "rebuilt.catala_en" in
  unit

let note_names (r : O.recovery) =
  List.map
    (function
      | O.ModuleNotFound _ -> "ModuleNotFound"
      | ModuleWontCompile _ -> "ModuleWontCompile"
      | ScopeNotFound _ -> "ScopeNotFound"
      | Other _ -> "Other"
      | WorkingCopyUnreadable _ -> "WorkingCopyUnreadable"
      | WorkingCopyRecovered _ -> "WorkingCopyRecovered")
    r.notes

let no_note r =
  Check.((note_names r = []) (list string)) ~error_msg:"unexpected notes: %L"

let candidates (c : O.scope_candidate list) =
  List.map
    (fun (c : O.scope_candidate) -> sf "%s.%s:%d" c.module_name c.name c.shared)
    c

let targets (r : O.recovery) =
  List.map
    (fun (t : O.test) ->
      sf "%s.%s" t.tested_scope.module_name t.tested_scope.name)
    (rebuilt r)

let marks_of r path =
  List.filter (fun m -> String.starts_with ~prefix:(path ^ ":") m) (marks r)

(* Fields in the order the test wrote them, one of which the module has gained
   since. *)
let struct_order () =
  register ~__FILE__
    ~title:"a rebuilt struct pairs fields by name, in declaration order"
    ~tags:["rebuild"]
  @@ fun () ->
  let dir = project "round_trip" in
  let* () = start dir in
  let* r = rebuild dir "test_details.catala_en" in
  let* text = write dir (rebuilt r) in
  let fields =
    String.split_on_char '\n' text
    |> List.map String.trim
    |> List.filter (String.starts_with ~prefix:"-- ")
  in
  Check.(
    (fields
    = [
        "-- rank: 3";
        "-- fee: $12.00";
        "-- stamp: |2026-01-01|";
        "-- note: impossible";
      ])
      (list string))
    ~error_msg:"rebuilt fields are %L, wanted %R";
  unit

let why_not () =
  register ~__FILE__ ~title:"why a rebuild could not proceed"
    ~tags:["rebuild"; "notes"]
  @@ fun () ->
  (* a scope renamed: the module compiles perfectly *)
  let dir = optionals ~into:"renamed" () in
  edit dir "optionals.catala_en" (subst_word "Grant" ~by:"Attribution");
  let* () = start dir in
  let* renamed = rebuild dir "test_optionals.catala_en" in
  (match renamed.notes with
  | [ScopeNotFound n] ->
    Check.list_mem Check.string "Optionals.Attribution:3"
      (candidates n.candidates) ~error_msg:"the note does not offer %L"
  | _ ->
    Test.fail "wanted a missing scope, got %s"
      (String.concat ", " (note_names renamed)));
  (* a field renamed in the declaration only: the module no longer compiles *)
  let dir = optionals ~into:"broken" () in
  edit dir "optionals.catala_en"
    (subst "  input base content money" ~by:"  input assiette content money");
  let* () = start dir in
  let* broken = rebuild dir "test_optionals.catala_en" in
  (match broken.notes with
  | [ModuleWontCompile e] ->
    if not (contains e.error "unknown identifier") then
      Test.fail "the compiler's own diagnostic was not kept: %s" e.error
  | _ ->
    Test.fail "wanted a module that won't compile, got %s"
      (String.concat ", " (note_names broken)));
  (* either way the tester's values survive: the point of the view *)
  List.iter
    (fun (r : O.recovery) ->
      Check.((List.length r.tests = 3) int)
        ~error_msg:"recovered %L of %R tests")
    [renamed; broken];
  unit

(* The module compiles and has the scope, but gained an input of a type the
   editor has no form for. *)
let unsupported () =
  register ~__FILE__ ~title:"a scope the editor cannot describe"
    ~tags:["rebuild"; "notes"]
  @@ fun () ->
  let dir = optionals () in
  edit dir "optionals.catala_en"
    (subst "  input bonus content optional of money\n"
       ~by:
         "  input bonus content optional of money\n\
         \  input rate content decimal depends on x content integer\n");
  let* () = start dir in
  let* r = rebuild dir "test_optionals.catala_en" in
  (match r.notes with
  | [Other e] ->
    if not (contains e.error "unsupported: function type") then
      Test.fail "the note does not say what was unsupported: %s" e.error
  | _ ->
    Test.fail "wanted an Other note, got %s" (String.concat ", " (note_names r)));
  Check.((targets r = []) (list string)) ~error_msg:"rebuilt against %L";
  unit

(* What each field becomes is the carry table's business; the rebuild adds no
   note for a rename, and says nothing of an output a test never asserted. *)
let field_renamed () =
  register ~__FILE__ ~title:"a field renamed" ~tags:["rebuild"; "carry"]
  @@ fun () ->
  let dir = optionals () in
  edit dir "optionals.catala_en" (subst_word "base" ~by:"amount");
  let* () = start dir in
  let* r = rebuild dir "test_optionals.catala_en" in
  no_note r;
  Check.((List.length (marks_of r "total") = 2) int)
    ~error_msg:"total reported %L times, by the %R tests that assert it";
  unit

(* The same constructor in another enum does not carry; a record that became
   optional and changed inside carries piece by piece. *)
let moods () =
  register ~__FILE__ ~title:"drift inside enums and records"
    ~tags:["rebuild"; "carry"]
  @@ fun () ->
  let dir = project "refusals" in
  edit dir "moods.catala_en" (fun s ->
      s
      |> subst "input shade content Colour" ~by:"input shade content Mood"
      |> subst "input box content Box" ~by:"input box content optional of Box"
      |> subst "data gone content integer" ~by:"data added content integer"
      |> subst "-- Colour.Red : true" ~by:"-- Mood.Red : true"
      |> subst "-- Colour.Green : false" ~by:"-- Mood.Blue : false");
  let* () = start dir in
  let* r = rebuild dir "test_moods.catala_en" in
  let marks = marks r in
  List.iter
    (fun m ->
      Check.list_mem Check.string m marks ~error_msg:"no %L among the marks")
    [
      "shade:In:TypeChanged";
      "box:In:Partial";
      "box.Present.gone:In:Dropped";
      "box.Present.added:In:WasUnset";
    ];
  (* an unreadable working copy is a note, not silently ignored *)
  write_file
    (dir // "test_moods.catala_en.repair")
    ~contents:"```catala\nscope Broken:\n  definition\n```\n";
  let* r = rebuild dir "test_moods.catala_en" in
  Check.list_mem Check.string "WorkingCopyUnreadable" (note_names r)
    ~error_msg:"no %L note";
  unit

(* A context var the test never overrode is not damage; z is a context output,
   carried on its Out side only. *)
let context_vars () =
  register ~__FILE__ ~title:"context vars through a rebuild"
    ~tags:["rebuild"; "carry"]
  @@ fun () ->
  let dir =
    project
      ~only:
        ["clerk.toml"; "context_vars.catala_en"; "test_context_vars.catala_en"]
      "round_trip"
  in
  edit dir "context_vars.catala_en" (subst_word "x" ~by:"amount");
  let* () = start dir in
  let* r = rebuild dir "test_context_vars.catala_en" in
  Check.(
    (List.sort compare (marks r)
    = ["amount:In:WasUnset"; "x:In:Dropped"; "y:In:Fits"; "z:Out:Fits"])
      (list string))
    ~error_msg:"marks are %L, wanted %R";
  unit

(* Whatever a rebuild carries, its working copy reads back. *)
let no_longer_allowed () =
  register ~__FILE__ ~title:"a value the live declaration no longer allows"
    ~tags:["rebuild"; "carry"]
  @@ fun () ->
  (* a struct lost a field the test fills *)
  let dir =
    project ~into:"lost"
      ~only:["clerk.toml"; "details.catala_en"; "test_details.catala_en"]
      "round_trip"
  in
  edit dir "details.catala_en" (subst "  data stamp content date\n" ~by:"");
  let* () = start dir in
  let* r = rebuild dir "test_details.catala_en" in
  let* () = rebuilt_reads_back dir r in
  (* a constructor the test wrote bare now wants a payload *)
  let dir =
    project ~into:"payload"
      ~only:["clerk.toml"; "bare.catala_en"; "test_bare.catala_en"]
      "round_trip"
  in
  edit dir "bare.catala_en"
    (subst "  -- Green\n" ~by:"  -- Green content money\n");
  let* () = start dir in
  let* r = rebuild dir "test_bare.catala_en" in
  rebuilt_reads_back dir r

let reads_the_buffer () =
  register ~__FILE__ ~title:"the rebuild reads the buffer, not the file"
    ~tags:["rebuild"]
  @@ fun () ->
  let dir = project "round_trip" in
  let* () = start dir in
  let buffer =
    subst "calc.base equals $1000.00" ~by:"calc.base equals $1234.00"
      (read_file (dir // "test_optionals.catala_en"))
  in
  let* r =
    testcase ~stdin:buffer dir
      ["rebuild"; "--buffer-path"; "test_optionals.catala_en"; "-"]
  in
  if not (contains (succeed r) "123400") then
    Test.fail "the rebuild read the file on disk";
  unit

(* Drift one renames base; the tester fills amount and edits bonus in the
   working copy; drift two renames it back. *)
let drifted_twice () =
  register ~__FILE__ ~title:"a working copy the scope drifted under again"
    ~tags:["rebuild"; "working_copy"]
  @@ fun () ->
  let dir = optionals () in
  let original = read_file (dir // "optionals.catala_en") in
  edit dir "optionals.catala_en" (subst_word "base" ~by:"amount");
  let* () = start dir in
  let* r = rebuild dir "test_optionals.catala_en" in
  let money n : O.runtime_value = { value = Money n; attrs = [] } in
  let optional : O.enum_declaration =
    {
      enum_name = "Optional";
      constructors = ["Absent", None; "Present", Some TMoney];
      ctor_attrs = [];
    }
  in
  let edited =
    List.map
      (fun (t : O.test) ->
        {
          t with
          test_inputs =
            t.test_inputs
            |> update "amount" (with_raw (Money 123400))
            |> update "bonus"
                 (with_raw (Enum (optional, ("Present", Some (money 5000)))));
        })
      (rebuilt r)
  in
  let* text = write dir edited in
  write_file (dir // "test_optionals.catala_en.repair") ~contents:text;
  write_file (dir // "optionals.catala_en") ~contents:original;
  let* () = start dir in
  let* r = rebuild dir "test_optionals.catala_en" in
  (match
     List.find_map
       (function O.WorkingCopyRecovered n -> Some n | _ -> None)
       r.notes
   with
  | Some n ->
    Check.list_mem Check.string "amount" n.lost
      ~error_msg:"the lost edit %L was not named"
  | None -> Test.fail "the drifted working copy was not recovered");
  let t =
    List.find
      (fun (t : O.recovered_test) -> t.authored.testing_scope = "Grant_absent")
      r.tests
  in
  let input f =
    match t.rebuilt with
    | Some t -> raw (List.assoc f t.test_inputs)
    | None -> Test.fail "Grant_absent was not rebuilt"
  in
  if input "base" <> Some (Money 100000) then
    Test.fail "base should come from the original";
  (match input "bonus" with
  | Some (Enum (_, ("Present", Some { value = Money 5000; _ }))) -> ()
  | _ -> Test.fail "the working copy's edit to bonus was lost");
  unit

(* Candidates ranked by shared field names, never picked; [--scope] is the
   tester's answer, and a working copy saved that way remembers it. *)
let picker () =
  register ~__FILE__ ~title:"rebuilding against a scope the tester chose"
    ~tags:["rebuild"; "retarget"]
  @@ fun () ->
  let dir = optionals () in
  edit dir "optionals.catala_en" (fun s ->
      subst "scope Grant" ~by:"scope Attribution" s
      ^ "\n\
         ```catala-metadata\n\
         declaration scope Unrelated:\n\
        \  input widget content integer\n\
        \  output gadget content integer\n\
         ```\n\n\
         ```catala\n\
         scope Unrelated:\n\
        \  definition gadget equals widget\n\
         ```\n");
  let* () = start dir in
  let* r = rebuild dir "test_optionals.catala_en" in
  (match r.notes with
  | [ScopeNotFound n] ->
    Check.(
      (candidates n.candidates
      = ["Optionals.Attribution:3"; "Optionals.Unrelated:0"])
        (list string))
      ~error_msg:"candidates are %L, wanted %R"
  | _ ->
    Test.fail "wanted a missing scope, got %s"
      (String.concat ", " (note_names r)));
  let* r = rebuild ~scope:"Attribution" dir "test_optionals.catala_en" in
  no_note r;
  let fits = List.filter (fun m -> contains m ":Fits") (marks r) in
  Check.((List.length fits >= 6) int)
    ~error_msg:"only %L fields carried against the chosen scope";
  let* text = write dir (rebuilt r) in
  write_file (dir // "test_optionals.catala_en.repair") ~contents:text;
  let* r = rebuild dir "test_optionals.catala_en" in
  no_note r;
  Check.list_mem Check.string "Optionals.Attribution" (targets r)
    ~error_msg:"reopening forgot the chosen %L";
  unit

(* The module itself is gone: candidates come from anywhere in the project,
   including a literate [.catala_en.md] module, and a qualified [--scope]
   retargets the test. *)
let other_module () =
  register ~__FILE__ ~title:"rebuilding against a scope in another module"
    ~tags:["rebuild"; "retarget"; "run"]
  @@ fun () ->
  let dir =
    project ~into:"workspace/project"
      ~only:["clerk.toml"; "test_optionals.catala_en"]
      "round_trip"
  in
  write_file
    (dir // "benefits.catala_en.md")
    ~contents:
      (subst "> Module Optionals" ~by:"> Module Benefits"
         (read_file (fixtures // "round_trip" // "optionals.catala_en")));
  let* () = start dir in
  let* r = rebuild dir "test_optionals.catala_en" in
  (match r.notes with
  | [ModuleNotFound n] ->
    Check.((n.module_name = "Optionals") string) ~error_msg:"the note names %L";
    Check.((List.hd (candidates n.candidates) = "Benefits.Grant:3") string)
      ~error_msg:"best candidate is %L"
  | _ ->
    Test.fail "wanted a missing module, got %s"
      (String.concat ", " (note_names r)));
  let* r = rebuild ~scope:"Benefits.Grant" dir "test_optionals.catala_en" in
  no_note r;
  Check.list_mem Check.string "Benefits.Grant" (targets r)
    ~error_msg:"the rebuild does not target %L";
  let* repair = write dir (rebuilt r) in
  if not (contains repair "\n> Using Benefits\n") then
    Test.fail "the working copy does not use the new module";
  write_file (dir // "test_optionals.catala_en.repair") ~contents:repair;
  let* r = rebuild dir "test_optionals.catala_en" in
  no_note r;
  Check.list_mem Check.string "Benefits.Grant" (targets r)
    ~error_msg:"reopening forgot the chosen %L";
  (* run from memory, the way the editor runs it; the original is broken *)
  let run_repair ?(from = dir) ?(buffer = "test_optionals.catala_en") text =
    run from "-" ~stdin:text ~buffer ~scope:"Grant_absent"
  in
  let* t = run_repair repair in
  Check.is_false t.assert_failures
    ~error_msg:"the retargeted working copy fails";
  Check.file_not_exists (dir // "test_optionals__run.catala_en");
  (* the editor's cwd is the workspace folder, maybe far above the project *)
  let* t =
    run_repair ~from:(Filename.dirname dir)
      ~buffer:"project/test_optionals.catala_en" repair
  in
  Check.is_false t.assert_failures
    ~error_msg:"the run fails from outside the project";
  (* a failing run says what it disagrees with *)
  let* t =
    run_repair (subst ".total = $1000.00)" ~by:".total = $999.00)" repair)
  in
  Check.is_true t.assert_failures ~error_msg:"a wrong expectation did not fail";
  (match t.diffs with
  | d :: _ ->
    if not (contains (path_name d.path) "total") then
      Test.fail "the diff is on %s" (path_name d.path)
  | [] -> Test.fail "a failing run reported no diff");
  unit

let register () =
  struct_order ();
  why_not ();
  unsupported ();
  field_renamed ();
  moods ();
  context_vars ();
  no_longer_allowed ();
  reads_the_buffer ();
  drifted_twice ();
  picker ();
  other_module ()
