(* What the editor writes reads back, through either reader, as the same
   file. *)

open Harness

let header = function
  | "fr" -> "Fichier écrit par l’éditeur de tests métier Catala"
  | _ -> "Written by the Catala testcase editor"

(* Scope declarations outside a catala-metadata block. *)
let stray_declarations text =
  let fence = ref "" in
  String.split_on_char '\n' text
  |> List.filter (fun l ->
      let starts prefix = String.starts_with ~prefix l in
      if starts "```catala-metadata" then (
        fence := "meta";
        false)
      else if starts "```catala" then (
        fence := "code";
        false)
      else if starts "```" then (
        fence := "";
        false)
      else starts "declaration scope" && !fence <> "meta")

let rec ctors_of_raw : O.runtime_value_raw -> string list = function
  | Enum (_, (c, v)) ->
    c
    :: Option.fold ~none:[]
         ~some:(fun (v : O.runtime_value) -> ctors_of_raw v.value)
         v
  | Struct (_, fields) ->
    List.concat_map
      (fun (_, (v : O.runtime_value)) -> ctors_of_raw v.value)
      fields
  | Array a ->
    List.concat_map
      (fun (v : O.runtime_value) -> ctors_of_raw v.value)
      (Array.to_list a)
  | _ -> []

let ios (t : O.test) = t.test_inputs @ t.test_outputs

let constructors tests =
  List.concat_map
    (fun t ->
      List.concat_map
        (fun (_, io) -> Option.fold ~none:[] ~some:ctors_of_raw (raw io))
        (ios t))
    tests

(* What a fixture must show beyond the round trip itself. *)
let also : string -> (string -> string -> unit Lwt.t) option = function
  | "test_context_vars" ->
    Some
      (fun dir written ->
        if not (contains written "definition c.y equals 99") then
          Test.fail "the override of y was lost";
        if contains written "definition c.z" then
          Test.fail "the unset context var z was written";
        let* r = run dir "test_context_vars.catala_en" ~scope:"C_test" in
        Check.is_false r.assert_failures ~error_msg:"the test itself fails";
        unit)
  | "test_optionals" ->
    (* written out, an optional and a bare enum look alike *)
    Some
      (fun dir _ ->
        Lwt_list.iter_s
          (fun reader ->
            let* tests = read ~reader dir "test_optionals.catala_en" in
            let typs =
              List.concat_map
                (fun t ->
                  List.map (fun (_, (io : O.test_io)) -> io.typ) (ios t))
                tests
            in
            if
              not
                (List.exists (function O.TOption _ -> true | _ -> false) typs)
            then Test.fail "%s does not type an optional as TOption" reader;
            if List.exists (function O.TEnum _ -> true | _ -> false) typs then
              Test.fail "%s types an optional as a bare enum" reader;
            unit)
          ["read"; "partial-read"])
  | "test_items" ->
    Some
      (fun _ written ->
        Check.((count written "testcase.uid" = 2) int)
          ~error_msg:"write emitted %L of %R item uids";
        unit)
  | ("test_plain_block" | "test_heading") as f ->
    (* a declaration in a plain block, or nested under a heading *)
    let scope =
      if f = "test_plain_block" then "Grant_plain" else "Grant_heading"
    in
    Some
      (fun dir _ ->
        Lwt_list.iter_s
          (fun reader ->
            let* tests = read ~reader dir (f ^ ".catala_en") in
            Check.list_mem Check.string scope (names_of tests)
              ~error_msg:(sf "%s misses %%L" reader);
            unit)
          ["read"; "partial-read"])
  | "test_spans" ->
    Some
      (fun dir written ->
        (* the case that motivated write/partial-read composition *)
        if not (contains written "equals 1 year + 2 month + 3 day") then
          Test.fail
            "write no longer joins a duration: check this test still bites";
        let* tests = partial_read dir "test_spans.catala_en" in
        let spans =
          List.concat_map
            (fun t -> List.filter_map (fun (_, io) -> raw io) (ios t))
            tests
        in
        if not (List.mem (O.Duration { years = 1; months = 2; days = 3 }) spans)
        then Test.fail "a multi-unit duration was not recovered";
        unit)
  | "test_bare" ->
    Some
      (fun dir _ ->
        (* a bare constructor names no enum: a partial read writes it bare... *)
        let* partial = partial_read dir "test_bare.catala_en" in
        let* text = write dir partial in
        if contains text "unknown." then
          Test.fail "the unknown-enum sentinel was written as a name";
        if not (contains text "equals Present content Green") then
          Test.fail "a bare constructor was not written bare";
        write_file (dir // "partial.catala_en") ~contents:text;
        let* () = typecheck dir "partial.catala_en" in
        (* ...and a rebuild, which has the live type, names it *)
        let* r = rebuild dir "test_bare.catala_en" in
        let* text = write dir (rebuilt r) in
        if not (contains text "equals Present content Bare.Colour.Green") then
          Test.fail "a rebuilt value did not adopt the live type's name";
        write_file (dir // "rebuilt.catala_en") ~contents:text;
        typecheck dir "rebuilt.catala_en")
  | "test_opt_fr" ->
    Some
      (fun dir written ->
        (* the surface keyword belongs to the writer alone *)
        let* () =
          Lwt_list.iter_s
            (fun reader ->
              let* tests = read ~reader dir "test_opt_fr.catala_fr" in
              let cs = constructors tests in
              if (not (List.mem "Present" cs)) || List.mem "Présent" cs then
                Test.fail "%s spells an option's constructor %s" reader
                  (String.concat ", " cs);
              unit)
            ["read"; "partial-read"]
        in
        if not (contains written "Présent contenu 50,00") then
          Test.fail "the writer does not emit the French keyword";
        unit)
  | _ -> None

(* These author a constructor bare: a partial read will not invent the enum name
   a full read learns from the declaration. *)
let spelled_alike f = not (List.mem f ["test_bare"; "test_tint"])

let round_trip ?(lang = "en") f =
  register ~__FILE__ ~title:(sf "round trip: %s" f) ~tags:["round_trip"]
  @@ fun () ->
  let dir = project "round_trip" in
  let* () = start dir in
  let file = sf "%s.catala_%s" f lang and out = sf "written.catala_%s" lang in
  let* tests = read dir file in
  if tests = [] then
    Test.fail "no test read: the round trip would prove nothing";
  let* written = write ~lang dir tests in
  write_file (dir // out) ~contents:written;
  let* () = typecheck dir out in
  Check.((count written (header lang) = 1) int)
    ~error_msg:(sf "the editor's header %S appears %%L times" (header lang));
  Check.((stray_declarations written = []) (list string))
    ~error_msg:"declared outside a metadata block: %L";
  (* the next save writes the same bytes, and a partial read accepts it *)
  let* again = read dir out in
  let* again = write ~lang dir again in
  Check.((again = written) string)
    ~error_msg:"write is not idempotent:\n%L\nthen\n%R";
  let* _ = partial_read dir out in
  let* () =
    if spelled_alike f then (
      let* partial = partial_read dir file in
      let* partial = write ~lang dir partial in
      Check.((partial = written) string)
        ~error_msg:
          "the readers spell it differently: partial-read gives\n\
           %L\n\
           read gives\n\
           %R";
      unit)
    else unit
  in
  match also f with Some check -> check dir written | None -> unit

(* Catala merges a scope's uses; partial read must too, or every block after the
   first is dropped, and deleted on promotion. *)
let split_scope_use () =
  register ~__FILE__
    ~title:"partial read merges a scope use split across blocks"
    ~tags:["partial"]
  @@ fun () ->
  let dir = project "round_trip" in
  let* () = start dir in
  write_file
    (dir // "test_split.catala_en")
    ~contents:
      (subst "  assertion (c.z = 198)\n"
         ~by:"```\n\n```catala\nscope C_test:\n  assertion (c.z = 198)\n"
         (read_file (dir // "test_context_vars.catala_en")));
  let* split = partial_read dir "test_split.catala_en" in
  let* split = write dir split in
  let* whole = read dir "test_context_vars.catala_en" in
  let* whole = write dir whole in
  Check.((split = whole) string)
    ~error_msg:"a second scope block was dropped or respelled:\n%L";
  unit

let generate () =
  register ~__FILE__ ~title:"a generated test typechecks" ~tags:["generate"]
  @@ fun () ->
  let dir = project "round_trip" in
  let* () = start dir in
  let* r =
    testcase dir ["generate"; "rename.catala_en"; "--scope"; "Example"]
  in
  let* text = write dir (J.test_list_of_string (succeed r)) in
  write_file (dir // "generated.catala_en") ~contents:text;
  typecheck dir "generated.catala_en"

let register () =
  List.iter round_trip
    [
      "test_bare";
      "test_context_vars";
      "test_heading";
      "test_implicit_import";
      "test_items";
      "test_optionals";
      "test_opt_out";
      "test_plain_block";
      "test_spans";
      "test_tint";
    ];
  round_trip ~lang:"fr" "test_opt_fr";
  split_scope_use ();
  generate ()
