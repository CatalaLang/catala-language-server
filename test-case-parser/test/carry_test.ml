(* What [carry_value] does with one field of a drifted test, one row per
   situation: [old_typ] inferred from the test's literal, [new_typ] from the
   live module, the value the tester wrote. Recovery is triggered per file, so
   most rows are fields that did not change -- described from one literal,
   narrower than the live type either way. *)

module O = Catala_types_t
open Testcase_lib
module R = Recovery

let money n = O.{ value = Money n; attrs = [] }
let int n = O.{ value = Integer n; attrs = [] }
let unset = O.{ value = Unset; attrs = [] }
let arr l = O.{ value = Array (Array.of_list l); attrs = [] }

(* Options as the readers build them. *)
let absent inner =
  O.
    {
      value =
        Enum (Model.mk_optional_enum_decl inner, (Model.option_absent, None));
      attrs = [];
    }

let present inner v =
  O.
    {
      value =
        Enum (Model.mk_optional_enum_decl inner, (Model.option_present, Some v));
      attrs = [];
    }

let colour ctors =
  O.{ enum_name = "M.Colour"; constructors = ctors; ctor_attrs = [] }

let enum_named enum_name ctors =
  O.{ enum_name; constructors = ctors; ctor_attrs = [] }

let red decl = O.{ value = Enum (decl, ("Red", None)); attrs = [] }
let red_of decl v = O.{ value = Enum (decl, ("Red", Some v)); attrs = [] }
let detail fields = O.{ struct_name = "M.Detail"; fields }
let struct_of decl fields = O.{ value = Struct (decl, fields); attrs = [] }

type row = {
  what : string;
  old_typ : O.typ;
  new_typ : O.typ;
  value : O.runtime_value;
  outcome : O.carry_outcome;
}

let rows =
  [
    (* ---- nothing changed ------------------------------------------------ *)
    {
      what = "a scalar whose type did not change";
      old_typ = TMoney;
      new_typ = TMoney;
      value = money 1000;
      outcome = Fits;
    };
    {
      what = "a scalar whose type changed outright";
      old_typ = TMoney;
      new_typ = TDate;
      value = money 1000;
      outcome = TypeChanged (TMoney, TDate);
    };
    {
      what = "an integer where money is now wanted: NOT a conversion";
      old_typ = TInt;
      new_typ = TMoney;
      value = int 5;
      outcome = TypeChanged (TInt, TMoney);
    };
    (* ---- the recovered type is unknown ---------------------------------- *)
    {
      (* The common case: the UI writes `impossible` for an unfilled input.
         [value_fits] accepts [Unset] against any type. *)
      what = "a field the old test never filled";
      old_typ = TUnset;
      new_typ = TMoney;
      value = unset;
      outcome = WasUnset;
    };
    {
      (* Valid for any list. *)
      what = "an empty list, element type unknowable from it";
      old_typ = TArray TUnset;
      new_typ = TArray TMoney;
      value = arr [];
      outcome = Fits;
    };
    {
      (* A bare `Absent` recovers as [TOption TUnset]; valid at any option. *)
      what = "an unchanged Absent, payload type unknowable from it";
      old_typ = TOption TUnset;
      new_typ = TOption TMoney;
      value = absent TUnit;
      outcome = Fits;
    };
    (* ---- options ------------------------------------------------------- *)
    {
      what = "a field that became optional";
      old_typ = TMoney;
      new_typ = TOption TMoney;
      value = money 1000;
      outcome = Wrap;
    };
    {
      what = "a field that stopped being optional, and had a value";
      old_typ = TOption TMoney;
      new_typ = TMoney;
      value = present TMoney (money 1000);
      outcome = Unwrap;
    };
    {
      what = "a field that stopped being optional, and was Absent";
      old_typ = TOption TMoney;
      new_typ = TMoney;
      value = absent TMoney;
      outcome = WasAbsentNowRequired;
    };
    {
      what = "a field that stopped being optional, and changed type too";
      old_typ = TOption TMoney;
      new_typ = TDate;
      value = present TMoney (money 1000);
      outcome = TypeChanged (TOption TMoney, TDate);
    };
    {
      (* Neither wrap nor unwrap; the payload descriptions differ anyway. *)
      what = "an unchanged optional enum, described from one literal";
      old_typ = TOption (TEnum (colour ["Red", None]));
      new_typ =
        TOption (TEnum (colour ["Red", None; "Green", None; "Blue", None]));
      value =
        present (TEnum (colour ["Red", None])) (red (colour ["Red", None]));
      outcome = Fits;
    };
    (* ---- names: the type's own name must agree when the literal gave one
       -- *)
    {
      what = "a constructor of the same name in another enum";
      old_typ = TEnum (enum_named "M.Colour" ["Red", None]);
      new_typ = TEnum (enum_named "M.Mood" ["Red", None; "Blue", None]);
      value = red (enum_named "M.Colour" ["Red", None]);
      outcome =
        TypeChanged
          ( TEnum (enum_named "M.Colour" ["Red", None]),
            TEnum (enum_named "M.Mood" ["Red", None; "Blue", None]) );
    };
    {
      (* An alias or a missing module prefix is not a different type. *)
      what = "the same enum, written through a module alias";
      old_typ = TEnum (enum_named "AL.Colour" ["Red", None]);
      new_typ = TEnum (enum_named "Aides.Colour" ["Red", None; "Green", None]);
      value = red (enum_named "AL.Colour" ["Red", None]);
      outcome = Fits;
    };
    {
      what = "a bare constructor, whose enum the literal did not name";
      old_typ = TEnum (enum_named Model.unknown_enum_name ["Red", None]);
      new_typ = TEnum (enum_named "M.Mood" ["Red", None]);
      value = red (enum_named Model.unknown_enum_name ["Red", None]);
      outcome = Fits;
    };
    {
      what = "a record of the same shape but another name";
      old_typ = TStruct O.{ struct_name = "M.Other"; fields = ["x", TInt] };
      new_typ = TStruct O.{ struct_name = "M.Detail"; fields = ["x", TInt] };
      value =
        struct_of
          O.{ struct_name = "M.Other"; fields = ["x", TInt] }
          ["x", int 1];
      outcome =
        TypeChanged
          ( TStruct O.{ struct_name = "M.Other"; fields = ["x", TInt] },
            TStruct O.{ struct_name = "M.Detail"; fields = ["x", TInt] } );
    };
    (* ---- partial declarations, which is the ordinary case ---------------- *)
    {
      (* One constructor of three; structural equality never holds. *)
      what = "an unchanged enum, described from one literal";
      old_typ = TEnum (colour ["Red", None]);
      new_typ = TEnum (colour ["Red", None; "Green", None; "Blue", None]);
      value = red (colour ["Red", None]);
      outcome = Fits;
    };
    {
      what = "an enum that lost the constructor this value used";
      old_typ = TEnum (colour ["Red", None]);
      new_typ = TEnum (colour ["Green", None; "Blue", None]);
      value = red (colour ["Red", None]);
      outcome =
        TypeChanged
          ( TEnum (colour ["Red", None]),
            TEnum (colour ["Green", None; "Blue", None]) );
    };
    {
      (* Fewer fields, in the test's own order: the ordinary case. *)
      what = "a struct the test filled only partly";
      old_typ = TStruct (detail ["fee", TMoney; "rank", TInt]);
      new_typ = TStruct (detail ["rank", TInt; "fee", TMoney; "stamp", TDate]);
      value =
        struct_of
          (detail ["fee", TMoney; "rank", TInt])
          ["fee", money 1200; "rank", int 3];
      outcome = Partial;
    };
    {
      what = "a struct field that changed type underneath";
      old_typ = TStruct (detail ["fee", TMoney]);
      new_typ = TStruct (detail ["fee", TDate]);
      value = struct_of (detail ["fee", TMoney]) ["fee", money 1200];
      outcome =
        TypeChanged
          (TStruct (detail ["fee", TMoney]), TStruct (detail ["fee", TDate]));
    };
    (* ---- the value claims more than the live declaration allows ---------- *)
    (* Each of these once answered [Fits], and the written working copy did
       not read back. A carried value must always survive an ordinary read. *)
    {
      what = "a struct that lost a field the test filled";
      old_typ = TStruct (detail ["fee", TMoney; "stamp", TInt]);
      new_typ = TStruct (detail ["fee", TMoney]);
      value =
        struct_of
          (detail ["fee", TMoney; "stamp", TInt])
          ["fee", money 1200; "stamp", int 3];
      (* The surviving field carries; the lost one is reported at its path. *)
      outcome = Partial;
    };
    {
      (* The recovered declaration is narrower than the live one: the wrap must
         go by fit, not by type equality. *)
      what = "an enum field that became optional";
      old_typ = TEnum (colour ["Red", None]);
      new_typ = TOption (TEnum (colour ["Red", None; "Green", None]));
      value = red (colour ["Red", None]);
      outcome = Wrap;
    };
    {
      (* The mirror: unwrap goes by fit for the same reason. *)
      what = "an optional enum field that stopped being optional";
      old_typ = TOption (TEnum (colour ["Red", None]));
      new_typ = TEnum (colour ["Red", None; "Green", None]);
      value =
        present (TEnum (colour ["Red", None])) (red (colour ["Red", None]));
      outcome = Unwrap;
    };
    {
      (* An enum value is not an option value: never read as Absent. *)
      what =
        "an enum where an option of that enum is wanted, old type an option";
      old_typ = TOption (TEnum (colour ["Red", None]));
      new_typ = TOption (TEnum (colour ["Red", None; "Green", None]));
      value = red (colour ["Red", None]);
      outcome =
        TypeChanged
          ( TOption (TEnum (colour ["Red", None])),
            TOption (TEnum (colour ["Red", None; "Green", None])) );
    };
    {
      what = "an enum constructor that now requires a payload, value bare";
      old_typ = TEnum (colour ["Red", None]);
      new_typ = TEnum (colour ["Red", Some TMoney; "Green", None]);
      value = red (colour ["Red", None]);
      outcome =
        TypeChanged
          ( TEnum (colour ["Red", None]),
            TEnum (colour ["Red", Some TMoney; "Green", None]) );
    };
    {
      what = "an enum constructor that lost its payload, value has one";
      old_typ = TEnum (colour ["Red", Some TMoney]);
      new_typ = TEnum (colour ["Red", None; "Green", None]);
      value = red_of (colour ["Red", Some TMoney]) (money 500);
      outcome =
        TypeChanged
          ( TEnum (colour ["Red", Some TMoney]),
            TEnum (colour ["Red", None; "Green", None]) );
    };
  ]

let show_outcome : O.carry_outcome -> string = function
  | Fits -> "Fits"
  | Wrap -> "Wrap"
  | Unwrap -> "Unwrap"
  | WasUnset -> "WasUnset"
  | WasAbsentNowRequired -> "WasAbsentNowRequired"
  | TypeChanged (a, b) ->
    Printf.sprintf "TypeChanged (%s -> %s)" (Model.typ_name a)
      (Model.typ_name b)
  | Dropped -> "Dropped"
  | Partial -> "Partial"

let check_row r =
  let outcome =
    R.outcome_of (R.carry_value ~old_typ:r.old_typ ~new_typ:r.new_typ r.value)
  in
  if outcome <> r.outcome then
    failwith
      (Printf.sprintf "%s: expected %s, got %s" r.what (show_outcome r.outcome)
         (show_outcome outcome))

(* A carried value is re-described with the live type. *)
let check_adopts_live_declarations () =
  let live = colour ["Red", None; "Green", None; "Blue", None] in
  match
    R.carry_value
      ~old_typ:(TEnum (colour ["Red", None]))
      ~new_typ:(TEnum live)
      (red (colour ["Red", None]))
  with
  | R.Fits { value = Enum (decl, _); _ } ->
    if List.length decl.O.constructors <> 3 then
      failwith
        "a carried enum kept the partial declaration it was recovered with"
  | _ -> failwith "an unchanged enum did not carry"

(* Attributes belong to the value, not to the type it is carried into. *)
let check_keeps_attributes () =
  let v = O.{ value = Money 1000; attrs = [Uid "abc"] } in
  match R.carry_value ~old_typ:TMoney ~new_typ:(TOption TMoney) v with
  | R.Wrap { value = Enum (_, (_, Some payload)); _ } ->
    if payload.O.attrs <> [O.Uid "abc"] then
      failwith "wrapping a value dropped its attributes"
  | _ -> failwith "a field that became optional did not wrap"

(* A record with one renamed field keeps the others; the rename shows up as a
   dropped old name and a blank new one, at their paths. *)
let check_carries_inside_records () =
  let old_typ = O.TStruct (detail ["x", O.TMoney; "second", O.TInt]) in
  let new_decl = detail ["x", O.TMoney; "amount", O.TInt] in
  let v =
    struct_of
      (detail ["x", O.TMoney; "second", O.TInt])
      ["x", money 1000; "second", int 3]
  in
  match R.carry_value ~old_typ ~new_typ:(O.TStruct new_decl) v with
  | R.Partial ({ value = Struct (decl, fields); _ }, nested) ->
    if decl <> new_decl then
      failwith "a carried record kept the old declaration";
    if List.assoc_opt "x" fields <> Some (money 1000) then
      failwith "an unchanged field of a changed record was not kept";
    if List.assoc_opt "amount" fields <> Some unset then
      failwith "a new field of a changed record is not a blank";
    if List.mem_assoc "second" fields then
      failwith "a dropped field survived in the carried record";
    let expect p o =
      if not (List.mem (p, o) nested) then
        failwith (Printf.sprintf "missing nested outcome %s" (show_outcome o))
    in
    expect [`StructField "second"] Dropped;
    expect [`StructField "amount"] WasUnset;
    if List.length nested <> 2 then failwith "unexpected nested outcomes"
  | r ->
    let outcome = R.outcome_of r in
    failwith
      (Printf.sprintf
         "a record with one renamed field: expected Partial, got %s"
         (show_outcome outcome))

(* A payload is carried like any nested value, at a path that names its
   constructor, as the editor addresses it. *)
let check_carries_inside_payloads () =
  let old_detail = detail ["x", O.TMoney; "second", O.TInt] in
  let new_detail = detail ["x", O.TMoney; "amount", O.TInt] in
  let v = struct_of old_detail ["x", money 1000; "second", int 3] in
  let check what ~old_typ ~new_typ ~ctor value =
    match R.carry_value ~old_typ ~new_typ value with
    | R.Partial
        ( { value = Enum (_, (c, Some { value = Struct (decl, _); _ })); _ },
          nested ) ->
      if c <> ctor then failwith (what ^ ": the constructor changed");
      if decl <> new_detail then
        failwith (what ^ ": the payload kept the old declaration");
      let expect p o =
        if not (List.mem (p, o) nested) then
          failwith
            (Printf.sprintf "%s: missing nested outcome %s" what
               (show_outcome o))
      in
      expect [`EnumPayload ctor; `StructField "second"] Dropped;
      expect [`EnumPayload ctor; `StructField "amount"] WasUnset;
      if List.length nested <> 2 then
        failwith (what ^ ": unexpected nested outcomes")
    | r ->
      let outcome = R.outcome_of r in
      failwith
        (Printf.sprintf "%s: expected Partial, got %s" what
           (show_outcome outcome))
  in
  let old_colour = colour ["Red", Some (O.TStruct old_detail)] in
  let new_colour = colour ["Red", Some (O.TStruct new_detail)] in
  check "enum payload" ~old_typ:(O.TEnum old_colour)
    ~new_typ:(O.TEnum new_colour) ~ctor:"Red" (red_of old_colour v);
  check "option payload" ~old_typ:(O.TOption (O.TStruct old_detail))
    ~new_typ:(O.TOption (O.TStruct new_detail)) ~ctor:Model.option_present
    (present (O.TStruct old_detail) v)

(* Elements carry one by one, and a list none of whose elements carry is one
   TypeChanged, not one per element. *)
let check_carries_inside_lists () =
  let old_elt = O.TStruct (detail ["x", O.TMoney; "second", O.TInt]) in
  let new_elt = O.TStruct (detail ["x", O.TMoney; "amount", O.TInt]) in
  let elt =
    struct_of
      (detail ["x", O.TMoney; "second", O.TInt])
      ["x", money 1; "second", int 1]
  in
  (match
     R.carry_value ~old_typ:(O.TArray old_elt) ~new_typ:(O.TArray new_elt)
       (arr [elt; elt])
   with
  | R.Partial ({ value = Array a; _ }, nested) ->
    if Array.length a <> 2 then failwith "a carried list lost elements";
    if not (List.mem ([`ListIndex 1; `StructField "amount"], O.WasUnset) nested)
    then failwith "nested outcomes of a list element are not indexed"
  | r ->
    let outcome = R.outcome_of r in
    failwith
      (Printf.sprintf "a list of changed records: expected Partial, got %s"
         (show_outcome outcome)));
  match
    R.carry_value ~old_typ:(O.TArray O.TMoney) ~new_typ:(O.TArray O.TDate)
      (arr [money 1; money 2])
  with
  | R.TypeChanged _ -> ()
  | r ->
    let outcome = R.outcome_of r and nested = R.nested_of r in
    failwith
      (Printf.sprintf
         "a list of moneys turned dates: expected one TypeChanged, got %s with \
          %d nested"
         (show_outcome outcome) (List.length nested))

let show_nested nested =
  String.concat "; "
    (List.map
       (fun (p, o) ->
         String.concat ""
           (List.map
              (function
                | `StructField n -> "." ^ n
                | `ListIndex i -> Printf.sprintf "[%d]" i
                | `TupleIndex i -> Printf.sprintf "(%d)" i
                | `EnumPayload c -> "<" ^ c ^ ">")
              p)
         ^ "="
         ^ show_outcome o)
       nested)

let expect_nested what nested p o =
  if not (List.mem (p, o) nested) then
    failwith
      (Printf.sprintf "%s: missing %s; nested = %s" what
         (show_nested [p, o])
         (show_nested nested))

(* Shapes below the top level, each at its path: transparent over options,
   indexed in lists and tuples, [Partial] only at the top. *)
let check_nested_shapes () =
  (* A list of options of a record that gained a field. *)
  let old_s = detail ["a", O.TInt]
  and new_s = detail ["a", O.TInt; "c", O.TInt] in
  (match
     R.carry_value ~old_typ:(TArray (TOption (TStruct old_s)))
       ~new_typ:(TArray (TOption (TStruct new_s)))
       (arr [present (O.TStruct old_s) (struct_of old_s ["a", int 1])])
   with
  | R.Partial ({ value = Array [| _ |]; _ }, nested) ->
    expect_nested "list of options" nested
      [`ListIndex 0; `EnumPayload Model.option_present; `StructField "c"]
      O.WasUnset;
    if List.length nested <> 1 then
      failwith ("list of options: nested = " ^ show_nested nested)
  | r -> failwith ("list of options: " ^ show_outcome (R.outcome_of r)));
  (* A tuple with one element changed keeps the other. *)
  (match
     R.carry_value
       ~old_typ:(TTuple [TInt; TMoney])
       ~new_typ:(TTuple [TInt; TDate])
       (arr [int 1; money 5])
   with
  | R.Partial
      ( { value = Array [| { value = Integer 1; _ }; { value = Unset; _ } |]; _ },
        [([`TupleIndex 1], TypeChanged _)] ) ->
    ()
  | r ->
    let outcome = R.outcome_of r and nested = R.nested_of r in
    failwith
      (Printf.sprintf "tuple: %s, nested = %s" (show_outcome outcome)
         (show_nested nested)));
  (* A record field that became optional wraps in place. *)
  let old_s = detail ["a", O.TInt] and new_s = detail ["a", O.TOption O.TInt] in
  (match
     R.carry_value ~old_typ:(TStruct old_s) ~new_typ:(TStruct new_s)
       (struct_of old_s ["a", int 1])
   with
  | R.Partial
      ( {
          value =
            Struct
              ( _,
                [
                  ( "a",
                    {
                      value = Enum (_, (ctor, Some { value = Integer 1; _ }));
                      _;
                    } );
                ] );
          _;
        },
        nested )
    when ctor = Model.option_present ->
    if nested <> [[`StructField "a"], O.Wrap] then
      failwith ("field became optional: nested = " ^ show_nested nested)
  | r ->
    let outcome = R.outcome_of r and nested = R.nested_of r in
    failwith
      (Printf.sprintf "field became optional: %s, nested = %s"
         (show_outcome outcome) (show_nested nested)));
  (* A Present record that lost a field: reported at the field's path, through
     the payload. *)
  let old_s = detail ["a", O.TInt; "b", O.TInt]
  and new_s = detail ["a", O.TInt] in
  (match
     R.carry_value ~old_typ:(TOption (TStruct old_s))
       ~new_typ:(TOption (TStruct new_s))
       (present (O.TStruct old_s) (struct_of old_s ["a", int 1; "b", int 2]))
   with
  | R.Partial
      ( { value = Enum (_, (_, Some { value = Struct (_, fields); _ })); _ },
        nested ) ->
    if List.mem_assoc "b" fields then failwith "present lost field: b survived";
    if
      nested
      <> [[`EnumPayload Model.option_present; `StructField "b"], O.Dropped]
    then failwith ("present lost field: nested = " ^ show_nested nested)
  | r -> failwith ("present lost field: " ^ show_outcome (R.outcome_of r)));
  (* Both ways at once, with a record inside: everything at its path, and no
     [Partial] below the top. *)
  let inner_old = detail ["x", O.TInt; "gone", O.TInt] in
  let inner_new = detail ["x", O.TInt; "added", O.TInt] in
  let old_s = detail ["inner", O.TStruct inner_old; "old_only", O.TInt] in
  let new_s = detail ["inner", O.TStruct inner_new; "new_only", O.TInt] in
  match
    R.carry_value ~old_typ:(TStruct old_s) ~new_typ:(TStruct new_s)
      (struct_of old_s
         [
           "inner", struct_of inner_old ["x", int 1; "gone", int 2];
           "old_only", int 3;
         ])
  with
  | R.Partial (_, nested) ->
    let expect = expect_nested "both ways" nested in
    expect [`StructField "inner"; `StructField "added"] O.WasUnset;
    expect [`StructField "inner"; `StructField "gone"] O.Dropped;
    expect [`StructField "new_only"] O.WasUnset;
    expect [`StructField "old_only"] O.Dropped;
    if List.exists (fun (_, o) -> o = O.Partial) nested then
      failwith ("both ways: a nested Partial: " ^ show_nested nested)
  | r ->
    let outcome = R.outcome_of r and nested = R.nested_of r in
    failwith
      (Printf.sprintf "both ways: %s, nested = %s" (show_outcome outcome)
         (show_nested nested))

(* A blank replacing an element keeps the element's attributes: row identity in
   the editor relies on the uid. *)
let check_hole_keeps_attributes () =
  let narrow = colour ["Red", None; "Blue", None] in
  let live = colour ["Red", None; "Green", None] in
  let row c =
    O.{ value = Enum (narrow, (c, None)); attrs = [Uid ("row-" ^ c)] }
  in
  match
    R.carry_value ~old_typ:(TArray (TEnum narrow))
      ~new_typ:(TArray (TEnum live))
      (arr [row "Red"; row "Blue"])
  with
  | R.Partial ({ value = Array [| kept; hole |]; _ }, _) ->
    if not (List.mem (O.Uid "row-Red") kept.O.attrs) then
      failwith "a carried element lost its uid";
    if hole.O.value <> O.Unset || not (List.mem (O.Uid "row-Blue") hole.O.attrs)
    then failwith "the blank replacing an element lost its uid"
  | r -> failwith ("hole keeps attributes: " ^ show_outcome (R.outcome_of r))

(* A record that became optional, or stopped being one, and changed inside: the
   payload carries piece by piece across the option boundary. *)
let check_carries_across_option () =
  let old_s = detail ["x", O.TInt; "gone", O.TInt] in
  let new_s = detail ["x", O.TInt; "added", O.TInt] in
  let v = struct_of old_s ["x", int 1; "gone", int 2] in
  (match
     R.carry_value ~old_typ:(TStruct old_s) ~new_typ:(TOption (TStruct new_s)) v
   with
  | R.Partial
      ( { value = Enum (_, (ctor, Some { value = Struct (_, fields); _ })); _ },
        nested )
    when ctor = Model.option_present ->
    if List.assoc_opt "x" fields <> Some (int 1) then failwith "wrapped: x lost";
    expect_nested "wrapped" nested
      [`EnumPayload Model.option_present; `StructField "gone"]
      O.Dropped;
    expect_nested "wrapped" nested
      [`EnumPayload Model.option_present; `StructField "added"]
      O.WasUnset
  | r ->
    let outcome = R.outcome_of r and nested = R.nested_of r in
    failwith
      (Printf.sprintf "wrapped: %s, nested = %s" (show_outcome outcome)
         (show_nested nested)));
  match
    R.carry_value ~old_typ:(TOption (TStruct old_s)) ~new_typ:(TStruct new_s)
      (present (TStruct old_s) v)
  with
  | R.Partial ({ value = Struct (_, fields); _ }, nested) ->
    if List.assoc_opt "x" fields <> Some (int 1) then
      failwith "unwrapped: x lost";
    expect_nested "unwrapped" nested [`StructField "gone"] O.Dropped;
    expect_nested "unwrapped" nested [`StructField "added"] O.WasUnset
  | r ->
    let outcome = R.outcome_of r and nested = R.nested_of r in
    failwith
      (Printf.sprintf "unwrapped: %s, nested = %s" (show_outcome outcome)
         (show_nested nested))

let () =
  let open Tezt.Test in
  register ~__FILE__ ~title:"carry_value: shapes below the top level"
    ~tags:["unit"; "carry"] (fun () -> Lwt.return @@ check_nested_shapes ());
  register ~__FILE__
    ~title:"carry_value: a blank keeps the element's attributes"
    ~tags:["unit"; "carry"] (fun () ->
      Lwt.return @@ check_hole_keeps_attributes ());
  register ~__FILE__ ~title:"carry_value: carries inside records"
    ~tags:["unit"; "carry"] (fun () ->
      Lwt.return @@ check_carries_inside_records ());
  register ~__FILE__ ~title:"carry_value: carries inside payloads"
    ~tags:["unit"; "carry"] (fun () ->
      Lwt.return @@ check_carries_inside_payloads ());
  register ~__FILE__ ~title:"carry_value: carries across an option boundary"
    ~tags:["unit"; "carry"] (fun () ->
      Lwt.return @@ check_carries_across_option ());
  register ~__FILE__ ~title:"carry_value: carries inside lists"
    ~tags:["unit"; "carry"] (fun () ->
      Lwt.return @@ check_carries_inside_lists ());
  register ~__FILE__ ~title:"carry_value: the table" ~tags:["unit"; "carry"]
    (fun () -> Lwt.return @@ List.iter check_row rows);
  register ~__FILE__ ~title:"carry_value: adopts the live declarations"
    ~tags:["unit"; "carry"] (fun () ->
      Lwt.return @@ check_adopts_live_declarations ());
  register ~__FILE__ ~title:"carry_value: keeps the value's attributes"
    ~tags:["unit"; "carry"] (fun () -> Lwt.return @@ check_keeps_attributes ())

let () = Tezt.Test.run ()
