(* What [carry_value] does with one field of a drifted test, one row per
   situation: [old_typ] inferred from the test's literal, [new_typ] from the
   live module, the value the tester wrote. Recovery is triggered per file, so
   most rows are fields that did not change -- described from one literal,
   narrower than the live type either way. *)

module O = Catala_types_t
module Lib = Test_case_parser_lib

let money n = O.{ value = Money n; attrs = [] }
let int n = O.{ value = Integer n; attrs = [] }
let unset = O.{ value = Unset; attrs = [] }
let arr l = O.{ value = Array (Array.of_list l); attrs = [] }

(* Options as the readers build them. *)
let absent inner =
  O.{ value = Enum (Lib.mk_optional_enum_decl inner, (Lib.option_absent, None)); attrs = [] }

let present inner v =
  O.
    {
      value = Enum (Lib.mk_optional_enum_decl inner, (Lib.option_present, Some v));
      attrs = [];
    }

let colour ctors =
  O.{ enum_name = "M.Colour"; constructors = ctors; ctor_attrs = [] }

let red decl = O.{ value = Enum (decl, ("Red", None)); attrs = [] }
let red_of decl v = O.{ value = Enum (decl, ("Red", Some v)); attrs = [] }

let detail fields =
  O.{ struct_name = "M.Detail"; fields }

let struct_of decl fields = O.{ value = Struct (decl, fields); attrs = [] }

type row = {
  what : string;
  old_typ : O.typ;
  new_typ : O.typ;
  value : O.runtime_value;
  outcome : O.carry_outcome;
  carried : bool;
}

let rows =
  [
    (* ---- nothing changed ------------------------------------------------ *)
    {
      what = "a scalar whose type did not change";
      old_typ = TMoney; new_typ = TMoney; value = money 1000;
      outcome = Fits; carried = true;
    };
    {
      what = "a scalar whose type changed outright";
      old_typ = TMoney; new_typ = TDate; value = money 1000;
      outcome = TypeChanged (TMoney, TDate); carried = false;
    };
    {
      what = "an integer where money is now wanted: NOT a conversion";
      old_typ = TInt; new_typ = TMoney; value = int 5;
      outcome = TypeChanged (TInt, TMoney); carried = false;
    };

    (* ---- the recovered type is unknown ---------------------------------- *)
    {
      (* The common case: the UI writes `impossible` for an unfilled input.
         [value_fits] accepts [Unset] against any type. *)
      what = "a field the old test never filled";
      old_typ = TUnset; new_typ = TMoney; value = unset;
      outcome = WasUnset; carried = false;
    };
    {
      (* Valid for any list. *)
      what = "an empty list, element type unknowable from it";
      old_typ = TArray TUnset; new_typ = TArray TMoney; value = arr [];
      outcome = Fits; carried = true;
    };
    {
      (* A bare `Absent` recovers as [TOption TUnset]; valid at any option. *)
      what = "an unchanged Absent, payload type unknowable from it";
      old_typ = TOption TUnset; new_typ = TOption TMoney; value = absent TUnit;
      outcome = Fits; carried = true;
    };

    (* ---- options ------------------------------------------------------- *)
    {
      what = "a field that became optional";
      old_typ = TMoney; new_typ = TOption TMoney; value = money 1000;
      outcome = Wrap; carried = true;
    };
    {
      what = "a field that stopped being optional, and had a value";
      old_typ = TOption TMoney; new_typ = TMoney; value = present TMoney (money 1000);
      outcome = Unwrap; carried = true;
    };
    {
      what = "a field that stopped being optional, and was Absent";
      old_typ = TOption TMoney; new_typ = TMoney; value = absent TMoney;
      outcome = WasAbsentNowRequired; carried = false;
    };
    {
      what = "a field that stopped being optional, and changed type too";
      old_typ = TOption TMoney; new_typ = TDate; value = present TMoney (money 1000);
      outcome = TypeChanged (TOption TMoney, TDate); carried = false;
    };
    {
      (* Neither wrap nor unwrap; the payload descriptions differ anyway. *)
      what = "an unchanged optional enum, described from one literal";
      old_typ = TOption (TEnum (colour ["Red", None]));
      new_typ = TOption (TEnum (colour ["Red", None; "Green", None; "Blue", None]));
      value = present (TEnum (colour ["Red", None])) (red (colour ["Red", None]));
      outcome = Fits; carried = true;
    };

    (* ---- partial declarations, which is the ordinary case ---------------- *)
    {
      (* One constructor of three; structural equality never holds. *)
      what = "an unchanged enum, described from one literal";
      old_typ = TEnum (colour ["Red", None]);
      new_typ = TEnum (colour ["Red", None; "Green", None; "Blue", None]);
      value = red (colour ["Red", None]);
      outcome = Fits; carried = true;
    };
    {
      what = "an enum that lost the constructor this value used";
      old_typ = TEnum (colour ["Red", None]);
      new_typ = TEnum (colour ["Green", None; "Blue", None]);
      value = red (colour ["Red", None]);
      outcome =
        TypeChanged
          ( TEnum (colour ["Red", None]),
            TEnum (colour ["Green", None; "Blue", None]) ); carried = false;
    };
    {
      (* Fewer fields, in the test's own order: the ordinary case. *)
      what = "a struct the test filled only partly";
      old_typ = TStruct (detail ["fee", TMoney; "rank", TInt]);
      new_typ = TStruct (detail ["rank", TInt; "fee", TMoney; "stamp", TDate]);
      value = struct_of (detail ["fee", TMoney; "rank", TInt])
                ["fee", money 1200; "rank", int 3];
      outcome = Fits; carried = true;
    };
    {
      what = "a struct field that changed type underneath";
      old_typ = TStruct (detail ["fee", TMoney]);
      new_typ = TStruct (detail ["fee", TDate]);
      value = struct_of (detail ["fee", TMoney]) ["fee", money 1200];
      outcome =
        TypeChanged
          (TStruct (detail ["fee", TMoney]), TStruct (detail ["fee", TDate])); carried = false;
    };

    (* ---- the value claims more than the live declaration allows ---------- *)
    (* Each of these once answered [Fits], and the written working copy did
       not read back. A carried value must always survive an ordinary read. *)
    {
      what = "a struct that lost a field the test filled";
      old_typ = TStruct (detail ["fee", TMoney; "stamp", TInt]);
      new_typ = TStruct (detail ["fee", TMoney]);
      value = struct_of (detail ["fee", TMoney; "stamp", TInt])
                ["fee", money 1200; "stamp", int 3];
      outcome =
        TypeChanged
          ( TStruct (detail ["fee", TMoney; "stamp", TInt]),
            TStruct (detail ["fee", TMoney]) ); carried = false;
    };
    {
      what = "an enum constructor that now requires a payload, value bare";
      old_typ = TEnum (colour ["Red", None]);
      new_typ = TEnum (colour ["Red", Some TMoney; "Green", None]);
      value = red (colour ["Red", None]);
      outcome =
        TypeChanged
          ( TEnum (colour ["Red", None]),
            TEnum (colour ["Red", Some TMoney; "Green", None]) ); carried = false;
    };
    {
      what = "an enum constructor that lost its payload, value has one";
      old_typ = TEnum (colour ["Red", Some TMoney]);
      new_typ = TEnum (colour ["Red", None; "Green", None]);
      value = red_of (colour ["Red", Some TMoney]) (money 500);
      outcome =
        TypeChanged
          ( TEnum (colour ["Red", Some TMoney]),
            TEnum (colour ["Red", None; "Green", None]) ); carried = false;
    };
  ]

let show_outcome : O.carry_outcome -> string = function
  | Fits -> "Fits"
  | Wrap -> "Wrap"
  | Unwrap -> "Unwrap"
  | WasUnset -> "WasUnset"
  | WasAbsentNowRequired -> "WasAbsentNowRequired"
  | TypeChanged (a, b) ->
    Printf.sprintf "TypeChanged (%s -> %s)" (Lib.typ_name a) (Lib.typ_name b)

let check_row r =
  let carried, outcome =
    Lib.carry_value ~old_typ:r.old_typ ~new_typ:r.new_typ r.value
  in
  if outcome <> r.outcome then
    failwith
      (Printf.sprintf "%s: expected %s, got %s" r.what (show_outcome r.outcome)
         (show_outcome outcome));
  if Option.is_some carried <> r.carried then
    failwith
      (Printf.sprintf "%s: expected %s, got %s" r.what
         (if r.carried then "a carried value" else "no value")
         (if Option.is_some carried then "a carried value" else "no value"))

(* A carried value is re-described with the live type. *)
let check_adopts_live_declarations () =
  let live = colour ["Red", None; "Green", None; "Blue", None] in
  match
    Lib.carry_value ~old_typ:(TEnum (colour ["Red", None]))
      ~new_typ:(TEnum live) (red (colour ["Red", None]))
  with
  | Some { value = Enum (decl, _); _ }, Fits ->
    if List.length decl.O.constructors <> 3 then
      failwith "a carried enum kept the partial declaration it was recovered with"
  | _ -> failwith "an unchanged enum did not carry"

(* Attributes belong to the value, not to the type it is carried into. *)
let check_keeps_attributes () =
  let v = O.{ value = Money 1000; attrs = [Uid "abc"] } in
  match Lib.carry_value ~old_typ:TMoney ~new_typ:(TOption TMoney) v with
  | Some { value = Enum (_, (_, Some payload)); _ }, Wrap ->
    if payload.O.attrs <> [O.Uid "abc"] then
      failwith "wrapping a value dropped its attributes"
  | _ -> failwith "a field that became optional did not wrap"

let () =
  let open Tezt.Test in
  register ~__FILE__ ~title:"carry_value: the table"
    ~tags:["unit"; "carry"] (fun () ->
      Lwt.return @@ List.iter check_row rows);
  register ~__FILE__ ~title:"carry_value: adopts the live declarations"
    ~tags:["unit"; "carry"] (fun () ->
      Lwt.return @@ check_adopts_live_declarations ());
  register ~__FILE__ ~title:"carry_value: keeps the value's attributes"
    ~tags:["unit"; "carry"] (fun () -> Lwt.return @@ check_keeps_attributes ())

let () = Tezt.Test.run ()
