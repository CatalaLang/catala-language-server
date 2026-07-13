(* This file is part of the Catala project. Copyright (C) 2024 Inria.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Shared_ast
module I = Desugared.Ast
module O = Catala_types_t
module J = Catala_types_j
open Project
open Model
open Commands

(* ==========================================================================
   COPIED FROM THE CATALA COMPILER. DO NOT EDIT HERE.

   Verbatim copy of [Desugared.From_surface.translate_literal]
   (compiler/desugared/from_surface.ml, Catala commit cf28da9f9), with the
   constants it takes from that module's top level inlined. The compiler does
   not export it, and the partial reader must turn surface literals into values
   without desugaring, which fails on a drifted test.

   Nothing checks that the two stay in sync. On every compiler upgrade, compare
   with the original; delete this copy once [From_surface] exports the function.
   ========================================================================== *)
let translate_literal l pos =
  let open Surface.Ast in
  let module Runtime = Catala_runtime in
  let int1 = Runtime.integer_of_int 1 in
  let intminus1 = Runtime.integer_of_int (-1) in
  let int100 = Runtime.integer_of_int 100 in
  let rat100 = Runtime.decimal_of_integer int100 in
  match l with
  | LNumber ((Int i, _), None) -> LInt (Runtime.integer_of_string i)
  | LNumber ((Int i, _), Some (Percent, _)) ->
    LRat
      Runtime.(
        Oper.o_div_rat_rat (Expr.pos_to_runtime pos) (decimal_of_string i)
          rat100)
  | LNumber ((Dec (i, f), _), None) ->
    LRat Runtime.(decimal_of_string (i ^ "." ^ f))
  | LNumber ((Dec (i, f), _), Some (Percent, _)) ->
    LRat
      Runtime.(
        Oper.o_div_rat_rat (Expr.pos_to_runtime pos)
          (decimal_of_string (i ^ "." ^ f))
          rat100)
  | LBool b -> LBool b
  | LMoneyAmount i ->
    LMoney
      Runtime.(
        money_of_cents_integer
          (Oper.o_mult_int_int
             (if i.money_amount_sign then int1 else intminus1)
             (Oper.o_add_int_int
                (Oper.o_mult_int_int
                   (integer_of_string i.money_amount_units)
                   int100)
                (integer_of_string i.money_amount_cents))))
  | LNumber ((Int i, _), Some (Year, _)) ->
    LDuration (Runtime.duration_of_numbers (int_of_string i) 0 0)
  | LNumber ((Int i, _), Some (Month, _)) ->
    LDuration (Runtime.duration_of_numbers 0 (int_of_string i) 0)
  | LNumber ((Int i, _), Some (Day, _)) ->
    LDuration (Runtime.duration_of_numbers 0 0 (int_of_string i))
  | LNumber ((Dec (_, _), _), Some ((Year | Month | Day), _)) ->
    Message.error ~pos
      "Impossible to specify decimal amounts of days, months or years."
  | LDate date ->
    if date.literal_date_month > 12 then
      Message.error ~pos
        "There is an error in this date: the month number is bigger than 12.";
    if date.literal_date_day > 31 then
      Message.error ~pos
        "There is an error in this date: the day number is bigger than 31.";
    LDate
      (try
         Runtime.date_of_numbers date.literal_date_year date.literal_date_month
           date.literal_date_day
       with Failure _ ->
         Message.error ~pos
           "There is an error in this date, it does not correspond to a \
            correct calendar day.")
(* ======================= END OF COPY FROM THE COMPILER ================== *)

(* Every scope declaration in the file, across all code blocks, with the mark
   its attributes hang off. Declarations and uses are paired by name. *)
(* Block kind ignored: Catala accepts either, so an ordinary read does, and the
   two readers must agree on what counts as a test. What the editor emits is
   pinned by a test instead. *)
(* Every code item in the file, under whatever headings. *)
let surface_code_items (prg : Surface.Ast.program) =
  let open Surface.Ast in
  let rec items = function
    | CodeBlock (cb, _, _) -> cb
    | LawHeading (_, sub) -> List.concat_map items sub
    | _ -> []
  in
  List.concat_map items prg.program_items

let surface_scope_decls (prg : Surface.Ast.program) =
  List.filter_map
    (function Surface.Ast.ScopeDecl s, m -> Some (s, m) | _ -> None)
    (surface_code_items prg)

let surface_scope_uses (prg : Surface.Ast.program) =
  List.filter_map
    (function Surface.Ast.ScopeUse s, _ -> Some s | _ -> None)
    (surface_code_items prg)

(* Before name resolution the plugin's attributes are still raw [Src], on the
   mark of the declaration's name. *)
let surface_attr_string path (m : Pos.t) : string option =
  List.find_map
    (function
      | Shared_ast.Src ((p, _), Shared_ast.String (v, _), _)
        when p = "testcase" :: path ->
        Some v
      | _ -> None)
    (Pos.attrs m)

(* The value attributes an ordinary read keeps ([get_value] sees them resolved;
   here they are still raw). The array editor tracks rows by them. *)
let surface_value_attrs (m : Pos.t) : O.attr_def list =
  List.filter_map
    (function
      | Shared_ast.Src ((p, _), Shared_ast.String (v, _), _) -> (
        match p with
        | ["testcase"; "uid"] -> Some (O.Uid v)
        | ["testcase"; "array_item_label"] -> Some (O.ArrayItemLabel v)
        | _ -> None)
      | _ -> None)
    (Pos.attrs m)

let surface_has_attr path (m : Pos.t) : bool =
  List.exists
    (function Shared_ast.Src ((p, _), _, _) -> p = path | _ -> false)
    (Pos.attrs m)

(* [unowned_test_scopes] without typechecking. The two must agree. *)
let surface_scopes_by_ownership (prg : Surface.Ast.program) =
  List.partition_map
    (fun ((decl : Surface.Ast.scope_decl), _) ->
      let m = Mark.get decl.scope_decl_name in
      let name = Mark.remove decl.scope_decl_name in
      if surface_has_attr ["testcase"; "testui"] m then Either.Left name
      else Either.Right name)
    (List.filter
       (fun ((decl : Surface.Ast.scope_decl), _) ->
         surface_has_attr ["test"] (Mark.get decl.scope_decl_name))
       (surface_scope_decls prg))

(* A bare constructor names no enum. Never a name; never printed as one. *)

let enum_name_of_path (p : Surface.Ast.path) =
  match p with
  | [] -> unknown_enum_name
  | p -> String.concat "." (List.map Mark.remove p)

(* Same kind of value, by name for records and enums: elements of one list
   describe one type, however few constructors or fields each literal wrote. *)
let rec same_shape (a : O.typ) (b : O.typ) =
  match a, b with
  | O.TUnset, _ | _, O.TUnset -> true
  | O.TEnum x, O.TEnum y -> x.O.enum_name = y.O.enum_name
  | O.TStruct x, O.TStruct y -> x.O.struct_name = y.O.struct_name
  | O.TArray x, O.TArray y | O.TOption x, O.TOption y -> same_shape x y
  | O.TTuple xs, O.TTuple ys ->
    List.length xs = List.length ys && List.for_all2 same_shape xs ys
  | _ -> a = b

let read_partial_test_one
    (scope_decl : Surface.Ast.scope_decl)
    (scope_use : Surface.Ast.scope_use) : (O.test, string) Result.t =
  let ( let*? ) = Result.bind in
  let open Surface.Ast in
  let vlit v = Some O.{ value = v; pos = None } in
  let exception Err of string in
  let rec convert_literal (e : expression) :
      (O.typ * O.runtime_value, string) Result.t =
    try convert_literal_unguarded e
    with Message.CompilerError _ | Failure _ | Z.Overflow ->
      Error
        (Printf.sprintf "unreadable literal at %s"
           (Pos.to_string_short (Mark.get e)))
  and convert_literal_unguarded (e : expression) :
      (O.typ * O.runtime_value, string) Result.t =
    let ok ty raw =
      Ok (ty, O.{ value = raw; attrs = surface_value_attrs (Mark.get e) })
    in
    match Mark.remove e with
    | Literal l -> begin
      match translate_literal l Pos.void with
      | LBool b -> ok O.TBool (O.Bool b)
      | LInt z -> ok O.TInt (O.Integer (Z.to_int z))
      | LRat q -> ok O.TRat (O.Decimal (Q.to_float q))
      | LMoney m -> ok O.TMoney (O.Money (Z.to_int m))
      | LUnit -> assert false
      | LDate d ->
        let year, month, day = Dates_calc.date_to_ymd d in
        ok O.TDate (O.Date { year; month; day })
      | LDuration dur ->
        let years, months, days = Dates_calc.period_to_ymds dur in
        ok O.TDuration (O.Duration { years; months; days })
    end
    | EnumInject ((CBuiltin Absent, _), None) ->
      (* TOption, as in a signature, even though the value is carried as the
         Optional enum. A bare `Absent` has an unknown payload type. *)
      let edecl = mk_optional_enum_decl TUnit in
      ok (O.TOption O.TUnset) (O.Enum (edecl, (option_absent, None)))
    | EnumInject ((CBuiltin Present, _), Some sube) ->
      let*? subt, subv = convert_literal sube in
      let edecl = mk_optional_enum_decl subt in
      ok (O.TOption subt) (O.Enum (edecl, (option_present, Some subv)))
    (* `Mod.Enum.Ctor` carries the enum's full name, spelled as an ordinary read
       spells it. Bare, it names no enum. *)
    | EnumInject ((CConstr (p, (u, _)), _), None) ->
      let edecl =
        {
          O.enum_name = enum_name_of_path p;
          constructors = [u, None];
          ctor_attrs = [];
        }
      in
      ok (O.TEnum edecl) (O.Enum (edecl, (u, None)))
    | EnumInject ((CConstr (p, (u, _)), _), Some sube) ->
      let*? subt, subv = convert_literal sube in
      let edecl =
        {
          O.enum_name = enum_name_of_path p;
          constructors = [u, Some subt];
          ctor_attrs = [];
        }
      in
      ok (O.TEnum edecl) (O.Enum (edecl, (u, Some subv)))
    (* Element type unknowable from an empty list. *)
    | ArrayLit [] -> ok O.(TArray TUnset) O.(Array [||])
    | ArrayLit (_ :: _ as l) ->
      let*? ty, l =
        try
          let l =
            List.map
              (fun lit ->
                match convert_literal lit with
                | Error s -> raise (Err s)
                | Ok v -> v)
              l
          in
          let ty, _ = List.hd l in
          if not (List.for_all (fun (t, _) -> same_shape t ty) l) then
            raise (Err "a list mixing element types");
          Ok (ty, List.map snd l)
        with Err s -> Error s
      in
      ok O.(TArray ty) O.(Array (Array.of_list l))
    | StructLit (((path, (s_name, _)), _), fields) ->
      (* Qualified as the test wrote it, which is how an ordinary read names
         it. *)
      let s_name =
        match path with
        | [] -> s_name
        | p -> String.concat "." (List.map Mark.remove p) ^ "." ^ s_name
      in
      let*? fields =
        try
          Ok
            (List.map
               (fun ((n, _), lit) ->
                 match convert_literal lit with
                 | Error s -> raise (Err s)
                 | Ok (ty, v) -> n, ty, v)
               fields)
        with Err s -> Error s
      in
      let struct_decl =
        {
          O.struct_name = s_name;
          fields = List.map (fun (n, ty, _) -> n, ty) fields;
        }
      in
      ok (O.TStruct struct_decl)
        (O.Struct (struct_decl, List.map (fun (n, _, v) -> n, v) fields))
    | Builtin Impossible -> ok O.TUnset O.Unset
    | Tuple elems ->
      let*? parts =
        try
          Ok
            (List.map
               (fun elem ->
                 match convert_literal elem with
                 | Error s -> raise (Err s)
                 | Ok v -> v)
               elems)
        with Err s -> Error s
      in
      ok
        (O.TTuple (List.map fst parts))
        (O.Array (Array.of_list (List.map snd parts)))
    (* `1 year + 2 month`: how [write] spells a multi-unit duration. Still a
       literal. `+` on anything else is a computation, not a value. *)
    | Binop ((Add _, _), lhs, rhs) -> (
      let*? lt, lv = convert_literal lhs in
      let*? rt, rv = convert_literal rhs in
      match lt, lv.O.value, rt, rv.O.value with
      | O.TDuration, O.Duration a, O.TDuration, O.Duration b ->
        ok O.TDuration
          (O.Duration
             {
               years = a.O.years + b.O.years;
               months = a.O.months + b.O.months;
               days = a.O.days + b.O.days;
             })
      | _ -> Error "unsupported expression")
    | _ -> Error "unsupported expression"
  in
  let convert_definition : definition -> (string * O.test_io, string) Result.t =
    function
    | { definition_condition = Some _; _ } ->
      Error "a conditional definition is not a literal"
    | { definition_exception_to = UnlabeledException | ExceptionToLabel _; _ }
      ->
      Error "an exception is not a literal"
    | { definition_state = Some _; _ } ->
      Error "a state definition is not a literal"
    | { definition_label = Some _; _ } ->
      Error "a labelled definition is not a literal"
    | { definition_parameter = Some _; _ } ->
      Error "a function definition is not a literal"
    | {
        definition_name = [_subscope_id; (input_var_name, _)], _;
        definition_expr = expr;
        _;
      } ->
      let*? typ, rv = convert_literal expr in
      let expr = O.{ typ; value = vlit rv } in
      Ok (input_var_name, expr)
    | _ -> Error "invalid definition shape"
  in
  (* `assertion (calc.total = $0.00)` is an expected output. Only field =
     literal is readable; anything richer refuses the whole test, as the
     ordinary read does -- skipped silently, promoting the working copy would
     delete it. *)
  let rec convert_assertion (e : expression) :
      (string * O.test_io, string) Result.t =
    match Mark.remove e with
    (* `assertion (x = y)` carries its parentheses into the tree. *)
    | Paren inner -> convert_assertion inner
    | Binop ((Eq, _), lhs, rhs) -> (
      match Mark.remove lhs with
      | Dotted (_, ((_path, (field, _)), _)) ->
        let*? typ, rv = convert_literal rhs in
        Ok (field, O.{ typ; value = vlit rv })
      | _ -> Error "unsupported assertion")
    | _ -> Error "unsupported assertion"
  in
  let convert_var_def = function
    | Definition d, _ -> begin
      match convert_definition d with
      | Error s -> raise (Err s)
      | Ok v -> Some v
    end
    | Rule _, _ -> raise (Err "a rule (filled / not filled) is not a literal")
    | DateRounding _, _ -> raise (Err "date rounding is not recoverable")
    | Assertion _, _ -> None
  in
  let*? () =
    match scope_use.scope_use_condition with
    | None -> Ok ()
    | Some _ -> Error "a conditional scope use is not recoverable"
  in
  let*? test_inputs =
    try Ok (List.filter_map convert_var_def scope_use.scope_use_items)
    with Err s -> Error s
  in
  let*? recovered_outputs =
    try
      Ok
        (List.filter_map
           (function
             | Assertion e, _ -> (
               match convert_assertion e with
               | Error s -> raise (Err s)
               | Ok v -> Some v)
             | _ -> None)
           scope_use.scope_use_items)
    with Err s -> Error s
  in
  let*? () =
    match
      List.find_opt
        (fun (f, _) ->
          List.length
            (List.filter (fun (g, _) -> String.equal f g) recovered_outputs)
          > 1)
        recovered_outputs
    with
    | Some (f, _) -> Error (Printf.sprintf "%S is asserted twice" f)
    | None -> Ok ()
  in
  let testing_scope = fst scope_decl.scope_decl_name in
  let*? tested_scope =
    List.find_map
      (function
        | ( ContextScope
              { scope_decl_context_scope_sub_scope = (p, (sname, _)), _; _ },
            _ ) ->
          Some
            (match List.rev p with
            | [] ->
              Error
                (Printf.sprintf "%S does not say which module it tests" sname)
            | (m, _) :: _ ->
              Ok
                {
                  O.name = sname;
                  module_name = m;
                  inputs =
                    List.map
                      (fun (v, (i_io : O.test_io)) ->
                        v, { O.typ = i_io.typ; is_context = false })
                      test_inputs;
                  outputs =
                    List.map
                      (fun (n, (io : O.test_io)) -> n, io.typ)
                      recovered_outputs;
                  module_deps = [];
                })
        | _ -> None)
      scope_decl.scope_decl_context
    |> function None -> Error "tested scope not found" | Some r -> r
  in
  Ok
    {
      O.testing_scope;
      tested_scope;
      test_inputs;
      test_outputs = recovered_outputs;
      description =
        Option.value ~default:""
          (surface_attr_string ["test_description"]
             (Mark.get scope_decl.scope_decl_name));
      title =
        Option.value ~default:""
          (surface_attr_string ["test_title"]
             (Mark.get scope_decl.scope_decl_name));
      variables = [];
    }

(* Each test independently: one failure must not lose the others. *)
let read_partial_tests options : (O.test list * string list, string) Result.t =
  let prg = Driver.Passes.surface options in
  let uses = surface_scope_uses prg in
  match surface_scope_decls prg with
  | [] ->
    (* Usually a syntax error, which leaves no surface AST at all. *)
    Error
      "no test declaration found — if the file has a syntax error, that is the \
       cause: a partial read still needs the file to parse"
  | decls ->
    let results =
      List.filter_map
        (fun ((decl : Surface.Ast.scope_decl), _) ->
          let name = Mark.remove decl.scope_decl_name in
          match
            List.filter
              (fun (u : Surface.Ast.scope_use) ->
                String.equal (Mark.remove u.scope_use_name) name)
              uses
          with
          | [] -> None (* declared but never defined: not a test *)
          | first :: _ as all ->
            (* Catala merges a scope's uses across blocks; so must we. *)
            let use =
              {
                first with
                Surface.Ast.scope_use_items =
                  List.concat_map
                    (fun (u : Surface.Ast.scope_use) -> u.scope_use_items)
                    all;
              }
            in
            Some (name, read_partial_test_one decl use))
        decls
    in
    let tests =
      List.filter_map (function _, Ok t -> Some t | _ -> None) results
    in
    let errors =
      List.filter_map
        (function
          | n, Error e -> Some (Printf.sprintf "%s: %s" n e) | _ -> None)
        results
    in
    if tests = [] && errors <> [] then Error (String.concat "; " errors)
    else Ok (tests, errors)

(* Rebuilding a broken test: one command producing everything the recovery view
   needs; none of this reasoning lives on the TypeScript side. *)

(* Re-describe a value with the live type's declarations: a recovered one is
   inferred from a single literal (one constructor of a hundred, only the fields
   the test wrote, [unknown_enum_name]). Attributes are the value's own and
   stay. Only call on a value that [value_fits] the type: the asserts below hold
   because fitting rejects a value the declaration has no place for, and
   adoption must never be the one to drop it. *)
let rec adopt_typ (t : O.typ) (v : O.runtime_value) : O.runtime_value =
  let value =
    match t, v.O.value with
    | O.TOption inner, O.Enum (_, (ctor, payload)) ->
      O.Enum
        ( mk_optional_enum_decl inner,
          (ctor, Option.map (adopt_typ inner) payload) )
    | O.TEnum d, O.Enum (_, (ctor, payload)) ->
      let payload =
        match List.assoc_opt ctor d.O.constructors with
        | Some (Some pt) -> Option.map (adopt_typ pt) payload
        | Some None ->
          assert (payload = None);
          None
        | None -> assert false
      in
      O.Enum (d, (ctor, payload))
    | O.TStruct d, O.Struct (_, fields) ->
      assert (List.for_all (fun (n, _) -> List.mem_assoc n d.O.fields) fields);
      (* In declaration order, as an ordinary read has them: the same test must
         write the same bytes whichever reader it came through. *)
      let declared =
        List.filter_map
          (fun (n, ft) ->
            Option.map (fun fv -> n, adopt_typ ft fv) (List.assoc_opt n fields))
          d.O.fields
      in
      O.Struct (d, declared)
    | O.TArray et, O.Array elems -> O.Array (Array.map (adopt_typ et) elems)
    | O.TTuple ts, O.Array elems when List.length ts = Array.length elems ->
      O.Array (Array.mapi (fun i e -> adopt_typ (List.nth ts i) e) elems)
    | _ -> v.O.value
  in
  { v with O.value }

(* How a recovered value may be carried into a live slot. Ordered by how much
   each rule claims: [Wrap]/[Unwrap] only that an option and its payload
   correspond, [Fits] only what an ordinary read checks. Anything that infers
   intent belongs behind explicit consent -- a wrong carry is worse than an
   empty field, because nobody re-checks a field that looks answered. Pinned by
   the table in test/carry_test.ml. *)
(* What became of one recovered value: the first four carry one, only
   [Partial] has more to say below it. The wire's [carry_outcome] is the same
   without the value. *)
type carry =
  | Fits of O.runtime_value
  | Wrap of O.runtime_value
  | Unwrap of O.runtime_value
  | Partial of O.runtime_value * (O.path_segment list * O.carry_outcome) list
  | WasUnset
  | WasAbsentNowRequired
  | TypeChanged of O.typ * O.typ

let value_of = function
  | Fits v | Wrap v | Unwrap v | Partial (v, _) -> Some v
  | WasUnset | WasAbsentNowRequired | TypeChanged _ -> None

let outcome_of : carry -> O.carry_outcome = function
  | Fits _ -> O.Fits
  | Wrap _ -> O.Wrap
  | Unwrap _ -> O.Unwrap
  | Partial _ -> O.Partial
  | WasUnset -> O.WasUnset
  | WasAbsentNowRequired -> O.WasAbsentNowRequired
  | TypeChanged (a, b) -> O.TypeChanged (a, b)

let nested_of = function Partial (_, n) -> n | _ -> []

let carry_rule ~(old_typ : O.typ) ~(new_typ : O.typ) (v : O.runtime_value) :
    carry =
  match new_typ, old_typ with
  (* [TUnset] is absence of evidence, not a differing type. First, because
     [value_fits] accepts [Unset] against anything. *)
  | _, O.TUnset -> WasUnset
  (* By fit, not type equality: the recovered type of an enum is narrower than
     the live one. An option never wraps another option. *)
  | O.TOption inner, _
    when (match old_typ with O.TOption _ -> false | _ -> true)
         && v.O.value <> O.Unset
         && value_fits inner v = Ok () ->
    Wrap
      {
        O.value = O.Enum (mk_optional_enum_decl inner, (option_present, Some v));
        attrs = [];
      }
  (* Option to option is neither: the fits check decides. *)
  | _, O.TOption _
    when not (match new_typ with O.TOption _ -> true | _ -> false) -> (
    match v.O.value with
    | O.Enum (_, (_, Some payload)) when value_fits new_typ payload = Ok () ->
      Unwrap payload
    | O.Enum (_, (_, None)) -> WasAbsentNowRequired
    | _ -> TypeChanged (old_typ, new_typ))
  (* Not type equality: a recovered type is inferred from one literal and never
     equals the live one, even when nothing changed. [Fits] promises exactly
     what an ordinary read checks. *)
  | _ when value_fits new_typ v = Ok () -> Fits v
  | _ -> TypeChanged (old_typ, new_typ)

(* For a sentence: [personnes[0].nir]. *)
let path_name (p : O.path_segment list) =
  String.concat ""
    (List.mapi
       (fun i -> function
         | `StructField n -> if i = 0 then n else "." ^ n
         | `ListIndex j | `TupleIndex j -> Printf.sprintf "[%d]" j
         | `EnumPayload _ -> "")
       p)

(* Below a value that does not carry whole, carry what does: field by field,
   element by element, through options, tuples and enum payloads. Nested
   outcomes are paths from the value down; [Fits] and [Partial] are not recorded
   there, the leaves say it all. A value none of whose parts carry stays
   [TypeChanged], so a list of moneys turned dates is one mark, not one per
   element. *)
let rec carry_value ~(old_typ : O.typ) ~(new_typ : O.typ) (v : O.runtime_value)
    : carry =
  match carry_rule ~old_typ ~new_typ v with
  (* A fit can still hold less than the live type declares: a field the old
     record never had is a blank to report, not a fit to pass. *)
  | Fits c -> (
    match carry_inside ~old_typ ~new_typ v with
    | Some (completed, (_ :: _ as nested)) -> Partial (completed, nested)
    | _ -> Fits (adopt_typ new_typ c))
  | Wrap c -> Wrap (adopt_typ new_typ c)
  | Unwrap c -> Unwrap (adopt_typ new_typ c)
  | TypeChanged _ as leaf -> (
    match carry_inside ~old_typ ~new_typ v with
    | Some (c, nested) -> Partial (c, nested)
    | None -> leaf)
  | (Partial _ | WasUnset | WasAbsentNowRequired) as c -> c

and carry_inside ~(old_typ : O.typ) ~(new_typ : O.typ) (v : O.runtime_value) :
    (O.runtime_value * (O.path_segment list * O.carry_outcome) list) option =
  let hole typ = (unset_default_value typ).O.value.O.value in
  let nested = ref [] in
  let push seg outcome = nested := ([seg], outcome) :: !nested in
  let carry_at seg ~old_typ ~new_typ e =
    let r = carry_value ~old_typ ~new_typ e in
    (match r with Fits _ | Partial _ -> () | r -> push seg (outcome_of r));
    nested :=
      List.rev_append
        (List.map (fun (p, o) -> seg :: p, o) (nested_of r))
        !nested;
    value_of r
  in
  (* A payload is a node of its own, named by its constructor as diff paths name
     it; only the enum itself is marked when the payload does not carry. *)
  let carry_payload ctor ~old_typ ~new_typ e =
    let r = carry_value ~old_typ ~new_typ e in
    let seg = `EnumPayload ctor in
    nested :=
      List.rev_append
        (List.map (fun (p, o) -> seg :: p, o) (nested_of r))
        !nested;
    value_of r
  in
  let indexed mk_seg ots nts elems =
    let carried =
      List.mapi
        (fun i (ot, nt) ->
          i, nt, carry_at (mk_seg i) ~old_typ:ot ~new_typ:nt elems.(i))
        (List.combine ots nts)
    in
    if List.for_all (fun (_, _, c) -> c = None) carried then None
    else
      Some
        (Array.of_list
           (List.map
              (fun (i, nt, c) ->
                Option.value c ~default:{ (elems.(i)) with O.value = hole nt })
              carried))
  in
  let result =
    match old_typ, new_typ, v.O.value with
    | O.TStruct od, O.TStruct nd, O.Struct (_, fields)
      when same_type_name ~recovered:od.O.struct_name ~live:nd.O.struct_name ->
      let any_carried = ref false in
      let carried =
        List.map
          (fun (n, nt) ->
            let seg = `StructField n in
            match List.assoc_opt n fields with
            | None ->
              push seg O.WasUnset;
              n, { O.value = hole nt; attrs = [] }
            | Some fv -> (
              let ot =
                Option.value (List.assoc_opt n od.O.fields) ~default:nt
              in
              match carry_at seg ~old_typ:ot ~new_typ:nt fv with
              | Some c ->
                any_carried := true;
                n, c
              | None -> n, { fv with O.value = hole nt }))
          nd.O.fields
      in
      List.iter
        (fun (n, _) ->
          if not (List.mem_assoc n nd.O.fields) then
            push (`StructField n) O.Dropped)
        fields;
      if !any_carried then Some (O.Struct (nd, carried)) else None
    | O.TArray oe, O.TArray ne, O.Array elems ->
      let n = Array.length elems in
      Option.map
        (fun a -> O.Array a)
        (indexed
           (fun i -> `ListIndex i)
           (List.init n (fun _ -> oe))
           (List.init n (fun _ -> ne))
           elems)
    | O.TTuple ots, O.TTuple nts, O.Array elems
      when List.length ots = List.length nts
           && List.length nts = Array.length elems ->
      Option.map
        (fun a -> O.Array a)
        (indexed (fun i -> `TupleIndex i) ots nts elems)
    | O.TOption oi, O.TOption ni, O.Enum (_, (ctor, Some p)) ->
      Option.map
        (fun c -> O.Enum (mk_optional_enum_decl ni, (ctor, Some c)))
        (carry_payload ctor ~old_typ:oi ~new_typ:ni p)
    | O.TEnum od, O.TEnum nd, O.Enum (_, (ctor, Some p))
      when same_type_name ~recovered:od.O.enum_name ~live:nd.O.enum_name -> (
      match List.assoc_opt ctor nd.O.constructors with
      | Some (Some npt) ->
        let opt =
          match List.assoc_opt ctor od.O.constructors with
          | Some (Some t) -> t
          | _ -> npt
        in
        Option.map
          (fun c -> O.Enum (nd, (ctor, Some c)))
          (carry_payload ctor ~old_typ:opt ~new_typ:npt p)
      | _ -> None)
    (* Across an option boundary: the payload is the value, or becomes it. *)
    | _, O.TOption ni, _
      when match old_typ with O.TOption _ -> false | _ -> true ->
      Option.map
        (fun c -> O.Enum (mk_optional_enum_decl ni, (option_present, Some c)))
        (carry_payload option_present ~old_typ ~new_typ:ni v)
    | O.TOption oi, _, O.Enum (_, (_, Some p)) ->
      let r = carry_value ~old_typ:oi ~new_typ p in
      nested := List.rev_append (nested_of r) !nested;
      Option.map (fun (c : O.runtime_value) -> c.O.value) (value_of r)
    | _ -> None
  in
  Option.map (fun value -> { v with O.value }, List.rev !nested) result

(* Scopes anywhere in the project that declare some of the test's field names,
   for a test whose module is gone. Surface-parsed only: no module need compile,
   and none but the one chosen ever will. *)
let rank_project_scope_candidates
    ~(from_dir : string)
    ~(wanted_module : string)
    ~(field_names : string list) : O.scope_candidate list =
  let total = List.length field_names in
  let mentions_a_field content =
    List.exists (fun f -> Re.execp (Re.compile (Re.str f)) content) field_names
  in
  let candidates_of (it : Scan.item) =
    match it.Scan.module_def with
    | None -> []
    | Some module_def -> (
      let module_name = Mark.remove module_def in
      let file = it.Scan.file_name in
      match File.contents file with
      | exception _ -> []
      | content when not (mentions_a_field content) -> []
      | _ -> (
        match
          Driver.Passes.surface
            (Global.enforce_options ~input_src:(Global.FileName file)
               ~language:(Some (Cli.file_lang file))
               ())
        with
        | exception e ->
          Message.debug "candidates: skipping %s: %s" file
            (Printexc.to_string e);
          []
        | prg ->
          List.filter_map
            (fun ((decl : Surface.Ast.scope_decl), _) ->
              let declared =
                List.filter_map
                  (fun (item, _) ->
                    match item with
                    | Surface.Ast.ContextData d ->
                      Some (Mark.remove d.scope_decl_context_item_name)
                    | Surface.Ast.ContextScope _ -> None)
                  decl.scope_decl_context
              in
              let shared =
                List.length
                  (List.filter (fun n -> List.mem n declared) field_names)
              in
              if shared = 0 then None
              else
                Some
                  {
                    O.module_name;
                    name = Mark.remove decl.scope_decl_name;
                    shared;
                    out_of = total;
                  })
            (surface_scope_decls prg)))
  in
  scan_catala_files (project_root from_dir)
  |> List.concat_map candidates_of
  |> List.sort (fun (a : O.scope_candidate) (b : O.scope_candidate) ->
      match compare b.shared a.shared with
      | 0 -> (
        match
          compare
            (Suggestions.levenshtein_distance wanted_module a.module_name)
            (Suggestions.levenshtein_distance wanted_module b.module_name)
        with
        | 0 -> compare (a.module_name, a.name) (b.module_name, b.name)
        | c -> c)
      | c -> c)
  |> List.filteri (fun i _ -> i < 8)

(* The module's scopes, ranked by shared field names then name distance. Ranked,
   not chosen: the tester picks. Same scopes as [list_scopes]. *)
let rank_scope_candidates
    (prg : I.program)
    ~(wanted : string)
    ~(field_names : string list) : O.scope_candidate list =
  let tested_module =
    match prg.I.program_module_name with
    | Some (m, _) -> m
    | None -> ModuleName.fresh ("no_module", Pos.void)
  in
  let total = List.length field_names in
  ScopeName.Map.fold
    (fun _ (sc : I.scope) acc ->
      match sc.I.scope_visibility with
      | Private -> acc
      | _ -> (
        match get_scope_def prg sc ~tested_module with
        | exception _ -> acc
        | def ->
          let declared =
            List.map fst def.O.inputs @ List.map fst def.O.outputs
          in
          let shared =
            List.length (List.filter (fun n -> List.mem n declared) field_names)
          in
          {
            O.module_name = ModuleName.to_string tested_module;
            name = def.O.name;
            shared;
            out_of = total;
          }
          :: acc))
    prg.I.program_root.module_scopes []
  |> List.sort (fun (a : O.scope_candidate) (b : O.scope_candidate) ->
      match compare b.shared a.shared with
      | 0 -> (
        match
          compare
            (Suggestions.levenshtein_distance wanted a.name)
            (Suggestions.levenshtein_distance wanted b.name)
        with
        | 0 -> compare a.name b.name
        | c -> c)
      | c -> c)

(* Not a Catala extension, so no scan sees it; matches TS workingCopyExt. *)
let working_copy_ext = ".repair"

let rebuild_broken_test
    (options : Global.options)
    (target : string option)
    buffer_path =
  let options =
    match options.Global.input_src, buffer_path with
    | Global.Stdin _, Some b ->
      Global.enforce_options
        ~input_src:(Global.Contents (In_channel.input_all stdin, b))
        ()
    | _ -> options
  in
  let test_file = Global.input_src_file options.Global.input_src in
  let lang = Cli.file_lang test_file in
  let notes = ref [] in
  let note n = notes := n :: !notes in
  let workspace_file = Filename.basename test_file ^ working_copy_ext in
  let emit tests =
    write_stdout J.write_recovery
      { O.tests; notes = List.rev !notes; working_copy = workspace_file }
  in
  let emit_bare recovered =
    emit
      (List.map
         (fun (t : O.test) -> { O.authored = t; rebuilt = None; outcomes = [] })
         recovered)
  in
  (match surface_scopes_by_ownership (Driver.Passes.surface options) with
  | _ :: _, (_ :: _ as unowned) -> error_mixed_ownership unowned
  | _ -> ());
  match read_partial_tests options with
  | Error e -> Format.ksprintf failwith "Error: %s" e
  | Ok (recovered, errors) -> (
    List.iter (fun e -> Message.warning "partial read: %s" e) errors;
    match recovered with
    | [] -> emit []
    | first :: _ -> (
      let scope = first.O.tested_scope in
      let workspace =
        Filename.concat (Filename.dirname test_file) workspace_file
      in
      (* A working copy the scope drifted under again is recovered like the
         original: [drifted], merged below once the live scope is known. *)
      let saved, drifted =
        match read_tests_of_file ~lang workspace with
        | Ok s -> s, []
        | Error e -> (
          match
            read_partial_tests
              (Global.enforce_options ~input_src:(Global.FileName workspace)
                 ~language:(Some lang) ())
          with
          | Ok ((_ :: _ as tests), _) -> None, tests
          | Ok ([], _) | Error _ | (exception _) ->
            note
              (O.WorkingCopyUnreadable { O.name = workspace_file; error = e });
            None, [])
      in
      (* Explicit choice, else the scope a saved working copy targets, else the
         declared one. *)
      let target_module, target_name =
        match target, saved, drifted with
        | Some t, _, _ -> parse_target t
        | None, Some (t :: _), _ | None, _, t :: _ ->
          Some t.O.tested_scope.O.module_name, t.O.tested_scope.O.name
        | None, _, _ -> None, scope.O.name
      in
      let module_name =
        Option.value target_module ~default:scope.O.module_name
      in
      let field_names =
        List.map fst first.O.test_inputs @ List.map fst first.O.test_outputs
      in
      let from_dir = Filename.dirname test_file in
      match find_module_file module_name from_dir with
      | None ->
        note
          (O.ModuleNotFound
             {
               O.module_name;
               candidates =
                 rank_project_scope_candidates ~from_dir
                   ~wanted_module:module_name ~field_names;
             });
        emit_bare recovered
      | Some module_file -> (
        (* Point the compiler at the module, not at the test that no longer
           typechecks. *)
        let module_options =
          Global.enforce_options ~input_src:(Global.FileName module_file) ()
        in
        (* Two distinct notes: a renamed scope is not a module that will not
           build. *)
        match
          let path_to_build, include_dirs =
            lookup_include_dirs module_options
          in
          read_program include_dirs path_to_build module_options
        with
        | exception e ->
          note
            (O.ModuleWontCompile { O.name = module_name; error = error_text e });
          emit_bare recovered
        | prg, _
          when not (Ident.Map.mem target_name prg.I.program_ctx.ctx_scope_index)
          ->
          note
            (O.ScopeNotFound
               {
                 O.module_name;
                 scope_name = target_name;
                 candidates =
                   rank_scope_candidates prg ~wanted:target_name ~field_names;
               });
          emit_bare recovered
        | program -> (
          match generate_test target_name ~program [] module_options with
          | exception e ->
            note
              (O.Other
                 {
                   O.name = module_name ^ "." ^ target_name;
                   error = error_text e;
                 });
            emit_bare recovered
          | live ->
            let pair_of (t : O.test) : O.recovered_test =
              let carried = ref [] in
              let record side path outcome =
                carried := { O.path; side; outcome; hint = [] } :: !carried
              in
              (* One entry per live field. [when_missing]: an absent input was
                 left unset; an absent output was never asserted. *)
              let carry_record
                  ~io
                  ~when_missing
                  (old_record : (string * O.test_io) list)
                  (live_record : (string * O.test_io) list) =
                List.map
                  (fun (name, (live_io : O.test_io)) ->
                    match List.assoc_opt name old_record with
                    | Some { O.value = Some vd; typ = old_typ } -> (
                      let field = `StructField name in
                      let r =
                        carry_value ~old_typ ~new_typ:live_io.typ vd.O.value
                      in
                      record io [field] (outcome_of r);
                      List.iter
                        (fun (p, o) -> record io (field :: p) o)
                        (nested_of r);
                      match value_of r with
                      | Some v ->
                        ( name,
                          {
                            live_io with
                            O.value = Some { O.value = v; pos = None };
                          } )
                      | None -> name, live_io)
                    | _ ->
                      (* A context var never overridden is not damage. *)
                      let defaulting =
                        match live_io.O.value with
                        | Some { O.value = { O.value = O.NotOverridden; _ }; _ }
                          ->
                          true
                        | _ -> false
                      in
                      if not defaulting then
                        Option.iter (record io [`StructField name]) when_missing;
                      name, live_io)
                  live_record
              in
              let mark_dropped
                  ~io
                  (old_record : (string * O.test_io) list)
                  (live_record : (string * O.test_io) list) =
                List.iter
                  (fun (name, (old_io : O.test_io)) ->
                    if
                      old_io.O.value <> None
                      && not (List.mem_assoc name live_record)
                    then record io [`StructField name] O.Dropped)
                  old_record
              in
              let rebuilt =
                {
                  live with
                  O.testing_scope = t.O.testing_scope;
                  title = t.O.title;
                  description = t.O.description;
                  test_inputs =
                    carry_record ~io:O.In ~when_missing:(Some O.WasUnset)
                      t.O.test_inputs live.O.test_inputs;
                  test_outputs =
                    carry_record ~io:O.Out ~when_missing:None t.O.test_outputs
                      live.O.test_outputs;
                }
              in
              mark_dropped ~io:O.In t.O.test_inputs live.O.test_inputs;
              mark_dropped ~io:O.Out t.O.test_outputs live.O.test_outputs;
              (* Where a dropped input's value would fit a field of a live
                 record that needs filling, say so on that record. *)
              let dropped =
                List.filter_map
                  (fun (r : O.carry_record) ->
                    match r.O.side, r.O.path, r.O.outcome with
                    | O.In, [`StructField n], O.Dropped -> (
                      match List.assoc_opt n t.O.test_inputs with
                      | Some { O.value = Some vd; _ } -> Some (n, vd.O.value)
                      | _ -> None)
                    | _ -> None)
                  !carried
              in
              let rec fields_of : O.typ -> (string * O.typ) list = function
                | O.TStruct d -> d.O.fields
                | O.TOption t | O.TArray t -> fields_of t
                | _ -> []
              in
              let with_hint (r : O.carry_record) =
                match r.O.side, r.O.path, r.O.outcome with
                | ( O.In,
                    [`StructField n],
                    (O.WasUnset | O.TypeChanged _ | O.Partial) ) -> (
                  match List.assoc_opt n live.O.test_inputs with
                  | None -> r
                  | Some (live_io : O.test_io) ->
                    let fields = fields_of live_io.O.typ in
                    let hint =
                      List.filter_map
                        (fun (d, v) ->
                          match List.assoc_opt d fields with
                          | Some ft when value_fits ft v = Ok () -> Some d
                          | _ -> None)
                        dropped
                    in
                    { r with O.hint })
                | _ -> r
              in
              {
                O.authored = t;
                rebuilt = Some rebuilt;
                outcomes = List.rev_map with_hint !carried;
              }
            in
            (* The drifted working copy wins, except where its own value could
               not follow the scope: there, what the original still carries. *)
            let saved =
              match drifted with
              | [] -> saved
              | _ ->
                let lost = ref [] in
                let merged =
                  List.filter_map
                    (fun (orig : O.test) ->
                      match
                        List.find_opt
                          (fun (w : O.test) ->
                            w.O.testing_scope = orig.O.testing_scope)
                          drifted
                      with
                      | None -> None
                      | Some w -> (
                        let from_w = pair_of w and from_o = pair_of orig in
                        match from_w.O.rebuilt, from_o.O.rebuilt with
                        | Some rw, Some ro ->
                          let not_followed (r : O.carry_record) =
                            match r.O.outcome with
                            | O.WasUnset | O.TypeChanged _
                            | O.WasAbsentNowRequired ->
                              true
                            | _ -> false
                          in
                          List.iter
                            (fun (r : O.carry_record) ->
                              match r.O.outcome with
                              | O.Dropped | O.TypeChanged _
                              | O.WasAbsentNowRequired ->
                                lost := path_name r.O.path :: !lost
                              | _ -> ())
                            from_w.O.outcomes;
                          let pick side mine theirs =
                            List.map
                              (fun (name, io) ->
                                if
                                  List.exists
                                    (fun (r : O.carry_record) ->
                                      r.O.side = side
                                      && r.O.path = [`StructField name]
                                      && not_followed r)
                                    from_w.O.outcomes
                                then
                                  ( name,
                                    Option.value
                                      (List.assoc_opt name theirs)
                                      ~default:io )
                                else name, io)
                              mine
                          in
                          Some
                            {
                              rw with
                              O.test_inputs =
                                pick O.In rw.O.test_inputs ro.O.test_inputs;
                              test_outputs =
                                pick O.Out rw.O.test_outputs ro.O.test_outputs;
                            }
                        | _ -> None))
                    recovered
                in
                note
                  (O.WorkingCopyRecovered
                     {
                       O.name = workspace_file;
                       lost = List.sort_uniq String.compare !lost;
                     });
                Some merged
            in
            (* A mark is answered once its field holds a value; [Dropped]
               never. *)
            let with_saved (pair : O.recovered_test) : O.recovered_test =
              match saved with
              | None | Some [] -> pair
              | Some saved -> (
                match
                  List.find_opt
                    (fun (s : O.test) ->
                      s.O.testing_scope = pair.O.authored.O.testing_scope)
                    saved
                with
                | None -> pair
                | Some s ->
                  let rec at (v : O.runtime_value) = function
                    | [] -> Some v
                    | `EnumPayload c :: rest -> (
                      match v.O.value with
                      | O.Enum (_, (c', Some p)) when c' = c -> at p rest
                      | _ -> None)
                    | `StructField n :: rest -> (
                      match v.O.value with
                      | O.Struct (_, fs) ->
                        Option.bind (List.assoc_opt n fs) (fun v -> at v rest)
                      | _ -> None)
                    | (`ListIndex i | `TupleIndex i) :: rest -> (
                      match v.O.value with
                      | O.Array a when i < Array.length a -> at a.(i) rest
                      | _ -> None)
                    | _ -> None
                  in
                  let still_blank side path =
                    let record =
                      match side with
                      | O.In -> s.O.test_inputs
                      | O.Out -> s.O.test_outputs
                    in
                    match path with
                    | `StructField field :: rest -> (
                      match
                        List.assoc_opt field record
                        |> Option.fold ~none:None ~some:(fun (io : O.test_io) ->
                            io.value)
                      with
                      | None -> true
                      | Some vd -> (
                        match at vd.O.value rest with
                        | None -> true
                        | Some v -> v.O.value = O.Unset))
                    | _ -> true
                  in
                  {
                    pair with
                    O.rebuilt = Some s;
                    outcomes =
                      List.filter
                        (fun (r : O.carry_record) ->
                          match r.O.outcome with
                          | O.WasUnset | O.TypeChanged _
                          | O.WasAbsentNowRequired ->
                            still_blank r.O.side r.O.path
                          | _ -> true)
                        pair.O.outcomes;
                  })
            in
            emit (List.map (fun t -> with_saved (pair_of t)) recovered)))))

let read_partial_test (options : Global.options) =
  match read_partial_tests options with
  | Ok (tests, errors) ->
    List.iter (fun e -> Message.warning "partial read: %s" e) errors;
    write_stdout J.write_test_list tests
  | Error s -> Message.error "%s" s
