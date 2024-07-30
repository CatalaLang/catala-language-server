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
open Stdlib
open Catala_utils
open Shared_ast
open Desugared.Ast

type test_input_value = {
  value : Runtime_ocaml.Runtime.runtime_value Mark.pos;
  typ : naked_typ;
}

type test = {
  test_scope_name : ScopeName.t;
  scope_being_tested : ScopeName.t;
  test_inputs : test_input_value ScopeVar.Map.t;
}

let rec runtime_value_to_yojson (v : Runtime_ocaml.Runtime.runtime_value) :
    Yojson.Safe.t =
  let typ, value =
    match v with
    | Unit -> "null", `Null
    | Bool b -> "bool", `Bool b
    | Money m -> "money", `Float (Runtime.money_to_float m)
    | Integer i -> "integer", `Int (Runtime.integer_to_int i)
    | Decimal d -> "decimal", `Float (Runtime.decimal_to_float d)
    | Date d ->
      let y, m, d = Dates_calc.Dates.date_to_ymd d in
      "date", `String (Format.asprintf "%4d-%2d-%2d" y m d)
    | Duration d ->
      let y, m, d = Dates_calc.Dates.period_to_ymds d in
      "duration", `Assoc ["years", `Int y; "months", `Int m; "days", `Int d]
    | Enum (enum_name, (enum_case, payload)) ->
      ( "enum",
        `Assoc
          [
            "enum_name", `String enum_name;
            "enum_case", `String enum_case;
            "enum_payload", runtime_value_to_yojson payload;
          ] )
    | Struct (struct_name, fields) ->
      ( "struct",
        `Assoc
          [
            "struct_name", `String struct_name;
            ( "fields",
              `List
                (List.map
                   (fun (field_name, field_value) ->
                     `Assoc
                       [
                         "field_name", `String field_name;
                         "field_value", runtime_value_to_yojson field_value;
                       ])
                   fields) );
          ] )
    | Array args ->
      "array", `List (List.map runtime_value_to_yojson (Array.to_list args))
    | Unembeddable -> "unembeddable", `Null
  in
  `Assoc ["typ", `String typ; "value", value]

let pos_to_yojson (p : Pos.t) : Yojson.Safe.t =
  `Assoc
    [
      "start_line", `Int (Pos.get_start_line p);
      "start_column", `Int (Pos.get_start_column p);
      "end_line", `Int (Pos.get_end_line p);
      "end_column", `Int (Pos.get_end_column p);
      "filename", `String (Pos.get_file p);
      "law_info", `List (List.map (fun x -> `String x) (Pos.get_law_info p));
    ]

let rec typ_to_yosjon (t : naked_typ) : Yojson.Safe.t =
  let typ_category, info =
    match t with
    | TLit TBool -> "literal", `String "bool"
    | TLit TUnit -> "literal", `String "unit"
    | TLit TInt -> "literal", `String "integer"
    | TLit TRat -> "literal", `String "decimal"
    | TLit TMoney -> "literal", `String "money"
    | TLit TDate -> "literal", `String "date"
    | TLit TDuration -> "literal", `String "duration"
    | TArray (t, _) -> "array", typ_to_yosjon t
    | TStruct name -> "struct", `String (StructName.to_string name)
    | TEnum name -> "enum", `String (EnumName.to_string name)
    | ty ->
      Message.error "type %a not supported for translation !"
        Type.format (Mark.add Pos.no_pos ty)
  in
  `Assoc ["typ_category", `String typ_category; "info", info]

let test_to_yojson (t : test) : Yojson.Safe.t =
  `Assoc
    [
      "test_scope_name", `String (ScopeName.to_string t.test_scope_name);
      "scope_being_tested", `String (ScopeName.to_string t.scope_being_tested);
      ( "test_inputs",
        `Assoc
          (List.map
             (fun (scope_input_var, test_input_value) ->
               ( ScopeVar.to_string scope_input_var,
                 `Assoc
                   [
                     ( "value",
                       runtime_value_to_yojson
                         (Mark.remove test_input_value.value) );
                     "pos", pos_to_yojson (Mark.get test_input_value.value);
                     "typ", typ_to_yosjon test_input_value.typ;
                   ] ))
             (ScopeVar.Map.bindings t.test_inputs)) );
    ]

type test_file_item = Test of test [@to_yojson: test_to_yojson]
[@@deriving to_yojson] [@@warning "-37"]

type test_file = test_file_item list [@@deriving to_yojson]

let rec expr_to_runtime_value (e : (desugared, untyped) naked_gexpr) :
    Runtime.runtime_value =
  match e with
  | ELit (LBool b) -> Runtime.Bool b
  | ELit (LInt i) -> Runtime.Integer i
  | ELit (LRat r) -> Runtime.Decimal r
  | ELit (LMoney m) -> Runtime.Money m
  | ELit LUnit -> Runtime.Unit
  | ELit (LDate d) -> Runtime.Date d
  | ELit (LDuration d) -> Runtime.Duration d
  | EArray args ->
    Runtime.Array
      (Array.of_list
         (List.map (fun a -> expr_to_runtime_value (Mark.remove a)) args))
  | EStruct { name; fields } ->
    let fields = StructField.Map.bindings fields in
    Runtime.Struct
      ( StructName.to_string name,
        List.map
          (fun (f_name, v) ->
            StructField.to_string f_name, expr_to_runtime_value (Mark.remove v))
          fields )
  | EInj { name; e; cons } ->
    Runtime.Enum
      ( EnumName.to_string name,
        (EnumConstructor.to_string cons, expr_to_runtime_value (Mark.remove e))
      )
  | e ->
    Message.error "A test input value is not a literal! (found: %a)" Expr.format (Mark.add (Untyped { pos = Pos.no_pos }) e)

let desugared_program_to_test_file (prg : program) : test_file =
  let root_module = prg.program_root in
  let scopes = ScopeName.Map.bindings root_module.module_scopes in
  List.map
    (fun (scope_name, scope) ->
      let subscope_var, scope_being_tested =
        if ScopeVar.Map.cardinal scope.scope_sub_scopes <> 1 then
          Message.error "Multiple or no sub-scopes in test scope %a!"
            ScopeName.format scope_name
        else ScopeVar.Map.choose scope.scope_sub_scopes
      in
      let test_inputs =
        let scope_defs = ScopeDef.Map.bindings scope.scope_defs in
        let subscope_inputs =
          List.filter_map
            (fun ((scope_def_var, scope_def_kind), scope_def) ->
              match scope_def_kind with
              | ScopeDef.SubScopeInput { name = _; var_within_origin_scope }
                when ScopeVar.equal subscope_var (Mark.remove scope_def_var) ->
                let typ = Mark.remove scope_def.scope_def_typ in
                let value =
                  let rules = scope_def.scope_def_rules in
                  if RuleName.Map.cardinal rules <> 1 then
                    Message.error
                      "Multiple or no definition of a test input value in \
                       test scope %a.%a!"
                      ScopeName.format scope_name
                      ScopeVar.format (Mark.remove scope_def_var)
                  else
                    let _, rule = RuleName.Map.choose rules in
                    let e = Expr.unbox_closed rule.rule_cons in
                    expr_to_runtime_value (Mark.remove e), Expr.pos e
                in
                Some (var_within_origin_scope, { value; typ })
              | _ -> None)
            scope_defs
        in
        ScopeVar.Map.of_list subscope_inputs
      in
      let test =
        { test_scope_name = scope_name; scope_being_tested; test_inputs }
      in
      Test test)
    scopes

let test_case_parser includes options : unit =
  let prg, _type_ordering = Driver.Passes.desugared options ~includes in
  let test_file = desugared_program_to_test_file prg in
  let test_file_json = test_file_to_yojson test_file in
  Format.printf "Test test:\n%s" (Yojson.Safe.show test_file_json)

let term =
  let open Cmdliner.Term in
  const test_case_parser $ Cli.Flags.include_dirs

(* For now, can be invoked through `catala test-case-parser --plugin-dir
   _build/default/ <FILE>` but we need to figure out distribution through vscode
   or otherwise. *)
let () = Driver.Plugin.register "test-case-parser" term

(* Test command : dune build && catala test-case-parser --plugin-dir
   _build/default/
   ~/catala-examples/aides_logement/tests/tests_calcul_al_locatif.catala_fr -I
   ~/catala-examples/aides_logement/ -I ~/catala-examples/prologue_france -I
   ~/catala-examples/allocations_familiales -I
   ~/catala-examples/prestations_familiales -I ~/catala-examples/smic -I
   ~/catala-examples/base_mensuelle_allocations_familiales *)
