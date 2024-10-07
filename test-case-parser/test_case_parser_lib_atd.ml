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

module O = Test_case_t

exception Unsupported of string
let unsupported fmt =
  Format.ksprintf (fun msg -> raise (Unsupported msg)) fmt

let get_typ_literal = function
  | TBool -> O.TBool
  | TUnit -> raise (Unsupported "unit type")
  | TInt -> O.TInt
  | TRat -> O.TRat
  | TMoney -> O.TMoney
  | TDate -> O.TDate
  | TDuration -> O.TDuration

let rec get_typ decl_ctx = function
  | TLit tlit, _ -> get_typ_literal tlit
  | TTuple tl, _ -> O.TTuple (List.map (get_typ decl_ctx) tl)
  | TStruct name, _ -> O.TStruct (get_struct decl_ctx name)
  | TEnum name, _ -> O.TEnum (get_enum decl_ctx name)
  | TOption ty, _ -> O.TOption (get_typ decl_ctx ty)
  | TArray ty, _ -> O.TArray (get_typ decl_ctx ty)
  | TArrow _, _ -> raise (Unsupported "function type")
  | TDefault _, _ -> raise (Unsupported "default type")
  | TAny, _ -> raise (Unsupported "wildcard type")
  | TClosureEnv, _ -> raise (Unsupported "closure type")

and get_struct decl_ctx struct_name =
  let fields_map = StructName.Map.find struct_name decl_ctx.ctx_structs in
  let fields =
    List.map (fun (field, typ) ->
        StructField.to_string field,
        get_typ decl_ctx typ)
      (StructField.Map.bindings fields_map)
  in
  {
    O.struct_name = StructName.to_string struct_name;
    fields
  }

and get_enum decl_ctx enum_name =
  let constr_map = EnumName.Map.find enum_name decl_ctx.ctx_enums in
  let constructors =
    List.map (fun (constr, typ) ->
        EnumConstructor.to_string constr,
        match typ with
        | TLit TUnit, _ -> None
        | _ -> Some (get_typ decl_ctx typ))
      (EnumConstructor.Map.bindings constr_map)
  in
  {
    O.enum_name = EnumName.to_string enum_name;
    constructors;
  }

let rec get_value decl_ctx = function
  | ELit (LBool b), _ -> O.Bool b
  | ELit (LInt i), _ -> O.Integer (Z.to_int i)
  | ELit (LRat r), _ -> O.Decimal (Q.to_float r)
  | ELit (LMoney m), _ -> O.Money (Z.to_int m)
  | ELit (LDate t), _ ->
    let year, month, day = Dates_calc.Dates.date_to_ymd t in
    O.Date { year; month; day }
  | ELit (LDuration dt), _ ->
    let years, months, days = Dates_calc.Dates.period_to_ymds dt in
    O.Duration { years; months; days }
  | EAppOp { op = Op.Add, _; args = [e1; e2]; tys = [TLit TDuration, _; TLit TDuration, _] }, _ as e ->
    (match get_value decl_ctx e1, get_value decl_ctx e2 with
     | O.Duration { years = y1; months = m1; days = d1 },
       O.Duration { years = y2; months = m2; days = d2 } ->
       O.Duration { years = y1 + y2; months = m1 + m2; days = d1 + d2 }
     | _ -> Message.error ~pos:(Expr.pos e) "Invalid duration literal.")
  | EArray args, _ ->
    O.Array (Array.of_list (List.map (get_value decl_ctx) args))
  | EStruct { name; fields }, _ ->
    O.Struct
      ( get_struct decl_ctx name,
        List.map
          (fun (field, v) ->
            StructField.to_string field, get_value decl_ctx v)
          (StructField.Map.bindings fields) )
  | EInj { name; e = ELit LUnit, _; cons }, _ ->
    O.Enum
      ( get_enum decl_ctx name,
        (EnumConstructor.to_string cons, None)
      )
  | EInj { name; e; cons }, _ ->
    O.Enum
      ( get_enum decl_ctx name,
        (EnumConstructor.to_string cons, Some (get_value decl_ctx e))
      )
  | e ->
    Message.error ~pos:(Expr.pos e)
      "This test value is not a literal."

let get_source_position pos = {
  O.filename = Pos.get_file pos;
  start_line = Pos.get_start_line pos;
  start_column = Pos.get_start_column pos;
  end_line = Pos.get_end_line pos;
  end_column = Pos.get_end_column pos;
  law_headings = Pos.get_law_info pos;
}

let scope_inputs decl_ctx scope =
  I.ScopeDef.Map.fold (fun ((v, _pos), _kind) sdef acc ->
      match sdef.I.scope_def_io.I.io_input with
      | Runtime.NoInput, _ -> acc
      | Runtime.OnlyInput, _ ->
        (ScopeVar.to_string v, get_typ decl_ctx sdef.I.scope_def_typ) :: acc
      | Runtime.Reentrant, _ ->
        (ScopeVar.to_string v, get_typ decl_ctx sdef.I.scope_def_typ) :: acc
    )
    scope.I.scope_defs []
  |> List.rev

let get_scope_def decl_ctx (sc: I.scope): O.scope_def =
  let info = ScopeName.Map.find sc.scope_uid decl_ctx.ctx_scopes in
  {
    O.name = ScopeName.to_string sc.scope_uid;
    inputs = scope_inputs decl_ctx sc;
    outputs = (get_struct decl_ctx info.out_struct_name).fields;
  }

let get_scope_test (prg : I.program) (testing_scope: string) (tested_scope: ScopeName.t): O.test =
  let tested_scope =
    let modul =
      List.fold_left
        (fun _ m -> ModuleName.Map.find m prg.program_modules)
        prg.program_root
        (ScopeName.path tested_scope)
    in
    get_scope_def prg.program_ctx
      (ScopeName.Map.find tested_scope modul.module_scopes)
  in
  let test_inputs =
    List.map
      (fun (v, typ) -> v, { O.typ; value = None })
      tested_scope.inputs
  in
  let test_outputs =
    List.map
      (fun (v, typ) -> v, { O.typ; value = None })
      tested_scope.outputs
  in
  {
    O.testing_scope;
    tested_scope;
    test_outputs;
    test_inputs;
  }

(* --- *)

let write_stdout f arg =
  let buf = Buffer.create 4096 in
  f buf arg;
  Buffer.output_buffer stdout buf

let print_test test =
  write_stdout Test_case_j.write_test test

let read_program includes options =
  Driver.Passes.desugared options ~includes

let generate_test tested_scope ?(testing_scope=tested_scope^"_test") include_dirs options =
  let prg, _ = read_program include_dirs options in
  let tested_scope =
    Ident.Map.find tested_scope prg.I.program_ctx.ctx_scope_index
  in
  let test = get_scope_test prg testing_scope tested_scope in
  print_test test

exception InvalidTestingScope of string
let invalid_testing_scope fmt =
  Format.kasprintf (fun msg -> raise (InvalidTestingScope msg)) fmt

let import_catala_tests (prg, naming_ctx) =
  let decl_ctx = prg.I.program_ctx in
  let root_module = prg.I.program_root in
  let scopes =
    let re_test = Re.(compile (str "_test")) in
    root_module.module_scopes
    |> ScopeName.Map.filter (fun name _ ->
        Re.execp re_test (ScopeName.base name))
    |> ScopeName.Map.bindings
  in
  List.map
    (fun (testing_scope_name, testing_scope) ->
      let subscope_var, tested_scope =
        let count = ScopeVar.Map.cardinal testing_scope.I.scope_sub_scopes in
        if count <> 1 then
          invalid_testing_scope "@{<b>%a@}: testing scopes are expected to have one, and only one subscope, this has %d" ScopeName.format testing_scope_name count
        else ScopeVar.Map.choose testing_scope.scope_sub_scopes
      in
      let tested_id_var_map =
        Ident.Map.filter_map (fun _ -> function
          | ScopeVar v -> Some v
          | SubScope _ -> None)
          (ScopeName.Map.find tested_scope naming_ctx.Desugared.Name_resolution.scopes).var_idmap
      in
      let base_test =
        get_scope_test prg (ScopeName.to_string testing_scope_name) tested_scope
      in
      let test_inputs =
        List.map (fun (var_str, test_in) ->
            let var_within_origin_scope =
              Ident.Map.find var_str tested_id_var_map
            in
            let value =
              let rules =
                try
                  let def_key =
                    (subscope_var, Pos.no_pos),
                    I.ScopeDef.SubScopeInput {
                      name = tested_scope;
                      var_within_origin_scope;
                    }
                  in
                  let def =
                    I.ScopeDef.Map.find def_key testing_scope.scope_defs
                  in
                  RuleName.Map.bindings def.scope_def_rules
                with Ident.Map.Not_found _ | I.ScopeDef.Map.Not_found _ ->
                  []
              in
              match rules with
              | [] -> None
              | [_, rule] ->
                let e = Expr.unbox_closed rule.rule_cons in
                let value = get_value decl_ctx e in
                Some { O.value; pos = Some (get_source_position (Expr.pos e)) }
              | rules ->
                let extra_pos =
                  List.map (fun (r, _) -> "", Mark.get (RuleName.get_info r))
                    rules
                in
                Message.error ~extra_pos
                  "Multiple definitions of test input value in test \
                   scope %a.%a!;@ %d rule(s) found: [%a]"
                  ScopeName.format testing_scope_name ScopeVar.format
                  var_within_origin_scope
                  (List.length rules)
                  (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
                     (fun ppf (r, _) -> RuleName.format ppf r))
                  rules
            in
            var_str, { test_in with O.value })
          base_test.test_inputs
      in
      let test_outputs =
        let scope_info = ScopeName.Map.find tested_scope decl_ctx.ctx_scopes in
        let scope_field_map =
          ScopeVar.Map.fold (fun var field acc ->
            StructField.Map.add field var acc)
            scope_info.out_struct_fields
            StructField.Map.empty
        in
        let assertion_values =
          I.AssertionName.Map.fold
            (fun _ e acc ->
               match Expr.unbox_closed e with
               | EAppOp {
                   op = Op.Eq, _;
                   args = [
                     EStructAccess {
                       field;
                       e =
                         ELocation
                           (DesugaredScopeVar { name = svar, pos; _ }), _;
                       _
                     }, _;
                     value];
                   _ }, _
                 when svar = subscope_var ->
                 let scope_var = StructField.Map.find field scope_field_map in
                 ScopeVar.Map.add scope_var { O.value = get_value decl_ctx value; pos = Some (get_source_position pos)} acc
               | EAppOp {
                   op = Op.Eq, _;
                   args = [
                      EStructAccess _, _ as e;
                     _value];
                   _ }, m ->
                 Message.error ~pos:(Expr.mark_pos m)
                   "X Could not read test assertion: %a" Expr.format e
               | _, m as e ->
                 Message.error ~pos:(Expr.mark_pos m)
                   "Could not read test assertion: %a" Expr.format e
            )
            testing_scope.scope_assertions
            ScopeVar.Map.empty
        in
        List.map (fun (var_str, test_out) ->
            let var =
              Ident.Map.find var_str tested_id_var_map
            in
            let value =
              ScopeVar.Map.find_opt var assertion_values
            in
            var_str, { test_out with O.value })
          base_test.test_outputs
      in
      { base_test with O.test_inputs;
                       test_outputs; })
    scopes

let read_test include_dirs options =
  let prg = read_program include_dirs options in
  let tests = import_catala_tests prg in
  write_stdout Test_case_j.write_test_list tests

(* let test_case_parser includes options : unit =
 *   let prg, _type_ordering = 
 *   let test_file = desugared_program_to_test_file prg in
 *   let test_file_json = test_file_to_yojson test_file in
 *   Format.printf "%s\n" (Yojson.Safe.to_string test_file_json) *)

let rec print_catala_value ppf =
  let open Format in
  function
  | O.Bool b -> pp_print_bool ppf b
  | O.Money m -> fprintf ppf "$%01d.%02d" (m/100) (m mod 100)
  | O.Integer i -> pp_print_int ppf i
  | O.Decimal f -> pp_print_float ppf f
  | O.Date { year; month; day } ->
    fprintf ppf "|%04d-%02d-%02d|" year month day
  | O.Duration { years = 0; months = 0; days = 0 } ->
    pp_print_string ppf "0 day"
  | O.Duration { years; months; days } ->
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " +@ ")
      (fun ppf t -> t ppf)
      ppf
      (List.filter_map Fun.id
         [ if years > 0 then Some (fun ppf -> fprintf ppf "%d year" years) else None;
           if months > 0 then Some (fun ppf -> fprintf ppf "%d month" months) else None;
           if days > 0 then Some (fun ppf -> fprintf ppf "%d day" days) else None; ])
  | O.Enum (_, (constr, Some v)) ->
    fprintf ppf "@[<hv 2>%s content %a@]" constr print_catala_value v
  | O.Enum (_, (constr, None)) ->
    pp_print_string ppf constr
  | O.Struct (st, fields) ->
    fprintf ppf "@[<hv 2>%s {@ %a@;<1 -2>}@]" st.struct_name
      (pp_print_list
        ~pp_sep:pp_print_space
         (fun ppf (fld, v) -> fprintf ppf "-- %s: %a@," fld print_catala_value v))
      fields
  | O.Array vl ->
    fprintf ppf "@[<hov 1>[%a]@]"
      (pp_print_seq ~pp_sep:(fun ppf () -> fprintf ppf ";@ ") print_catala_value)
      (Array.to_seq vl)

let write_catala_test ppf t =
  let open Format in
  let open O in
  let tested_module, sscope_var =
    let modl, sname = match Filename.extension t.tested_scope.name with
      | "" -> None, t.tested_scope.name
      | s -> Some (Filename.remove_extension t.tested_scope.name), String.sub s 1 (String.length s - 1)
    in
    modl, String.to_snake_case sname
  in
  pp_open_vbox ppf 0;
  Option.iter (fun m -> fprintf ppf "> Using %s@," m) tested_module;
  fprintf ppf "@,```catala@,";
  fprintf ppf "@[<v 2>declaration scope %s:@," t.testing_scope;
  fprintf ppf "output %s scope %s@," sscope_var t.tested_scope.name;
  fprintf ppf "@]@,";
  fprintf ppf "@[<v 2>scope %s:" t.testing_scope;
  List.iter (fun (tvar, t_in) ->
      match t_in.value with
      | None -> ()
      | Some { value; _ } ->
        fprintf ppf "@,@[<hv 2>definition %s.%s equals@ %a@]"
          sscope_var tvar print_catala_value value;)
    t.test_inputs;
  List.iter (fun (tvar, t_out) ->
      match t_out.value with
      | None -> ()
      | Some { value; _ } ->
        fprintf ppf "@,assertion (@[<hv>%s.%s =@ %a)@]"
          sscope_var tvar print_catala_value value;)
    t.test_outputs;
  fprintf ppf "@]@,```@,"

let write_catala options outfile =
  let tests = Test_case_j.read_test_list (Yojson.init_lexer ()) (Lexing.from_channel stdin) in
  let _fname, with_out =
    File.get_formatter_of_out_channel ()
      ~source_file:(Global.Stdin "")
      ~output_file:(Option.map options.Global.path_rewrite outfile)
  in
  with_out @@ fun oc -> List.iter (write_catala_test oc) tests

let run_test _include_dirs _options = failwith "TODO"
