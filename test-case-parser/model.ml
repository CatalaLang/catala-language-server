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

type lang_strings = {
  declaration_scope : string;
  output_scope : string;
  using_module : string;
  (* The first line of every file the editor writes: it owns the file, and says
     so where a reader of the source will look first. *)
  header : string;
  definition : string;
  assertion : string;
  equals : string;
  content : string;
  scope : string;
  present : string;
  absent : string;
}

let get_lang_strings =
  let fr_strings =
    {
      declaration_scope = "déclaration champ d'application";
      output_scope = "résultat";
      using_module = "Usage de";
      header =
        "Fichier écrit par l’éditeur de tests métier Catala. Ne pas modifier à \
         la main.";
      definition = "définition";
      assertion = "assertion";
      equals = "égal à";
      content = "contenu";
      scope = "champ d'application";
      present = "Présent";
      absent = "Absent";
    }
  in
  let en_strings =
    {
      declaration_scope = "declaration scope";
      output_scope = "output";
      using_module = "Using";
      header = "Written by the Catala testcase editor. Do not edit by hand.";
      definition = "definition";
      assertion = "assertion";
      equals = "equals";
      content = "content";
      scope = "scope";
      present = "Present";
      absent = "Absent";
    }
  in
  let pl_strings =
    {
      declaration_scope = "deklaracja zakres";
      output_scope = "wyjście";
      using_module = "Using";
      definition = "definicja";
      assertion = "asercja";
      equals = "wynosi";
      content = "typu";
      scope = "zakres";
      present = "Obecny";
      absent = "Nieobecny";
      header =
        "Plik zapisany przez edytor przypadków testowych Catala. Nie edytować \
         ręcznie.";
    }
  in
  function `Fr -> fr_strings | `En -> en_strings | `Pl -> pl_strings

(* Runtime names, as an ordinary read produces them and the editor's option form
   expects them. The surface keyword is the writer's business. *)
let option_enum_name = EnumName.to_string ConstantNames.option_enum
let option_absent = EnumConstructor.to_string ConstantNames.none_constr
let option_present = EnumConstructor.to_string ConstantNames.some_constr

let mk_optional_enum_decl typ =
  {
    O.enum_name = option_enum_name;
    constructors = [option_absent, None; option_present, Some typ];
    ctor_attrs = [];
  }

let mk_absent typ =
  O.Enum
    ( mk_optional_enum_decl typ,
      (EnumConstructor.to_string ConstantNames.none_constr, None) )

let get_typ_literal = function
  | TBool -> O.TBool
  | TUnit -> raise (Unsupported "unit type")
  | TInt -> O.TInt
  | TRat -> O.TRat
  | TMoney -> O.TMoney
  | TDate -> O.TDate
  | TDuration -> O.TDuration
  | TPos -> raise (Unsupported "position type")

let rec get_typ lang decl_ctx = function
  | TLit tlit, _ -> get_typ_literal tlit
  | TTuple tl, _ -> O.TTuple (List.map (get_typ lang decl_ctx) tl)
  | TStruct name, _ -> O.TStruct (get_struct lang decl_ctx name)
  | TEnum name, _ -> O.TEnum (get_enum lang decl_ctx name)
  | TOption ty, _ -> O.TOption (get_typ lang decl_ctx ty)
  | TArray ty, _ -> O.TArray (get_typ lang decl_ctx ty)
  | TArrow _, _ -> raise (Unsupported "function type")
  | TDefault _, _ -> raise (Unsupported "default type")
  | TForAll _, _ -> raise (Unsupported "wildcard type")
  | TVar _, _ -> raise (Unsupported "type variable")
  | TClosureEnv, _ -> raise (Unsupported "closure type")
  | TError, _ -> raise (Unsupported "error type")
  | TAbstract _, _ -> raise (Unsupported "abstract type")

and get_struct lang decl_ctx struct_name =
  let fields_map = StructName.Map.find struct_name decl_ctx.ctx_structs in
  let module_name =
    if StructName.path struct_name = [] then None
    else
      let module_name =
        List.rev (StructName.path struct_name)
        |> List.hd
        |> ModuleName.to_string
      in
      let alias_opt = lookup_aliased_name lang module_name in
      Option.(some (value alias_opt ~default:module_name))
  in
  let fields =
    List.map
      (fun (field, typ) ->
        StructField.to_string field, get_typ lang decl_ctx typ)
      (StructField.Map.bindings fields_map)
  in
  let struct_name =
    match module_name with
    | None -> StructName.base struct_name
    | Some s -> Format.asprintf "%s.%s" s (StructName.base struct_name)
  in
  { O.struct_name; fields }

and enum_ctor_attrs constr_map =
  List.filter_map
    (fun (constr, _) ->
      let pos = EnumConstructor.get_info constr |> snd in
      let attrs =
        Pos.get_attrs pos (function
          | Description s -> Some (O.Description s)
          | _ -> None)
      in
      if attrs = [] then None else Some (EnumConstructor.to_string constr, attrs))
    (EnumConstructor.Map.bindings constr_map)

and get_enum (lang : Global.backend_lang) (decl_ctx : decl_ctx) enum_name =
  let constr_map = EnumName.Map.find enum_name decl_ctx.ctx_enums in
  if EnumName.equal enum_name ConstantNames.option_enum then
    let typ =
      let x =
        EnumConstructor.Map.bindings constr_map
        |> List.find_map (function
          | _, (TLit TUnit, _) -> None
          | _, typ -> Some (get_typ lang decl_ctx typ))
      in
      x |> Option.get
    in
    mk_optional_enum_decl typ
  else
    let module_name =
      if EnumName.path enum_name = [] then None
      else
        let module_name =
          List.rev (EnumName.path enum_name) |> List.hd |> ModuleName.to_string
        in
        let alias_opt = lookup_aliased_name lang module_name in
        Option.(some (value alias_opt ~default:module_name))
    in
    let bindings = EnumConstructor.Map.bindings constr_map in
    let constructors =
      List.map
        (fun (constr, typ) ->
          ( EnumConstructor.to_string constr,
            match typ with
            | TLit TUnit, _ -> None
            | _ -> Some (get_typ lang decl_ctx typ) ))
        bindings
    in
    let ctor_attrs = enum_ctor_attrs constr_map in
    let enum_name =
      match module_name with
      | None -> EnumName.base enum_name
      | Some s -> Format.asprintf "%s.%s" s (EnumName.base enum_name)
    in
    { O.enum_name; constructors; ctor_attrs }

type Pos.attr += TestUi
type Pos.attr += Uid of string
type Pos.attr += TestDescription of string
type Pos.attr += TestTitle of string
type Pos.attr += ArrayItemLabel of string
type Pos.attr += ExpectedVariable of string

let rec get_value : type a.
    Global.backend_lang -> decl_ctx -> (a, 'm) gexpr -> O.runtime_value =
 fun lang decl_ctx e ->
  let pos = Expr.pos e in
  let attrs =
    Pos.get_attrs pos (function
      | Uid s -> Some (O.Uid s)
      | ArrayItemLabel s -> Some (O.ArrayItemLabel s)
      | _ -> None)
  in
  let value =
    match Mark.remove e with
    | ELit (LBool b) -> O.Bool b
    | ELit (LInt i) -> O.Integer (Z.to_int i)
    | ELit (LRat r) -> O.Decimal (Q.to_float r)
    | ELit (LMoney m) -> O.Money (Z.to_int m)
    | ELit (LDate t) ->
      let year, month, day = Dates_calc.date_to_ymd t in
      O.Date { year; month; day }
    | ELit (LDuration dt) ->
      let years, months, days = Dates_calc.period_to_ymds dt in
      O.Duration { years; months; days }
    | EAppOp
        {
          op = Op.Add, _;
          args = [e1; e2];
          tys = [(TLit TDuration, _); (TLit TDuration, _)];
        } -> (
      match
        (get_value lang decl_ctx e1).value, (get_value lang decl_ctx e2).value
      with
      | ( O.Duration { years = y1; months = m1; days = d1 },
          O.Duration { years = y2; months = m2; days = d2 } ) ->
        O.Duration { years = y1 + y2; months = m1 + m2; days = d1 + d2 }
      | _ -> Message.error ~pos "Invalid duration literal.")
    | EArray args ->
      O.Array (Array.of_list (List.map (get_value lang decl_ctx) args))
    | EStruct { name; fields } ->
      O.Struct
        ( get_struct lang decl_ctx name,
          List.map
            (fun (field, v) ->
              StructField.to_string field, get_value lang decl_ctx v)
            (StructField.Map.bindings fields) )
    | EInj { name; e; _ } when EnumName.equal ConstantNames.option_enum name
      -> (
      match Typing.expr decl_ctx e |> Expr.unbox with
      | ELit LUnit, _ty ->
        let none_field =
          EnumConstructor.to_string ConstantNames.none_constr, None
        in
        let decl =
          {
            O.enum_name = EnumName.to_string ConstantNames.option_enum;
            constructors = [none_field];
            ctor_attrs = [];
          }
        in
        O.Enum (decl, none_field)
      | _, Typed { ty; _ } ->
        let some_field =
          let ty =
            match ty with
            | TForAll _, _ ->
              (* e.g., while reading 'Present content impossible' *) O.TUnset
            | ty -> get_typ lang decl_ctx ty
          in
          EnumConstructor.to_string ConstantNames.some_constr, Some ty
        in
        let some_value =
          ( EnumConstructor.to_string ConstantNames.some_constr,
            Some (get_value lang decl_ctx e) )
        in
        let decl =
          {
            O.enum_name = EnumName.to_string ConstantNames.option_enum;
            constructors = [some_field];
            ctor_attrs = [];
          }
        in
        O.Enum (decl, some_value))
    | EInj { name; e = ELit LUnit, _; cons } ->
      O.Enum
        (get_enum lang decl_ctx name, (EnumConstructor.to_string cons, None))
    | EInj { name; e; cons } ->
      O.Enum
        ( get_enum lang decl_ctx name,
          (EnumConstructor.to_string cons, Some (get_value lang decl_ctx e)) )
    | EFatalError Impossible -> O.Unset
    | EEmpty -> O.Empty
    | _ ->
      Message.error ~pos "This test value is not a literal: %a." Expr.format e
  in
  { O.value; attrs }

let get_source_position pos =
  {
    O.filename = Pos.get_file pos;
    start_line = Pos.get_start_line pos;
    start_column = Pos.get_start_column pos;
    end_line = Pos.get_end_line pos;
    end_column = Pos.get_end_column pos;
    law_headings = Pos.get_law_info pos;
  }

let scope_inputs lang decl_ctx scope =
  I.ScopeDef.Map.fold
    (fun ((v, _pos), kind) sdef acc ->
      match kind with
      | SubScopeInput _ -> acc
      | Var _ -> (
        match fst sdef.I.scope_def_io.I.io_input with
        | Catala_runtime.NoInput -> acc
        | Catala_runtime.OnlyInput ->
          ( ScopeVar.to_string v,
            O.
              {
                typ = get_typ lang decl_ctx sdef.I.scope_def_typ;
                is_context = false;
              } )
          :: acc
        | Catala_runtime.Reentrant ->
          ( ScopeVar.to_string v,
            O.
              {
                typ = get_typ lang decl_ctx sdef.I.scope_def_typ;
                is_context = true;
              } )
          :: acc))
    scope.I.scope_defs []
  |> List.rev

let retrieve_scope_module_deps (prg : I.program) (scope : I.scope) =
  let decl_ctx = prg.program_ctx in
  let filtered_input_typs : typ list =
    I.ScopeDef.Map.fold
      (fun (_, kind) (sdef : I.scope_def) acc ->
        (* Do not consider subscopes *)
        match kind with
        | I.ScopeDef.SubScopeInput _ -> acc
        | I.ScopeDef.Var _ ->
          let is_input =
            match Mark.remove sdef.scope_def_io.io_input with
            | NoInput -> false
            | OnlyInput | Reentrant -> true
          in
          let is_output = Mark.remove sdef.scope_def_io.io_output in
          if is_input || is_output then sdef.I.scope_def_typ :: acc else acc)
      scope.I.scope_defs []
    |> List.rev
  in
  let rec process_typ (acc : ModuleName.Set.t) ty =
    match Mark.remove ty with
    | TLit _ -> acc
    | TTuple tl -> List.fold_left process_typ acc tl
    | TStruct sname ->
      let p = StructName.path sname in
      let acc = ModuleName.Set.add_seq (List.to_seq p) acc in
      let sfields = StructName.Map.find sname decl_ctx.ctx_structs in
      StructField.Map.fold (fun _ ty acc -> process_typ acc ty) sfields acc
    | TEnum ename ->
      let p = EnumName.path ename in
      let acc = ModuleName.Set.add_seq (List.to_seq p) acc in
      let scases = EnumName.Map.find ename decl_ctx.ctx_enums in
      EnumConstructor.Map.fold (fun _ ty acc -> process_typ acc ty) scases acc
    | TOption ty -> process_typ acc ty
    | TArray ty -> process_typ acc ty
    | TArrow _ -> raise (Unsupported "function type")
    | TDefault _ -> raise (Unsupported "default type")
    | TForAll _ -> raise (Unsupported "wildcard type")
    | TVar _ -> raise (Unsupported "type variable")
    | TClosureEnv -> raise (Unsupported "closure type")
    | TError -> raise (Unsupported "error type")
    | TAbstract _ -> raise (Unsupported "abstract type")
  in
  List.fold_left process_typ ModuleName.Set.empty filtered_input_typs
  |> ModuleName.Set.elements
  |> List.map ModuleName.to_string

let get_scope_def (prg : I.program) (sc : I.scope) ~tested_module : O.scope_def
    =
  let lang = prg.program_lang in
  let decl_ctx = prg.program_ctx in
  let module_name = ModuleName.to_string tested_module in
  let info = ScopeName.Map.find sc.scope_uid decl_ctx.ctx_scopes in
  {
    O.name = ScopeName.base sc.scope_uid;
    module_name;
    inputs = scope_inputs lang decl_ctx sc;
    outputs = (get_struct lang decl_ctx info.out_struct_name).fields;
    module_deps = retrieve_scope_module_deps prg sc;
  }

(** Default placeholder for uninitialized inputs: empty array for TArray, Absent
    for TOption, explicit Unset for everything else. *)
let unset_default_value (typ : O.typ) : O.value_def =
  let value =
    match typ with
    | TArray _ -> { O.value = O.Array [||]; attrs = [] }
    | TOption typ -> { O.value = mk_absent typ; attrs = [] }
    | _ -> { O.value = O.Unset; attrs = [] }
  in
  { O.value; pos = None }

(** For context variables, use [NotOverridden] regardless of type. Context
    variables have a scope-computed default. [NotOverridden] means "no override
    — let the scope compute its own value". The field is omitted from the JSON
    input sent to the runtime and from the rendered Catala test. This differs
    from [unset_default_value], which uses [Array [||]] for array types: that
    would generate an explicit [definition x = []] override. *)
let context_var_default : O.value_def =
  { O.value = { O.value = O.NotOverridden; attrs = [] }; pos = None }

(* A bare constructor names no enum. Never a name; never printed as one. *)
let unknown_enum_name = "unknown"

(* Names as the literal wrote them, which may omit or alias the module: only the
   type's own name is compared. *)
let same_type_name ~recovered ~live =
  let last n =
    match String.rindex_opt n '.' with
    | Some i -> String.sub n (i + 1) (String.length n - i - 1)
    | None -> n
  in
  recovered = unknown_enum_name || last recovered = last live

(* Does [v] still inhabit [t]? (recovery of drifted tests). *)
let rec value_fits (t : O.typ) (v : O.runtime_value) : (unit, string) Result.t =
  let mismatch expected got =
    Error (Printf.sprintf ": expected %s, got %s" expected got)
  in
  let under seg = Result.map_error (( ^ ) seg) in
  match t, v.O.value with
  (* Value-less, so nothing to check. *)
  | _, (O.Unset | O.NotOverridden) -> Ok ()
  (* The empty default exists only in run results and diffs. *)
  | _, O.Empty -> mismatch "a value" "empty"
  | O.TBool, O.Bool _
  | O.TInt, O.Integer _
  | O.TRat, O.Decimal _
  | O.TMoney, O.Money _
  | O.TDate, O.Date _
  | O.TDuration, O.Duration _
  | O.TUnit, _
  | O.TUnset, _
  | O.TArrow _, _ ->
    Ok ()
  (* An option is an enum in the runtime, so it is checked as one below. *)
  | O.TOption ot, O.Enum (_, (ctor, payload)) -> (
    match payload with
    | None when ctor = option_absent -> Ok ()
    | Some p when ctor = option_present ->
      under (Printf.sprintf ".%s" ctor) (value_fits ot p)
    | _ ->
      mismatch
        (Printf.sprintf "%s or %s" option_absent option_present)
        (Printf.sprintf "%s%s" ctor
           (match payload with None -> "" | Some _ -> " with a payload")))
  | O.TEnum d, O.Enum (rd, _)
    when not (same_type_name ~recovered:rd.O.enum_name ~live:d.O.enum_name) ->
    mismatch d.O.enum_name rd.O.enum_name
  | O.TEnum d, O.Enum (_, (ctor, payload)) -> (
    match List.assoc_opt ctor d.O.constructors, payload with
    | None, _ ->
      mismatch
        (Printf.sprintf "one of %s"
           (String.concat " | " (List.map fst d.O.constructors)))
        (Printf.sprintf "%s.%s" d.O.enum_name ctor)
    | Some None, None -> Ok ()
    | Some None, Some _ ->
      mismatch
        (Printf.sprintf "bare %s" ctor)
        (Printf.sprintf "%s with a payload" ctor)
    | Some (Some _), None ->
      mismatch
        (Printf.sprintf "%s with a payload" ctor)
        (Printf.sprintf "bare %s" ctor)
    | Some (Some pt), Some p ->
      under (Printf.sprintf ".%s" ctor) (value_fits pt p))
  | O.TStruct d, O.Struct (rd, _)
    when not (same_type_name ~recovered:rd.O.struct_name ~live:d.O.struct_name)
    ->
    mismatch d.O.struct_name rd.O.struct_name
  | O.TStruct d, O.Struct (_, fields) -> (
    let declared =
      List.fold_left
        (fun acc (fname, ft) ->
          match acc with
          | Error _ -> acc
          | Ok () -> (
            match List.assoc_opt fname fields with
            | None -> Ok () (* absent field: nothing to contradict the type *)
            | Some fv -> under ("." ^ fname) (value_fits ft fv)))
        (Ok ()) d.O.fields
    in
    match declared with
    | Error _ -> declared
    | Ok () -> (
      match
        List.find_opt (fun (n, _) -> not (List.mem_assoc n d.O.fields)) fields
      with
      | None -> Ok ()
      | Some (n, _) ->
        Error (Printf.sprintf ".%s: not a field of %s" n d.O.struct_name)))
  | O.TArray et, O.Array elems ->
    let rec go i =
      if i >= Array.length elems then Ok ()
      else
        match under (Printf.sprintf "[%d]" i) (value_fits et elems.(i)) with
        | Ok () -> go (i + 1)
        | e -> e
    in
    go 0
  | O.TTuple ts, O.Array elems when List.length ts = Array.length elems ->
    let rec go i = function
      | [] -> Ok ()
      | et :: rest -> (
        match under (Printf.sprintf "[%d]" i) (value_fits et elems.(i)) with
        | Ok () -> go (i + 1) rest
        | e -> e)
    in
    go 0 ts
  | _ -> mismatch (typ_name t) (value_name v.O.value)

and typ_name : O.typ -> string = function
  | O.TBool -> "boolean"
  | O.TInt -> "integer"
  | O.TRat -> "decimal"
  | O.TMoney -> "money"
  | O.TDate -> "date"
  | O.TDuration -> "duration"
  | O.TUnit -> "unit"
  | O.TUnset -> "unset"
  | O.TTuple ts -> Printf.sprintf "a %d-tuple" (List.length ts)
  | O.TStruct d -> d.O.struct_name
  | O.TEnum d -> d.O.enum_name
  | O.TOption t -> "optional of " ^ typ_name t
  | O.TArray t -> "list of " ^ typ_name t
  | O.TArrow _ -> "function"

and value_name : O.runtime_value_raw -> string = function
  | O.Bool _ -> "a boolean"
  | O.Money _ -> "money"
  | O.Integer _ -> "an integer"
  | O.Decimal _ -> "a decimal"
  | O.Date _ -> "a date"
  | O.Duration _ -> "a duration"
  | O.Enum (d, _) -> d.O.enum_name
  | O.Struct (d, _) -> d.O.struct_name
  | O.Array _ -> "a list"
  | O.Unset -> "no value"
  | O.NotOverridden -> "a default"
  | O.Empty -> "empty"

(* "Scope" or "Module.Scope". *)
let parse_target (t : string) : string option * string =
  match String.rindex_opt t '.' with
  | Some i ->
    Some (String.sub t 0 i), String.sub t (i + 1) (String.length t - i - 1)
  | None -> None, t

type duration_units = { day : string; month : string; year : string }

type value_strings = {
  true_str : string;
  false_str : string;
  money_fmt : (int -> int -> unit, Format.formatter, unit) format;
  decimal_sep : char;
  content_str : string;
  duration_units : duration_units;
  present : string;
  absent : string;
}

let get_value_strings =
  let fr_strings =
    {
      true_str = "vrai";
      false_str = "faux";
      money_fmt = format_of_string "%01d,%02d €";
      decimal_sep = ',';
      content_str = "contenu";
      duration_units = { day = "jour"; month = "mois"; year = "an" };
      present = "Présent";
      absent = "Absent";
    }
  in
  let en_strings =
    {
      true_str = "true";
      false_str = "false";
      money_fmt = format_of_string "$%01d.%02d";
      decimal_sep = '.';
      content_str = "content";
      duration_units = { day = "day"; month = "month"; year = "year" };
      present = "Present";
      absent = "Absent";
    }
  in
  let pl_strings =
    {
      true_str = "prawda";
      false_str = "fałsz";
      money_fmt = format_of_string "%01d.%02d PLN";
      decimal_sep = '.';
      content_str = "typu";
      duration_units = { day = "dzień"; month = "miesiąc"; year = "rok" };
      present = "Obecny";
      absent = "Nieobecny";
    }
  in
  function `Fr -> fr_strings | `En -> en_strings | `Pl -> pl_strings

let print_attrs ppf (attrs : O.attr_def list) =
  let open Format in
  pp_print_list
    (fun ppf (attr : O.attr_def) ->
      match attr with
      | Uid s -> fprintf ppf "#[testcase.uid = \"%s\"]@\n" s
      | ArrayItemLabel s ->
        fprintf ppf "#[testcase.array_item_label = \"%s\"]@\n" s
      (* TODO error out if we come across TestDescription or TestTitle? *)
      | _ -> ())
    ppf attrs

let rec print_catala_value ~(typ : O.typ option) ~lang ppf (v : O.runtime_value)
    =
  let open Format in
  let strings = get_value_strings lang in
  print_attrs ppf v.attrs;
  match typ, v.value with
  | _, O.Unset -> pp_print_string ppf "impossible"
  | _, O.NotOverridden -> assert false (* filtered before printing *)
  | _, O.Bool b ->
    pp_print_string ppf (if b then strings.true_str else strings.false_str)
  | _, O.Money m ->
    let major = abs m / 100 in
    let minor = abs m mod 100 in
    if m < 0 then fprintf ppf "-";
    fprintf ppf strings.money_fmt major minor
  | _, O.Integer i -> pp_print_int ppf i
  | _, O.Decimal f ->
    let s = sprintf "%g" f in
    let s = if String.contains s '.' then s else sprintf "%.1f" f in
    pp_print_string ppf
      (String.map (function '.' -> strings.decimal_sep | c -> c) s)
  | _, O.Date { year; month; day } ->
    fprintf ppf "|%04d-%02d-%02d|" year month day
  | _, O.Duration { years = 0; months = 0; days = 0 } ->
    fprintf ppf "0 %s" strings.duration_units.day
  | _, O.Duration { years; months; days } ->
    pp_print_list
      ~pp_sep:(fun ppf () -> fprintf ppf " +@ ")
      (fun ppf t -> t ppf)
      ppf
      (List.filter_map Fun.id
         [
           (if years <> 0 then
              Some
                (fun ppf ->
                  fprintf ppf "%d %s" years strings.duration_units.year)
            else None);
           (if months <> 0 then
              Some
                (fun ppf ->
                  fprintf ppf "%d %s" months strings.duration_units.month)
            else None);
           (if days <> 0 then
              Some
                (fun ppf -> fprintf ppf "%d %s" days strings.duration_units.day)
            else None);
         ])
  | _, O.Enum ({ enum_name; constructors; _ }, (constr, v))
    when enum_name = option_enum_name ->
    if v = None then pp_print_string ppf strings.absent
    else
      let payload_typ =
        match typ with
        | Some (O.TOption inner) -> Some inner
        | _ -> Option.join (List.assoc_opt constr constructors)
      in
      fprintf ppf "%s %s %a" strings.present strings.content_str
        (print_catala_value ~typ:payload_typ ~lang)
        (Option.get v)
  | Some (TEnum { enum_name; constructors; _ }), O.Enum (_en, (constr, Some v))
    when enum_name <> unknown_enum_name ->
    fprintf ppf "@[<hv 2>%s.%s %s %a@]" enum_name constr strings.content_str
      (print_catala_value
         ~typ:(Option.join (List.assoc_opt constr constructors))
         ~lang)
      v
  (* Name unknown: written bare, for Catala to infer as it did the first
     time. *)
  | _, O.Enum (_, (constr, Some v)) ->
    fprintf ppf "@[<hv 2>%s %s %a@]" constr strings.content_str
      (print_catala_value ~typ:None ~lang)
      v
  | Some (TEnum { enum_name; _ }), O.Enum (_en, (constr, None))
    when enum_name <> unknown_enum_name ->
    fprintf ppf "%s.%s" enum_name constr
  | _, O.Enum (_, (constr, None)) -> pp_print_string ppf constr
  (* By name: a recovered value has only the fields its test wrote, in the
     test's order. *)
  | Some (O.TStruct sdecl), O.Struct (st, fields) ->
    fprintf ppf "@[<hv 2>%s {@ %a@;<1 -2>}@]" st.struct_name
      (pp_print_list ~pp_sep:pp_print_space (fun ppf (fld, v) ->
           fprintf ppf "-- %s: %a" fld
             (print_catala_value ~typ:(List.assoc_opt fld sdecl.O.fields) ~lang)
             v))
      fields
  | _, O.Struct (st, fields) ->
    fprintf ppf "@[<hv 2>%s {@ %a@;<1 -2>}@]" st.struct_name
      (pp_print_list ~pp_sep:pp_print_space (fun ppf (fld, v) ->
           fprintf ppf "-- %s: %a" fld (print_catala_value ~typ:None ~lang) v))
      fields
  | Some (O.TArray t), O.Array vl ->
    fprintf ppf "@[<hov 1>[%a]@]"
      (pp_print_seq
         ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
         (print_catala_value ~typ:(Some t) ~lang))
      (Array.to_seq vl)
  | _, O.Array vl ->
    fprintf ppf "@[<hov 1>[%a]@]"
      (pp_print_seq
         ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
         (print_catala_value ~typ:None ~lang))
      (Array.to_seq vl)
  | _, O.Empty -> pp_print_string ppf "impossible"

let print_catala_value_opt ~lang ppf (t_in : O.test_io) =
  let typ = t_in.typ in
  match t_in.O.value, typ with
  | Some { value = { value = O.Unset; _ }; _ }, TArray _ | None, TArray _ ->
    Format.fprintf ppf "[]"
  | Some { value = { value = O.Unset; _ }; _ }, _ | None, _ ->
    Format.fprintf ppf "impossible"
  | Some { value; _ }, typ -> print_catala_value ~typ:(Some typ) ~lang ppf value

let string_of_runtime_value ~lang (v : O.runtime_value) : string =
  match v.O.value with
  | O.Bool b -> if b then "true" else "false"
  | O.Integer i -> string_of_int i
  | O.Decimal f ->
    let s = Printf.sprintf "%.12f" f in
    let len = ref (String.length s) in
    while !len > 1 && s.[!len - 1] = '0' do
      decr len
    done;
    let s = String.sub s 0 !len in
    if s.[String.length s - 1] = '.' then s ^ "0" else s
  | O.Money m -> (
    let major = abs m / 100 and minor = abs m mod 100 in
    let sign = if m < 0 then "-" else "" in
    match lang with
    | `En -> Printf.sprintf "%s$%d.%02d" sign major minor
    | _ -> Printf.sprintf "%s%d,%02d €" sign major minor)
  | O.Date { year; month; day } ->
    Printf.sprintf "%04d-%02d-%02d" year month day
  | O.Duration { years; months; days } ->
    Printf.sprintf "%dy %dm %dd" years months days
  | O.Enum (_, (ctor, _)) -> ctor
  | _ -> ""

let runtime_value_of_string (s : string) : O.runtime_value =
  let enum ctor =
    O.Enum
      ( { O.enum_name = "Optional"; constructors = []; ctor_attrs = [] },
        (ctor, None) )
  in
  let money_of s =
    let mk n =
      match
        float_of_string_opt
          (String.trim (String.map (function ',' -> '.' | c -> c) n))
      with
      | Some f -> Some (O.Money (int_of_float (Float.round (f *. 100.))))
      | None -> None
    in
    if String.contains s '$' then
      mk (String.concat "" (String.split_on_char '$' s))
    else
      let euro = "€" in
      let ls = String.length s and le = String.length euro in
      if ls >= le && String.sub s (ls - le) le = euro then
        mk (String.sub s 0 (ls - le))
      else None
  in
  let scan fmt f = try Some (Scanf.sscanf s fmt f) with _ -> None in
  let raw =
    match s with
    | "true" -> O.Bool true
    | "false" -> O.Bool false
    | "Absent" | "--" -> enum "Absent"
    | s -> (
      match money_of s with
      | Some m -> m
      | None -> (
        match int_of_string_opt s with
        | Some i -> O.Integer i
        | None -> (
          match scan "%d-%d-%d%!" (fun y m d -> y, m, d) with
          | Some (year, month, day) -> O.Date { year; month; day }
          | None -> (
            match scan "%dy %dm %dd%!" (fun y m d -> y, m, d) with
            | Some (years, months, days) -> O.Duration { years; months; days }
            | None -> (
              match float_of_string_opt s with
              | Some f -> O.Decimal f
              | None -> enum s)))))
  in
  { O.value = raw; attrs = [] }

let write_catala_test ppf t lang =
  let open Format in
  let open O in
  let strings = get_lang_strings lang in
  let sscope_var =
    (* The scope part of a possibly qualified name, like [parse_target]. *)
    String.to_snake_case (snd (parse_target t.tested_scope.name))
  in
  pp_open_vbox ppf 0;
  fprintf ppf "@,```catala-metadata@,";
  fprintf ppf "#[test]@\n";
  fprintf ppf "#[testcase.testui]@\n";
  fprintf ppf "#[testcase.test_description = %s]@\n"
    (String.quote t.description);
  fprintf ppf "#[testcase.test_title = %s]@\n" (String.quote t.title);
  List.iter
    (fun (var, value) ->
      let payload =
        match value with
        | None -> var
        | Some v ->
          Printf.sprintf "%s: %s" var (string_of_runtime_value ~lang v)
      in
      fprintf ppf "#[testcase.variable = %s]@\n" (String.quote payload))
    t.variables;
  fprintf ppf "@[<v 2>%s %s:@," strings.declaration_scope t.testing_scope;
  fprintf ppf "%s %s %s %s.%s@," strings.output_scope sscope_var strings.scope
    t.tested_scope.module_name t.tested_scope.name;
  fprintf ppf "@]@,```@,";
  fprintf ppf "@,```catala@,";
  fprintf ppf "@[<v 2>%s %s:" strings.scope t.testing_scope;
  List.iter
    (fun (tvar, t_in) ->
      let should_skip =
        match t_in.O.value with
        | Some { value = { value = O.NotOverridden; _ }; _ } -> true
        | _ -> false
      in
      if should_skip then ()
      else
        fprintf ppf "@,@[<hv 2>%s %s.%s %s@ %a@]" strings.definition sscope_var
          tvar strings.equals
          (print_catala_value_opt ~lang)
          t_in)
    t.test_inputs;
  List.iter
    (fun (tvar, t_out) ->
      match t_out.value with
      | None | Some { value = { value = O.NotOverridden; _ }; _ } -> ()
      | Some { value; _ } ->
        fprintf ppf "@,%s (@[<hv>%s.%s =@ %a)@]" strings.assertion sscope_var
          tvar
          (print_catala_value ~typ:(Some t_out.typ) ~lang)
          value)
    t.test_outputs;
  fprintf ppf "@]@,```@,"

(* Every input and output of every test, checked against its declared type. *)
let check_tests_fit (tests : O.test list) : (unit, string list) Result.t =
  let problems =
    List.concat_map
      (fun (t : O.test) ->
        List.concat_map
          (fun (where, record) ->
            List.filter_map
              (fun (name, (io : O.test_io)) ->
                match io.value with
                | None -> None
                | Some vd -> (
                  match value_fits io.typ vd.O.value with
                  | Ok () -> None
                  | Error msg ->
                    Some
                      (Printf.sprintf "%s: %s.%s%s" t.O.testing_scope where name
                         msg)))
              record)
          ["in", t.O.test_inputs; "out", t.O.test_outputs])
      tests
  in
  if problems = [] then Ok () else Error problems

let write_catala options outfile =
  let tests =
    J.read_test_list (Yojson.init_lexer ()) (Lexing.from_channel stdin)
  in
  (match check_tests_fit tests with
  | Ok () -> ()
  | Error problems ->
    Message.error "These values do not fit their declared types:@\n%s"
      (String.concat "\n" problems));
  let lang =
    Catala_utils.Cli.file_lang
      (match options.Global.input_src with
      | Global.FileName f -> f
      | Global.Contents (_, f) -> f
      | Global.Stdin _ -> "")
  in
  let _fname, with_out =
    File.get_main_out_formatter () ~source_file:(Global.Stdin "")
      ~output_file:(Option.map options.Global.path_rewrite outfile)
  in
  with_out
  @@ fun ppf ->
  Format.fprintf ppf "%s@\n@\n" (get_lang_strings lang).header;
  let _opened =
    List.fold_left
      (fun opened test ->
        Format.pp_open_vbox ppf 0;
        let opened =
          (* Implicit stdlib aliases get no Using line. TODO: remove once the
             compiler provides active imports for the target module, so we can
             decide this precisely. *)
          let modules_to_open =
            String.Set.(
              diff
                (of_list
                   (test.O.tested_scope.module_name
                   :: test.O.tested_scope.module_deps))
                opened
              |> filter (fun m -> not (is_implicit_stdlib_alias lang m)))
          in
          String.Set.iter
            (fun modname ->
              Format.fprintf ppf "> %s %s@,"
                (get_lang_strings lang).using_module modname)
            modules_to_open;
          String.Set.union modules_to_open opened
        in
        write_catala_test ppf test lang;
        Format.pp_close_box ppf ();
        opened)
      String.Set.empty tests
  in
  ()

type path =
  | SField of StructField.t
  | ListIdx of int
  | TupIdx of int
  | EnumPayload of string

type diff = {
  path : path list;
  expected : (dcalc, typed) gexpr;
  actual : (dcalc, typed) gexpr;
}

let pp_diff fmt { path; expected; actual } =
  let open Format in
  let pp_path fmt = function
    | SField sf -> fprintf fmt "<%a>" StructField.format sf
    | ListIdx i -> fprintf fmt "[%d]" i
    | TupIdx i -> fprintf fmt "(%d)" i
    | EnumPayload c -> fprintf fmt "{%s}" c
  in
  fprintf fmt "@[<v 2>Diff on %a:@ expected: %a@ actual: %a@]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "→") pp_path)
    path (Print.expr ()) expected (Print.expr ()) actual

let rec compute_diff
    curr_rev_path
    (expected_result : (dcalc, typed) gexpr)
    (actual_result : (dcalc, typed) gexpr) : diff list =
  let l, r = Mark.remove expected_result, Mark.remove actual_result in
  (* Infix operator to chain comparisons lexicographically. *)
  let mk_diff ?path expected actual =
    {
      path =
        List.rev
          (match path with
          | None -> curr_rev_path
          | Some path -> path :: curr_rev_path);
      expected;
      actual;
    }
  in
  let eempty : (dcalc, typed) gexpr =
    Mark.add
      (Typed { pos = Pos.void; ty = Mark.add Pos.void (TLit TUnit) })
      EEmpty
  in
  match l, r with
  | ELit l1, ELit l2 ->
    if Expr.compare_lit l1 l2 = 0 then []
    else [mk_diff expected_result actual_result]
  | EApp _, EApp _ -> assert false
  | EAppOp _, EAppOp _ -> assert false
  | EArray a1, EArray a2 ->
    let rec loop i = function
      | [], [] -> []
      | [], h :: t -> mk_diff ~path:(ListIdx i) eempty h :: loop (succ i) ([], t)
      | h :: t, [] -> mk_diff ~path:(ListIdx i) h eempty :: loop (succ i) (t, [])
      | h :: t, h' :: t' ->
        compute_diff (ListIdx i :: curr_rev_path) h h' @ loop (succ i) (t, t')
    in
    loop 0 (a1, a2)
  | ETuple es1, ETuple es2 ->
    let es1 = List.mapi (fun i x -> i, x) es1 in
    List.concat_map
      (fun ((i, e1), e2) -> compute_diff (TupIdx i :: curr_rev_path) e1 e2)
      (List.combine es1 es2)
  | ( EStruct { name = _; fields = field_map1 },
      EStruct { name = _; fields = field_map2 } ) ->
    let lb, rb =
      StructField.Map.bindings field_map1, StructField.Map.bindings field_map2
    in
    List.map2
      (fun (sf, e) (_, e') -> compute_diff (SField sf :: curr_rev_path) e e')
      lb rb
    |> List.concat
  | EVar _, EVar _ -> assert false
  | EExternal _, EExternal _ -> assert false
  | EAbs _, EAbs _ -> assert false
  | EIfThenElse _, EIfThenElse _ -> assert false
  | EStructAccess _, EStructAccess _ -> assert false
  | EMatch _, EMatch _ -> assert false
  | ETupleAccess _, ETupleAccess _ -> assert false
  | ( EInj { e = e1; name = _name1; cons = cons1 },
      EInj { e = e2; name = _name2; cons = cons2 } ) ->
    if EnumConstructor.equal cons1 cons2 then
      compute_diff
        (EnumPayload (EnumConstructor.to_string cons1) :: curr_rev_path)
        e1 e2
    else [mk_diff expected_result actual_result]
  | EPos p1, EPos p2 ->
    if Pos.compare p1 p2 = 0 then []
    else [mk_diff expected_result actual_result]
  | EEmpty, EEmpty -> []
  | EAssert _, EAssert _ -> assert false
  | EFatalError _, EFatalError _ -> assert false
  | EDefault _, EDefault _ -> assert false
  | EPureDefault _, EPureDefault _ -> assert false
  | EErrorOnEmpty _, EErrorOnEmpty _ -> assert false
  | _ -> assert false

let compute_diff
    (expected_results : (StructField.t * (dcalc, typed) gexpr) list)
    (actual_results : (StructField.t * (dcalc, typed) gexpr) list) : diff list =
  let expected_results =
    List.sort (fun (l, _) (r, _) -> StructField.compare l r) expected_results
  in
  let actual_results =
    List.sort (fun (l, _) (r, _) -> StructField.compare l r) actual_results
    |> List.filter (fun (f, _) -> List.mem_assoc f expected_results)
  in
  if List.length expected_results <> List.length actual_results then
    Message.error "The run's outputs do not cover every asserted field";
  List.map2
    (fun (field, e) (_, a) ->
      (* Start the path with the field name *)
      compute_diff [SField field] e a)
    expected_results actual_results
  |> List.concat

let proj_diff get_value ({ path; expected; actual } : diff) : O.diff =
  let proj_path : path -> O.path_segment = function
    | SField sf -> `StructField (StructField.to_string sf)
    | ListIdx i -> `ListIndex i
    | TupIdx i -> `TupleIndex i
    | EnumPayload c -> `EnumPayload c
  in
  let expected = get_value expected in
  let actual = get_value actual in
  { O.path = List.map proj_path path; expected; actual }
