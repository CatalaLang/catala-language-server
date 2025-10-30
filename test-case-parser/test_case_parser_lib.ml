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

let to_relative (p : File.t) =
  let cwd = File.path_to_list (Sys.getcwd ()) in
  let pl = File.path_to_list p in
  let rec loop = function
    | [], [] -> ["."]
    | h :: t, (h' :: t' as r) -> if h = h' then loop (t, t') else r
    | [], r -> r
    | _, [] -> ["."]
  in
  File.clean_path (loop (cwd, pl) |> String.concat Filename.dir_sep)

let lookup_clerk_toml from_dir =
  let open Catala_utils in
  try
    begin
      let from_dir =
        if Filename.is_relative from_dir then
          (File.(Sys.getcwd () / from_dir) :> string)
        else from_dir
      in
      match
        File.(
          find_in_parents ~cwd:from_dir (fun dir -> exists (dir / "clerk.toml")))
      with
      | None -> None
      | Some (abs_dir, rel) ->
        let clerk_toml_path = File.(abs_dir / "clerk.toml") in
        Message.debug "Found config file: %s" clerk_toml_path;
        let config = Clerk_lib.Clerk_config.read clerk_toml_path in
        Some (config, rel)
    end
  with _ -> None

let lookup_config current_dir =
  (* Otherwise, lookup for the toml *)
  let dir =
    if Filename.is_relative current_dir then
      if current_dir = "." then Sys.getcwd ()
      else File.(Sys.getcwd () / current_dir)
    else current_dir
  in
  lookup_clerk_toml dir

let lookup_include_dirs ?(prefix_build = false) ?buffer_path options =
  (* Otherwise, lookup for the toml *)
  let f dir =
    let dir =
      if Filename.is_relative dir then
        if dir = "." then Sys.getcwd () else File.(Sys.getcwd () / dir)
      else to_relative dir
    in
    lookup_clerk_toml dir
    |> function
    | None -> ".", []
    | Some (config, rel) ->
      let path_to_build = to_relative File.(dir / rel) in
      let include_dirs =
        if prefix_build then
          List.map
            (fun p -> File.(path_to_build / "_build" / p))
            config.global.include_dirs
        else List.map (File.( / ) path_to_build) config.global.include_dirs
      in
      Message.debug "@[<h>Found %s dirs:@ %a@]"
        (if prefix_build then "build" else "include")
        Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
        include_dirs;
      path_to_build, List.map Global.raw_file include_dirs
  in
  match options.Global.input_src with
  | FileName file | Contents (_, file) -> f (Filename.dirname file)
  | Stdin _ -> (
    match buffer_path with
    | None -> f (Sys.getcwd ())
    | Some buffer_path -> f (Filename.dirname buffer_path))

let build_dir_rel ?buffer_path options =
  (* Otherwise, lookup for the toml *)
  let f dir =
    let dir =
      if Filename.is_relative dir then
        if dir = "." then Sys.getcwd () else File.(Sys.getcwd () / dir)
      else to_relative dir
    in
    lookup_clerk_toml dir
    |> function
    | None -> None | Some (_config, rel) -> Some (to_relative File.(dir / rel))
  in
  match options.Global.input_src with
  | FileName file | Contents (_, file) -> f (Filename.dirname file)
  | Stdin _ -> (
    match buffer_path with
    | None -> f (Sys.getcwd ())
    | Some buffer_path -> f (Filename.dirname buffer_path))

exception Unsupported of string

let unsupported fmt = Format.ksprintf (fun msg -> raise (Unsupported msg)) fmt

let get_typ_literal = function
  | TBool -> O.TBool
  | TUnit -> raise (Unsupported "unit type")
  | TInt -> O.TInt
  | TRat -> O.TRat
  | TMoney -> O.TMoney
  | TDate -> O.TDate
  | TDuration -> O.TDuration
  | TPos -> assert false

let rec get_typ ?module_name decl_ctx = function
  | TLit tlit, _ -> get_typ_literal tlit
  | TTuple tl, _ -> O.TTuple (List.map (get_typ ?module_name decl_ctx) tl)
  | TStruct name, _ -> O.TStruct (get_struct ?module_name decl_ctx name)
  | TEnum name, _ -> O.TEnum (get_enum ?module_name decl_ctx name)
  | TOption ty, _ -> O.TOption (get_typ ?module_name decl_ctx ty)
  | TArray ty, _ -> O.TArray (get_typ ?module_name decl_ctx ty)
  | TArrow _, _ -> raise (Unsupported "function type")
  | TDefault _, _ -> raise (Unsupported "default type")
  | TForAll _, _ -> raise (Unsupported "wildcard type")
  | TVar _, _ -> raise (Unsupported "type variable")
  | TClosureEnv, _ -> raise (Unsupported "closure type")

and get_struct ?module_name decl_ctx struct_name =
  let fields_map = StructName.Map.find struct_name decl_ctx.ctx_structs in
  let module_name =
    if StructName.path struct_name = [] then module_name
    else Some (Uid.Path.to_string (StructName.path struct_name))
  in
  let fields =
    List.map
      (fun (field, typ) ->
        StructField.to_string field, get_typ ?module_name decl_ctx typ)
      (StructField.Map.bindings fields_map)
  in
  let struct_name =
    match module_name with
    | None -> StructName.to_string struct_name
    | Some s ->
      if StructName.path struct_name = [] then
        Format.sprintf "%s.%s" s (StructName.to_string struct_name)
      else StructName.to_string struct_name
  in
  { O.struct_name; fields }

and get_enum ?module_name decl_ctx enum_name =
  let constr_map = EnumName.Map.find enum_name decl_ctx.ctx_enums in
  let module_name =
    if EnumName.path enum_name = [] then module_name
    else Some (Uid.Path.to_string (EnumName.path enum_name))
  in
  let constructors =
    List.map
      (fun (constr, typ) ->
        ( EnumConstructor.to_string constr,
          match typ with
          | TLit TUnit, _ -> None
          | _ -> Some (get_typ ?module_name decl_ctx typ) ))
      (EnumConstructor.Map.bindings constr_map)
  in
  let enum_name =
    match module_name with
    | None -> EnumName.to_string enum_name
    | Some s ->
      if EnumName.path enum_name = [] then
        Format.sprintf "%s.%s" s (EnumName.to_string enum_name)
      else EnumName.to_string enum_name
  in
  { O.enum_name; constructors }

type Pos.attr += Uid of string
type Pos.attr += TestDescription of string

let rec get_value : type a. decl_ctx -> (a, 'm) gexpr -> O.runtime_value =
 fun decl_ctx e ->
  let pos = Expr.pos e in
  let attrs =
    Pos.get_attrs pos (function Uid s -> Some (O.Uid s) | _ -> None)
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
      match (get_value decl_ctx e1).value, (get_value decl_ctx e2).value with
      | ( O.Duration { years = y1; months = m1; days = d1 },
          O.Duration { years = y2; months = m2; days = d2 } ) ->
        O.Duration { years = y1 + y2; months = m1 + m2; days = d1 + d2 }
      | _ -> Message.error ~pos "Invalid duration literal.")
    | EArray args ->
      O.Array (Array.of_list (List.map (get_value decl_ctx) args))
    | EStruct { name; fields } ->
      O.Struct
        ( get_struct decl_ctx name,
          List.map
            (fun (field, v) ->
              StructField.to_string field, get_value decl_ctx v)
            (StructField.Map.bindings fields) )
    | EInj { name; e = ELit LUnit, _; cons } ->
      O.Enum (get_enum decl_ctx name, (EnumConstructor.to_string cons, None))
    | EInj { name; e; cons } ->
      O.Enum
        ( get_enum decl_ctx name,
          (EnumConstructor.to_string cons, Some (get_value decl_ctx e)) )
    | EEmpty -> O.Empty
    | _ -> Message.error ~pos "This test value is not a literal: %a." Expr.format e
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

let scope_inputs ?module_name decl_ctx scope =
  I.ScopeDef.Map.fold
    (fun ((v, _pos), kind) sdef acc ->
      match kind with
      | SubScopeInput _ -> acc
      | Var _ -> (
        match fst sdef.I.scope_def_io.I.io_input with
        | Catala_runtime.NoInput -> acc
        | Catala_runtime.OnlyInput ->
          ( ScopeVar.to_string v,
            get_typ ?module_name decl_ctx sdef.I.scope_def_typ )
          :: acc
        | Catala_runtime.Reentrant ->
          ( ScopeVar.to_string v,
            get_typ ?module_name decl_ctx sdef.I.scope_def_typ )
          :: acc))
    scope.I.scope_defs []
  |> List.rev

let retrieve_scope_module_deps (prg : I.program) (scope : I.scope) =
  let decl_ctx = prg.program_ctx in
  let filtered_input_typs : typ list =
    I.ScopeDef.Map.fold
      (fun (_, kind) (sdef : I.scope_def) acc ->
        (* Do not consider: internals & subscopes, *)
        match kind, Mark.remove sdef.scope_def_io.io_input with
        | _, NoInput | I.ScopeDef.SubScopeInput _, _ -> acc
        | _, (OnlyInput | Reentrant) -> sdef.I.scope_def_typ :: acc)
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
  in
  List.fold_left process_typ ModuleName.Set.empty filtered_input_typs
  |> ModuleName.Set.elements
  |> List.map ModuleName.to_string

let get_scope_def (prg : I.program) (sc : I.scope) ~tested_module : O.scope_def
    =
  let decl_ctx = prg.program_ctx in
  let module_name = ModuleName.to_string tested_module in
  let info = ScopeName.Map.find sc.scope_uid decl_ctx.ctx_scopes in
  {
    O.name = ScopeName.base sc.scope_uid;
    module_name;
    inputs = scope_inputs ~module_name decl_ctx sc;
    outputs = (get_struct ~module_name decl_ctx info.out_struct_name).fields;
    module_deps = retrieve_scope_module_deps prg sc;
  }

let get_scope_test
    (prg : I.program)
    (testing_scope : string)
    (tested_scope : ScopeName.t)
    ~tested_module : O.test =
  let tested_module =
    match tested_module with
    | None ->
      Format.ksprintf failwith "Tested scope %s is not part of a Catala module"
        (ScopeName.to_string tested_scope)
    | Some x -> x
  in
  let tested_scope =
    let modul =
      List.fold_left
        (fun _ m -> ModuleName.Map.find m prg.program_modules)
        prg.program_root
        (ScopeName.path tested_scope)
    in
    get_scope_def prg
      (ScopeName.Map.find tested_scope modul.module_scopes)
      ~tested_module
  in
  let test_inputs =
    List.map (fun (v, typ) -> v, { O.typ; value = None }) tested_scope.inputs
  in
  let test_outputs =
    List.map (fun (v, typ) -> v, { O.typ; value = None }) tested_scope.outputs
  in
  let description = "" in
  { O.testing_scope; tested_scope; test_outputs; test_inputs; description }

(* --- *)

let write_stdout f arg =
  let buf = Buffer.create 4096 in
  f buf arg;
  Buffer.output_buffer stdout buf

let print_test test = write_stdout Test_case_j.write_test test
let print_tests test = write_stdout Test_case_j.write_test_list test

let read_program includes path_to_build options =
  let stdlib =
    Some (Global.raw_file File.(path_to_build / "_build" / "libcatala"))
  in
  Driver.Passes.desugared options ~stdlib ~includes

let generate_test
    tested_scope
    ?(testing_scope = tested_scope ^ "_test")
    include_dirs
    options =
  let path_to_build, include_dirs =
    if include_dirs = [] then lookup_include_dirs options else ".", include_dirs
  in
  let prg, _ = read_program include_dirs path_to_build options in
  let tested_scope =
    Ident.Map.find tested_scope prg.I.program_ctx.ctx_scope_index
  in
  let test =
    get_scope_test prg testing_scope tested_scope
      ~tested_module:(Option.map fst prg.I.program_module_name)
  in
  print_tests [test]

exception InvalidTestingScope of string

let invalid_testing_scope fmt =
  Format.kasprintf (fun msg -> raise (InvalidTestingScope msg)) fmt

let get_test_scopes prg =
  prg.I.program_root.module_scopes
  |> ScopeName.Map.filter (fun scope_name _scope ->
         Pos.has_attr (Mark.get (ScopeName.get_info scope_name)) Test)
  |> ScopeName.Map.keys

let get_catala_test (prg, naming_ctx) testing_scope_name =
  let testing_scope =
    ScopeName.Map.find testing_scope_name prg.I.program_root.module_scopes
  in
  let description =
    Pos.get_attr
      (Mark.get (ScopeName.get_info testing_scope_name))
      (function TestDescription s -> Some s | _ -> None)
    |> Option.value ~default:""
  in
  let subscope_var, tested_scope =
    let count = ScopeVar.Map.cardinal testing_scope.I.scope_sub_scopes in
    if count <> 1 then
      invalid_testing_scope
        "@{<b>%a@}: testing scopes are expected to have one, and only one \
         subscope, this has %d"
        ScopeName.format testing_scope_name count
    else ScopeVar.Map.choose testing_scope.scope_sub_scopes
  in
  let tested_id_var_map =
    Ident.Map.filter_map
      (fun _ -> function ScopeVar v -> Some v | SubScope _ -> None)
      (ScopeName.Map.find tested_scope
         naming_ctx.Desugared.Name_resolution.scopes)
        .var_idmap
  in
  let tested_module = ScopeName.path tested_scope |> List.hd |> Option.some in
  let base_test =
    get_scope_test ~tested_module prg
      (ScopeName.to_string testing_scope_name)
      tested_scope
  in
  let test_inputs =
    List.map
      (fun (var_str, test_in) ->
        let var_within_origin_scope =
          Ident.Map.find var_str tested_id_var_map
        in
        let value =
          let rules =
            try
              let def_key =
                ( (subscope_var, Pos.void),
                  I.ScopeDef.SubScopeInput
                    { name = tested_scope; var_within_origin_scope } )
              in
              let def = I.ScopeDef.Map.find def_key testing_scope.scope_defs in
              RuleName.Map.bindings def.scope_def_rules
            with Ident.Map.Not_found _ | I.ScopeDef.Map.Not_found _ -> []
          in
          match rules with
          | [] -> None
          | [(_, rule)] ->
            let e = Expr.unbox_closed rule.rule_cons in
            let value = get_value prg.program_ctx e in
            Some { O.value; pos = Some (get_source_position (Expr.pos e)) }
          | rules ->
            let extra_pos =
              List.map (fun (r, _) -> "", Mark.get (RuleName.get_info r)) rules
            in
            Message.error ~extra_pos
              "Multiple definitions of test input value in test scope %a.%a!;@ \
               %d rule(s) found: [%a]"
              ScopeName.format testing_scope_name ScopeVar.format
              var_within_origin_scope (List.length rules)
              (Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
                 (fun ppf (r, _) -> RuleName.format ppf r))
              rules
        in
        var_str, { test_in with O.value })
      base_test.test_inputs
  in
  let test_outputs =
    let scope_info =
      ScopeName.Map.find tested_scope prg.program_ctx.ctx_scopes
    in
    let scope_field_map =
      ScopeVar.Map.fold
        (fun var field acc -> StructField.Map.add field var acc)
        scope_info.out_struct_fields StructField.Map.empty
    in
    let assertion_values =
      I.AssertionName.Map.fold
        (fun _ e acc ->
          match Expr.unbox_closed e with
          | ( EAppOp
                {
                  op = Op.Eq, _;
                  args =
                    [
                      ( EStructAccess
                          {
                            field;
                            e =
                              ( ELocation
                                  (DesugaredScopeVar { name = svar, pos; _ }),
                                _ );
                            _;
                          },
                        _ );
                      value;
                    ];
                  _;
                },
              _ )
            when svar = subscope_var ->
            let scope_var = StructField.Map.find field scope_field_map in
            ScopeVar.Map.add scope_var
              {
                O.value = get_value prg.program_ctx value;
                pos = Some (get_source_position pos);
              }
              acc
          | ( EAppOp
                {
                  op = Op.Eq, _;
                  args = [((EStructAccess _, _) as e); _value];
                  _;
                },
              m ) ->
            Message.error ~pos:(Expr.mark_pos m)
              "Could not read test assertion: %a" Expr.format e
          | (_, m) as e ->
            Message.error ~pos:(Expr.mark_pos m)
              "Could not read test assertion: %a" Expr.format e)
        testing_scope.scope_assertions ScopeVar.Map.empty
    in
    List.map
      (fun (var_str, test_out) ->
        let var = Ident.Map.find var_str tested_id_var_map in
        let value = ScopeVar.Map.find_opt var assertion_values in
        var_str, { test_out with O.value })
      base_test.test_outputs
  in
  { base_test with O.test_inputs; test_outputs; description }

let import_catala_tests (prg, naming_ctx) =
  List.map (get_catala_test (prg, naming_ctx)) (get_test_scopes prg)

let read_test include_dirs (options : Global.options) buffer_path =
  let path_to_build, include_dirs =
    if include_dirs = [] then lookup_include_dirs ?buffer_path options
    else ".", include_dirs
  in
  let prg = read_program include_dirs path_to_build options in
  let tests = import_catala_tests prg in
  write_stdout Test_case_j.write_test_list tests

type lang_strings = {
  declaration_scope : string;
  output_scope : string;
  using_module : string;
  definition : string;
  assertion : string;
  equals : string;
  content : string;
  scope : string;
}

let get_lang_strings = function
  | Catala_utils.Global.Fr ->
    {
      declaration_scope = "déclaration champ d'application";
      output_scope = "résultat";
      using_module = "Usage de";
      definition = "définition";
      assertion = "assertion";
      equals = "égal à";
      content = "contenu";
      scope = "champ d'application";
    }
  | En ->
    {
      declaration_scope = "declaration scope";
      output_scope = "output";
      using_module = "Using";
      definition = "definition";
      assertion = "assertion";
      equals = "equals";
      content = "content";
      scope = "scope";
    }
  | _ -> raise (unsupported "unsupported language")

type duration_units = { day : string; month : string; year : string }

type value_strings = {
  true_str : string;
  false_str : string;
  money_fmt : (int -> int -> unit, Format.formatter, unit) format;
  decimal_sep : char;
  content_str : string;
  duration_units : duration_units;
}

let get_value_strings = function
  | Catala_utils.Global.Fr ->
    {
      true_str = "vrai";
      false_str = "faux";
      money_fmt = format_of_string "%01d,%02d €";
      decimal_sep = ',';
      content_str = "contenu";
      duration_units = { day = "jour"; month = "mois"; year = "an" };
    }
  | En ->
    {
      true_str = "true";
      false_str = "false";
      money_fmt = format_of_string "$%01d.%02d";
      decimal_sep = '.';
      content_str = "content";
      duration_units = { day = "day"; month = "month"; year = "year" };
    }
  | _ -> raise (unsupported "unsupported language")

let print_attrs ppf (attrs : O.attr_def list) =
  let open Format in
  pp_print_list
    (fun ppf (attr : O.attr_def) ->
      match attr with
      | Uid (s : string) -> fprintf ppf "#[testcase.uid = \"%s\"]@\n" s
      | TestDescription (s : string) ->
        fprintf ppf "#[testcase.test_description = %s]@\n" (String.quote s))
    ppf attrs

let rec print_catala_value ~(typ : O.typ option) ~lang ppf (v : O.runtime_value)
    =
  let open Format in
  let strings = get_value_strings lang in
  print_attrs ppf v.attrs;
  match typ, v.value with
  | _, O.Bool b ->
    pp_print_string ppf (if b then strings.true_str else strings.false_str)
  | _, O.Money m -> fprintf ppf strings.money_fmt (m / 100) (m mod 100)
  | _, O.Integer i -> pp_print_int ppf i
  | _, O.Decimal f ->
    let s = sprintf "%g" f in
    let s = if String.contains s '.' then s else sprintf "%.0f." f in
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
           (if years > 0 then
              Some
                (fun ppf ->
                  fprintf ppf "%d %s" years strings.duration_units.year)
            else None);
           (if months > 0 then
              Some
                (fun ppf ->
                  fprintf ppf "%d %s" months strings.duration_units.month)
            else None);
           (if days > 0 then
              Some
                (fun ppf -> fprintf ppf "%d %s" days strings.duration_units.day)
            else None);
         ])
  | Some (TEnum { enum_name; constructors }), O.Enum (_en, (constr, Some v)) ->
    fprintf ppf "@[<hv 2>%s.%s %s %a@]" enum_name constr strings.content_str
      (print_catala_value ~typ:(List.assoc constr constructors) ~lang)
      v
  | _, O.Enum (en, (constr, Some v)) ->
    fprintf ppf "@[<hv 2>%s.%s %s %a@]" en.enum_name constr strings.content_str
      (print_catala_value ~typ:None ~lang)
      v
  | Some (TEnum { enum_name; _ }), O.Enum (_en, (constr, None)) ->
    fprintf ppf "%s.%s" enum_name constr
  | _, O.Enum (en, (constr, None)) -> fprintf ppf "%s.%s" en.enum_name constr
  | Some (O.TStruct sdecl), O.Struct (st, fields) ->
    fprintf ppf "@[<hv 2>%s {@ %a@;<1 -2>}@]" st.struct_name
      (pp_print_list ~pp_sep:pp_print_space (fun ppf (typ, (fld, v)) ->
           fprintf ppf "-- %s: %a@," fld
             (print_catala_value ~typ:(Some typ) ~lang)
             v))
      (List.combine (List.map snd sdecl.fields) fields)
  | _, O.Struct (st, fields) ->
    fprintf ppf "@[<hv 2>%s {@ %a@;<1 -2>}@]" st.struct_name
      (pp_print_list ~pp_sep:pp_print_space (fun ppf (fld, v) ->
           fprintf ppf "-- %s: %a@," fld (print_catala_value ~typ:None ~lang) v))
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
  | _, O.Empty -> assert false

let rec generate_default_value (typ : O.typ) : O.runtime_value =
  let value =
    match typ with
    | TBool -> O.Bool false
    | TInt -> O.Integer 0
    | TRat -> O.Decimal 0.
    | TMoney -> O.Money 0
    | TDate -> O.Date { year = 1970; month = 1; day = 1 }
    | TDuration -> O.Duration { years = 0; months = 0; days = 0 }
    | TTuple l -> O.Array (List.map generate_default_value l |> Array.of_list)
    | TStruct decl ->
      O.Struct
        (decl, List.map (fun (s, t) -> s, generate_default_value t) decl.fields)
    | TEnum decl ->
      let elt =
        let cn, ty =
          List.find_opt
            (function _, None -> true | _ -> false)
            decl.constructors
          |> function Some s -> s | None -> List.hd decl.constructors
        in
        cn, Option.map generate_default_value ty
      in
      O.Enum (decl, elt)
    | TOption typ -> (generate_default_value typ).value
    | TArray _ -> O.Array [||]
  in
  { value; attrs = [] }

let print_catala_value_opt ~lang ppf (t_in : O.test_io) =
  match t_in.O.value with
  | None -> generate_default_value t_in.typ |> print_catala_value ~lang ppf
  | Some { value; _ } -> print_catala_value ~lang ppf value

let write_catala_test ppf t lang =
  let open Format in
  let open O in
  let strings = get_lang_strings lang in
  let sscope_var =
    let sname =
      match Filename.extension t.tested_scope.name with
      | "" -> t.tested_scope.name
      | s -> String.sub s 1 (String.length s - 1)
    in
    String.to_snake_case sname
  in
  pp_open_vbox ppf 0;
  fprintf ppf "@,```catala-metadata@,";
  fprintf ppf "#[test]@\n";
  fprintf ppf "#[testcase.test_description = %s]@\n"
    (String.quote t.description);
  fprintf ppf "@[<v 2>%s %s:@," strings.declaration_scope t.testing_scope;
  fprintf ppf "%s %s %s %s.%s@," strings.output_scope sscope_var strings.scope
    t.tested_scope.module_name t.tested_scope.name;
  fprintf ppf "@]@,```@,";
  fprintf ppf "@,```catala@,";
  fprintf ppf "@[<v 2>%s %s:" strings.scope t.testing_scope;
  List.iter
    (fun (tvar, t_in) ->
      fprintf ppf "@,@[<hv 2>%s %s.%s %s@ %a@]" strings.definition sscope_var
        tvar strings.equals
        (print_catala_value_opt ~typ:None ~lang)
        t_in)
    t.test_inputs;
  List.iter
    (fun (tvar, t_out) ->
      match t_out.value with
      | None -> ()
      | Some { value; _ } ->
        fprintf ppf "@,%s (@[<hv>%s.%s =@ %a)@]" strings.assertion sscope_var
          tvar
          (print_catala_value ~typ:(Some t_out.typ) ~lang)
          value)
    t.test_outputs;
  fprintf ppf "@]@,```@,"

let write_catala options outfile =
  let tests =
    Test_case_j.read_test_list (Yojson.init_lexer ())
      (Lexing.from_channel stdin)
  in
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
  let _opened =
    List.fold_left
      (fun opened test ->
        Format.pp_open_vbox ppf 0;
        let opened =
          let modules_to_open =
            Ident.Set.(
              diff
                (of_list
                   (test.O.tested_scope.module_name
                   :: test.O.tested_scope.module_deps))
                opened)
          in
          Ident.Set.iter
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

let retrieve_assertions_values (dcalc_prg : typed Dcalc.Ast.program) :
    (StructField.t * (dcalc, typed) gexpr) list =
  let get_expected_value (assert_e : (dcalc, typed) gexpr) =
    match Mark.remove assert_e with
    | EAssert (EAppOp { args = [(EStructAccess { field; _ }, _); v]; _ }, _) ->
      field, v
    | _ -> assert false
  in
  let code_items = dcalc_prg.code_items |> BoundList.to_seq |> List.of_seq in
  List.fold_left
    (fun acc -> function
      | _, Topdef _ -> acc
      | _, ScopeDef (_, body) ->
        let _, body_list = Bindlib.unbind body.scope_body_expr in
        let scope_lets : (dcalc, typed) gexpr scope_let list =
          body_list |> BoundList.to_seq |> List.of_seq |> List.map snd
        in
        List.filter_map
          (function
            | { scope_let_kind = Assertion; scope_let_expr; _ } ->
              Some (get_expected_value scope_let_expr)
            | _ -> None)
          scope_lets)
    [] code_items

type path = SField of StructField.t | ListIdx of int | TupIdx of int

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
    if EnumConstructor.equal cons1 cons2 then compute_diff curr_rev_path e1 e2
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
  assert (List.length expected_results = List.length actual_results);
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
  in
  let expected = get_value expected in
  let actual = get_value actual in
  { O.path = List.map proj_path path; expected; actual }

let run_test testing_scope include_dirs options =
  let path_to_build, include_dirs =
    if include_dirs = [] then
      let _path_to_build, include_dirs = lookup_include_dirs options in
      let path_to_build, build_include_dirs =
        lookup_include_dirs ~prefix_build:true options
      in
      path_to_build, build_include_dirs @ include_dirs
    else ".", []
  in
  let desugared_prg, naming_ctx =
    read_program include_dirs path_to_build options
  in
  let testing_scope_name =
    match
      Ident.Map.find_opt testing_scope
        Desugared.Name_resolution.(naming_ctx.local.typedefs)
    with
    | Some (TScope (sname, _)) -> sname
    | _ -> Message.error "No scope %S was found in the program" testing_scope
  in
  let test = get_catala_test (desugared_prg, naming_ctx) testing_scope_name in
  let dcalc_prg : ((dcalc, dcalc, typed) base_gexpr * typed mark) program =
    let prg =
      Scopelang.From_desugared.(
        translate_program desugared_prg (build_exceptions_graph desugared_prg))
    in
    let prg = Scopelang.Ast.type_program prg in
    Dcalc.From_scopelang.translate_program prg
  in
  Interpreter.load_runtime_modules
    ~hashf:Hash.(finalise ~monomorphize_types:false)
    (dcalc_prg : typed Dcalc.Ast.program);
  let program_fun = Expr.unbox (Program.to_expr dcalc_prg testing_scope_name) in
  let program_fun =
    Message.with_delayed_errors
    @@ fun () ->
    Interpreter.evaluate_expr dcalc_prg.decl_ctx dcalc_prg.lang program_fun
  in
  let _args, program_expr =
    match program_fun with
    | EAbs { binder; _ }, _ -> Bindlib.unmbind binder
    | _ -> assert false
  in
  let failed_asserts = ref [] in
  let on_assert_failures e =
    match e with
    | { Message.kind = AssertFailure; _ } ->
      failed_asserts := e :: !failed_asserts;
      false (* absorb error *)
    | _ -> true (* propagate error and crash *)
  in
  let () =
    Catala_utils.Message.register_lsp_error_absorber on_assert_failures
  in
  let result_struct =
    Message.with_delayed_errors
    @@ fun () ->
    Interpreter.evaluate_expr dcalc_prg.decl_ctx dcalc_prg.lang program_expr
  in
  let (actual_results : (StructField.t * (dcalc, typed) gexpr) list), out_struct
      =
    match result_struct with
    | EStruct { fields; _ }, _ -> (
      match StructField.Map.choose fields with
      | _, (EStruct { fields; name }, _) ->
        let b = StructField.Map.bindings fields in
        ( List.map (fun (f, e) -> f, Interpreter.delcustom e) b,
          StructName.Map.find name dcalc_prg.decl_ctx.ctx_structs )
      | _ -> assert false)
    | _ -> assert false
  in
  let expected_results = retrieve_assertions_values dcalc_prg in
  let diffs =
    compute_diff expected_results actual_results
    |> List.map (proj_diff (get_value dcalc_prg.decl_ctx))
  in
  let test_outputs =
    List.map
      (fun (field, value_expr) ->
        let pos = Some (get_source_position (Expr.pos value_expr)) in
        ( StructField.to_string field,
          {
            O.value =
              Some { value = get_value dcalc_prg.decl_ctx value_expr; pos };
            typ =
              get_typ dcalc_prg.decl_ctx (StructField.Map.find field out_struct);
          } ))
      actual_results
  in
  let test = { test with test_outputs } in
  let assert_failures = not (!failed_asserts = []) in
  let test_run = { O.test; O.assert_failures; O.diffs } in
  write_stdout Test_case_j.write_test_run test_run

let print_scopes scopes = write_stdout Test_case_j.write_scope_def_list scopes

let list_scopes include_dirs options =
  let path_to_build, include_dirs =
    if include_dirs = [] then lookup_include_dirs options else ".", include_dirs
  in
  let prg, _ = read_program include_dirs path_to_build options in
  let module_name =
    match prg.program_module_name with
    | None -> failwith "Expected a Catala module"
    | Some (mn, _) -> mn
  in
  let modul = prg.program_root in
  let filtered_scopes =
    ScopeName.Map.filter_map
      (fun _sn -> function
        | { I.scope_visibility = Private; _ } -> None
        | sc -> (
          if scope_inputs prg.program_ctx sc = [] then
            (* We do not consider no-input scopes *)
            None
          else
            try Some (get_scope_def prg sc ~tested_module:module_name)
            with _ -> None))
      modul.module_scopes
    |> ScopeName.Map.bindings
    |> List.map snd
  in
  print_scopes filtered_scopes
