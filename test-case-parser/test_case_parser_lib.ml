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
        let config = Clerk_config.read clerk_toml_path in
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
    | None -> []
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
      List.map Global.raw_file include_dirs
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
  | TAny, _ -> raise (Unsupported "wildcard type")
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
      let year, month, day = Dates_calc.Dates.date_to_ymd t in
      O.Date { year; month; day }
    | ELit (LDuration dt) ->
      let years, months, days = Dates_calc.Dates.period_to_ymds dt in
      O.Duration { years; months; days }
    | EAppOp
        {
          op = Op.Add, _;
          args = [e1; e2];
          tys = [(TLit TDuration, _); (TLit TDuration, _)];
        } -> (
      match
        (get_value decl_ctx e1).value, (get_value decl_ctx e2).value
      with
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
    | _ -> Message.error ~pos "This test value is not a literal."
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
        | Runtime.NoInput -> acc
        | Runtime.OnlyInput ->
          ( ScopeVar.to_string v,
            get_typ ?module_name decl_ctx sdef.I.scope_def_typ )
          :: acc
        | Runtime.Reentrant ->
          ( ScopeVar.to_string v,
            get_typ ?module_name decl_ctx sdef.I.scope_def_typ )
          :: acc))
    scope.I.scope_defs []
  |> List.rev

let retrieve_scope_module_deps (prg : I.program) (scope : I.scope) =
  let decl_ctx = prg.program_ctx in
  let input_typs : typ list =
    I.ScopeDef.Map.fold
      (fun _ sdef acc -> sdef.I.scope_def_typ :: acc)
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
    | TAny -> raise (Unsupported "wildcard type")
    | TClosureEnv -> raise (Unsupported "closure type")
  in
  List.fold_left process_typ ModuleName.Set.empty input_typs
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
  { O.testing_scope; tested_scope; test_outputs; test_inputs }

(* --- *)

let write_stdout f arg =
  let buf = Buffer.create 4096 in
  f buf arg;
  Buffer.output_buffer stdout buf

let print_test test = write_stdout Test_case_j.write_test test
let print_tests test = write_stdout Test_case_j.write_test_list test
let read_program includes options = Driver.Passes.desugared options ~includes

let generate_test
    tested_scope
    ?(testing_scope = tested_scope ^ "_test")
    include_dirs
    options =
  let include_dirs =
    if include_dirs = [] then lookup_include_dirs options else include_dirs
  in
  let prg, _ = read_program include_dirs options in
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
  let re_test = Re.(compile (str "_test")) in
  prg.I.program_root.module_scopes
  |> ScopeName.Map.filter (fun name _ -> Re.execp re_test (ScopeName.base name))
  |> ScopeName.Map.keys

let get_catala_test (prg, naming_ctx) testing_scope_name =
  let testing_scope =
    ScopeName.Map.find testing_scope_name prg.I.program_root.module_scopes
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
  { base_test with O.test_inputs; test_outputs }

let import_catala_tests (prg, naming_ctx) =
  List.map (get_catala_test (prg, naming_ctx)) (get_test_scopes prg)

let read_test include_dirs (options : Global.options) buffer_path =
  let include_dirs =
    if include_dirs = [] then lookup_include_dirs ?buffer_path options
    else include_dirs
  in
  let prg = read_program include_dirs options in
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

let print_attrs ppf (attrs:O.attr_def list) =
  let open Format in
  pp_print_list
  (fun ppf (attr:O.attr_def) ->
    match attr with
    | Uid (s:string) -> fprintf ppf "#[testcase.uid = \"%s\"]@\n" s)
  ppf attrs

let rec print_catala_value ~lang ppf (v : O.runtime_value) =
  let open Format in
  let strings = get_value_strings lang in
  print_attrs ppf v.attrs;
  match v.value with
  | O.Bool b ->
    pp_print_string ppf (if b then strings.true_str else strings.false_str)
  | O.Money m -> fprintf ppf strings.money_fmt (m / 100) (m mod 100)
  | O.Integer i -> pp_print_int ppf i
  | O.Decimal f ->
    let s = sprintf "%g" f in
    let s = if String.contains s '.' then s else sprintf "%.0f." f in
    pp_print_string ppf
      (String.map (function '.' -> strings.decimal_sep | c -> c) s)
  | O.Date { year; month; day } -> fprintf ppf "|%04d-%02d-%02d|" year month day
  | O.Duration { years = 0; months = 0; days = 0 } ->
    fprintf ppf "0 %s" strings.duration_units.day
  | O.Duration { years; months; days } ->
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
  | O.Enum (en, (constr, Some v)) ->
    fprintf ppf "@[<hv 2>%s.%s %s %a@]" en.enum_name constr strings.content_str
      (print_catala_value ~lang) v
  | O.Enum (en, (constr, None)) -> fprintf ppf "%s.%s" en.enum_name constr
  | O.Struct (st, fields) ->
    fprintf ppf "@[<hv 2>%s {@ %a@;<1 -2>}@]" st.struct_name
      (pp_print_list ~pp_sep:pp_print_space (fun ppf (fld, v) ->
           fprintf ppf "-- %s: %a@," fld (print_catala_value ~lang) v))
      fields
  | O.Array vl ->
    fprintf ppf "@[<hov 1>[%a]@]"
      (pp_print_seq
         ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
         (print_catala_value ~lang))
      (Array.to_seq vl)

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
  fprintf ppf "@,```catala@,";
  fprintf ppf "@[<v 2>%s %s:@," strings.declaration_scope t.testing_scope;
  fprintf ppf "%s %s %s %s.%s@," strings.output_scope sscope_var strings.scope
    t.tested_scope.module_name t.tested_scope.name;
  fprintf ppf "@]@,";
  fprintf ppf "@[<v 2>%s %s:" strings.scope t.testing_scope;
  List.iter
    (fun (tvar, t_in) ->
      fprintf ppf "@,@[<hv 2>%s %s.%s %s@ %a@]" strings.definition sscope_var
        tvar strings.equals
        (print_catala_value_opt ~lang)
        t_in)
    t.test_inputs;
  List.iter
    (fun (tvar, t_out) ->
      match t_out.value with
      | None -> ()
      | Some { value; _ } ->
        fprintf ppf "@,%s (@[<hv>%s.%s =@ %a)@]" strings.assertion sscope_var
          tvar (print_catala_value ~lang) value)
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
    File.get_formatter_of_out_channel () ~source_file:(Global.Stdin "")
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

let find_cmo_target file_name =
  let cmo_name = File.(file_name -.- "cmo") in
  Message.debug "Looking for %s clerk target" cmo_name;
  try
    let out =
      let ((ic, _oc, _stderr) as p) =
        Unix.open_process_full "clerk build" (Unix.environment ())
      in
      let out = In_channel.input_all ic in
      let _status = Unix.close_process_full p in
      out
    in
    let re =
      Re.(compile (seq [bol; str "_build"; rep notnl; str cmo_name; eol]))
    in
    let l = Re.matches re out in
    match l with
    | target :: _ ->
      Message.debug "Found clerk target: %s" target;
      Some target
    | [] -> None
  with exn ->
    Message.debug "Failed to run 'clerk build': %s" (Printexc.to_string exn);
    None

let run_test testing_scope include_dirs options =
  let include_dirs =
    if include_dirs = [] then
      let include_dirs = lookup_include_dirs options in
      let build_include_dirs = lookup_include_dirs ~prefix_build:true options in
      build_include_dirs @ include_dirs
    else []
  in
  let file_name =
    match options.Global.input_src with
    | FileName f | Contents (_, f) -> f
    | Stdin _ -> failwith "Error: run command must be given a file."
  in
  (* Try calling clerk build *)
  let () =
    find_cmo_target file_name
    |> function
    | None ->
      Message.debug "Cannot find cmo target for %s in clerk build output"
        file_name
    | Some cmo_target -> (
      build_dir_rel options
      |> function
      | None -> Message.debug "Did not find the _build/ relative dir path"
      | Some build_dir_rel ->
        Message.debug "Found the _build/ relative dir path: %s" build_dir_rel;
        let cmd, args = "clerk", ["build"; File.(build_dir_rel / cmo_target)] in
        Message.debug "Running '%s %s'" cmd (String.concat " " args);
        File.process_out cmd args |> ignore)
  in
  let desugared_prg, naming_ctx = read_program include_dirs options in
  let desugared_prg =
    (* Remove assertion, the diff will be performed by the GUI *)
    let open Desugared.Ast in
    let module_scopes =
      ScopeName.Map.map
        (fun scope ->
          {
            scope with
            scope_assertions = AssertionName.Map.empty;
            scope_meta_assertions = [];
          })
        desugared_prg.program_root.module_scopes
    in
    let program_root = { desugared_prg.program_root with module_scopes } in
    { desugared_prg with program_root }
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
  let dcalc_prg =
    let prg =
      Scopelang.From_desugared.(
        translate_program desugared_prg (build_exceptions_graph desugared_prg))
    in
    let prg = Scopelang.Ast.type_program prg in
    Dcalc.From_scopelang.translate_program prg
  in
  Interpreter.load_runtime_modules
    ~hashf:Hash.(finalise ~closure_conversion:false ~monomorphize_types:false)
    dcalc_prg;
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
  let result_struct =
    Message.with_delayed_errors
    @@ fun () ->
    Interpreter.evaluate_expr dcalc_prg.decl_ctx dcalc_prg.lang program_expr
  in
  let results, out_struct =
    match result_struct with
    | EStruct { fields; _ }, _ -> (
      match StructField.Map.choose fields with
      | _, (EStruct { fields; name }, _) ->
        ( StructField.Map.bindings fields,
          StructName.Map.find name dcalc_prg.decl_ctx.ctx_structs )
      | _ -> assert false)
    | _ -> assert false
  in
  let test_outputs =
    List.map
      (fun (field, value_expr) ->
        ( StructField.to_string field,
          {
            O.value =
              Some
                {
                  value = get_value dcalc_prg.decl_ctx value_expr;
                  pos = None (* TODO: Retrieve pos from value_expr? *);
                };
            typ =
              get_typ dcalc_prg.decl_ctx (StructField.Map.find field out_struct);
          } ))
      results
  in
  let test = { test with test_outputs } in
  write_stdout Test_case_j.write_test test

let print_scopes scopes = write_stdout Test_case_j.write_scope_def_list scopes

let list_scopes include_dirs options =
  let include_dirs =
    if include_dirs = [] then lookup_include_dirs options else include_dirs
  in
  let prg, _ = read_program include_dirs options in
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
