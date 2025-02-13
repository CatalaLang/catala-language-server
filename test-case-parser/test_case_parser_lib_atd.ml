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

let lookup_clerk_toml from_dir =
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  let open Catala_utils in
  let find_in_parents cwd predicate =
    let rec lookup dir =
      if predicate dir then Some dir
      else if dir = home then None
      else
        let parent = Filename.dirname dir in
        if parent = dir then None else lookup parent
    in
    match lookup cwd with Some rel -> Some rel | None -> None
  in
  try
    begin
      match
        find_in_parents from_dir (fun dir -> File.(exists (dir / "clerk.toml")))
      with
      | None -> None
      | Some dir -> (
        try
          let config = Clerk_config.read File.(dir / "clerk.toml") in
          Some (config, dir)
        with Message.CompilerError _c -> None)
    end
  with _ -> None

let lookup_include_dirs ?(include_dirs = []) ?buffer_path options =
  let f dir =
    if include_dirs <> [] then include_dirs
    else
      lookup_clerk_toml dir
      |> function
      | None -> include_dirs
      | Some (config, _) -> List.map Global.raw_file config.global.include_dirs
  in
  match options.Global.input_src with
  | FileName file | Contents (_, file) -> f (Filename.dirname file)
  | Stdin _ -> (
    match buffer_path with
    | None -> include_dirs
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
    List.map
      (fun (field, typ) -> StructField.to_string field, get_typ decl_ctx typ)
      (StructField.Map.bindings fields_map)
  in
  { O.struct_name = StructName.to_string struct_name; fields }

and get_enum decl_ctx enum_name =
  let constr_map = EnumName.Map.find enum_name decl_ctx.ctx_enums in
  let constructors =
    List.map
      (fun (constr, typ) ->
        ( EnumConstructor.to_string constr,
          match typ with
          | TLit TUnit, _ -> None
          | _ -> Some (get_typ decl_ctx typ) ))
      (EnumConstructor.Map.bindings constr_map)
  in
  { O.enum_name = EnumName.to_string enum_name; constructors }

let rec get_value : type a. decl_ctx -> (a, 'm) gexpr -> O.runtime_value =
 fun decl_ctx e ->
  match e with
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
  | ( EAppOp
        {
          op = Op.Add, _;
          args = [e1; e2];
          tys = [(TLit TDuration, _); (TLit TDuration, _)];
        },
      _ ) as e -> (
    match get_value decl_ctx e1, get_value decl_ctx e2 with
    | ( O.Duration { years = y1; months = m1; days = d1 },
        O.Duration { years = y2; months = m2; days = d2 } ) ->
      O.Duration { years = y1 + y2; months = m1 + m2; days = d1 + d2 }
    | _ -> Message.error ~pos:(Expr.pos e) "Invalid duration literal.")
  | EArray args, _ ->
    O.Array (Array.of_list (List.map (get_value decl_ctx) args))
  | EStruct { name; fields }, _ ->
    O.Struct
      ( get_struct decl_ctx name,
        List.map
          (fun (field, v) -> StructField.to_string field, get_value decl_ctx v)
          (StructField.Map.bindings fields) )
  | EInj { name; e = ELit LUnit, _; cons }, _ ->
    O.Enum (get_enum decl_ctx name, (EnumConstructor.to_string cons, None))
  | EInj { name; e; cons }, _ ->
    O.Enum
      ( get_enum decl_ctx name,
        (EnumConstructor.to_string cons, Some (get_value decl_ctx e)) )
  | e -> Message.error ~pos:(Expr.pos e) "This test value is not a literal."

let get_source_position pos =
  {
    O.filename = Pos.get_file pos;
    start_line = Pos.get_start_line pos;
    start_column = Pos.get_start_column pos;
    end_line = Pos.get_end_line pos;
    end_column = Pos.get_end_column pos;
    law_headings = Pos.get_law_info pos;
  }

let scope_inputs decl_ctx scope =
  I.ScopeDef.Map.fold
    (fun ((v, _pos), _kind) sdef acc ->
      match sdef.I.scope_def_io.I.io_input with
      | Runtime.NoInput, _ -> acc
      | Runtime.OnlyInput, _ ->
        (ScopeVar.to_string v, get_typ decl_ctx sdef.I.scope_def_typ) :: acc
      | Runtime.Reentrant, _ ->
        (ScopeVar.to_string v, get_typ decl_ctx sdef.I.scope_def_typ) :: acc)
    scope.I.scope_defs []
  |> List.rev

let retrieve_scope_module_deps (prg : I.program) (scope : I.scope) =
  let decl_ctx = prg.program_ctx in
  let input_typs : typ list =
    I.ScopeDef.Map.fold
      (fun _ sdef acc ->
        match sdef.I.scope_def_io.I.io_input with
        | Runtime.NoInput, _ -> acc
        | Runtime.OnlyInput, _ | Runtime.Reentrant, _ ->
          sdef.I.scope_def_typ :: acc)
      scope.I.scope_defs []
    |> List.rev
  in
  let rec process_typ (acc : ModuleName.Set.t) = function
    | TLit _, _ -> acc
    | TTuple tl, _ -> List.fold_left process_typ acc tl
    | TStruct sname, _ ->
      let p = StructName.path sname in
      ModuleName.Set.add_seq (List.to_seq p) acc
    | TEnum ename, _ ->
      let p = EnumName.path ename in
      ModuleName.Set.add_seq (List.to_seq p) acc
    | TOption ty, _ -> process_typ acc ty
    | TArray ty, _ -> process_typ acc ty
    | TArrow _, _ -> raise (Unsupported "function type")
    | TDefault _, _ -> raise (Unsupported "default type")
    | TAny, _ -> raise (Unsupported "wildcard type")
    | TClosureEnv, _ -> raise (Unsupported "closure type")
  in
  List.fold_left process_typ ModuleName.Set.empty input_typs
  |> ModuleName.Set.elements
  |> List.map ModuleName.to_string

let get_scope_def (prg : I.program) (sc : I.scope) : O.scope_def =
  let decl_ctx = prg.program_ctx in
  let module_name =
    match prg.program_module_name with
    | Some (mname, _) -> ModuleName.to_string mname
    | None ->
      Format.ksprintf failwith "scope %s is not in a module"
        (ScopeName.to_string sc.scope_uid)
  in
  let info = ScopeName.Map.find sc.scope_uid decl_ctx.ctx_scopes in
  {
    O.name = ScopeName.to_string sc.scope_uid;
    module_name;
    inputs = scope_inputs decl_ctx sc;
    outputs = (get_struct decl_ctx info.out_struct_name).fields;
    module_deps = retrieve_scope_module_deps prg sc;
  }

let get_scope_test
    (prg : I.program)
    (testing_scope : string)
    (tested_scope : ScopeName.t) : O.test =
  let tested_scope =
    let modul =
      List.fold_left
        (fun _ m -> ModuleName.Map.find m prg.program_modules)
        prg.program_root
        (ScopeName.path tested_scope)
    in
    get_scope_def prg (ScopeName.Map.find tested_scope modul.module_scopes)
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
let read_program includes options = Driver.Passes.desugared options ~includes

let generate_test
    tested_scope
    ?(testing_scope = tested_scope ^ "_test")
    include_dirs
    options =
  let include_dirs = lookup_include_dirs ~include_dirs options in
  let prg, _ = read_program include_dirs options in
  let tested_scope =
    Ident.Map.find tested_scope prg.I.program_ctx.ctx_scope_index
  in
  let test = get_scope_test prg testing_scope tested_scope in
  print_test test

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
  let base_test =
    get_scope_test prg (ScopeName.to_string testing_scope_name) tested_scope
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
                ( (subscope_var, Pos.no_pos),
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
  let include_dirs = lookup_include_dirs ~include_dirs ?buffer_path options in
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

let rec print_catala_value ~lang ppf =
  let open Format in
  let strings = get_value_strings lang in
  function
  | O.Bool b ->
    pp_print_string ppf (if b then strings.true_str else strings.false_str)
  | O.Money m -> fprintf ppf strings.money_fmt (m / 100) (m mod 100)
  | O.Integer i -> pp_print_int ppf i
  | O.Decimal f ->
    let s = sprintf "%g" f in
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
  | O.Enum (_, (constr, Some v)) ->
    fprintf ppf "@[<hv 2>%s %s %a@]" constr strings.content_str
      (print_catala_value ~lang) v
  | O.Enum (_, (constr, None)) -> pp_print_string ppf constr
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
  match typ with
  | TBool -> Bool false
  | TInt -> Integer 0
  | TRat -> Decimal 0.
  | TMoney -> Money 0
  | TDate -> Date { year = 1970; month = 1; day = 1 }
  | TDuration -> Duration { years = 0; months = 0; days = 0 }
  | TTuple l -> Array (List.map generate_default_value l |> Array.of_list)
  | TStruct decl ->
    Struct
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
    Enum (decl, elt)
  | TOption typ -> generate_default_value typ
  | TArray _ -> Array [||]

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
  fprintf ppf "%s %s %s %s@," strings.output_scope sscope_var strings.scope
    t.tested_scope.name;
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

let run_test testing_scope include_dirs options =
  let include_dirs = lookup_include_dirs ~include_dirs options in
  let include_dirs : Global.raw_file list =
    List.map
      (fun (s : Global.raw_file) ->
        let open File in
        Global.raw_file ("_build" / (s :> string)))
      include_dirs
    @ include_dirs
  in
  let desugared_prg, naming_ctx = read_program include_dirs options in
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
    Interpreter.evaluate_expr dcalc_prg.decl_ctx dcalc_prg.lang program_fun
  in
  let _args, program_expr =
    match program_fun with
    | EAbs { binder; _ }, _ -> Bindlib.unmbind binder
    | _ -> assert false
  in
  let result_struct =
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
      (fun (field, value) ->
        ( StructField.to_string field,
          {
            O.value =
              Some { value = get_value dcalc_prg.decl_ctx value; pos = None };
            typ =
              get_typ dcalc_prg.decl_ctx (StructField.Map.find field out_struct);
          } ))
      results
  in
  let test = { test with test_outputs } in
  write_stdout Test_case_j.write_test test
