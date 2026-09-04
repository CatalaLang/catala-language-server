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

let _ =
  Clerk_backend.
    [
      OCaml.config_backend;
      Java.config_backend;
      C.config_backend;
      Python.config_backend;
    ]

let to_relative (p : File.t) = File.make_relative_to ~dir:(Sys.getcwd ()) p

let lookup_clerk_toml from_dir =
  let open Catala_utils in
  try
    begin
      let from_dir = File.make_absolute from_dir in
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

let lookup_include_dirs ?(prefix_build = false) ?buffer_path options =
  (* Otherwise, lookup for the toml *)
  let dir =
    match options.Global.input_src with
    | FileName file | Contents (_, file) -> Filename.dirname file
    | Stdin _ -> (
      match buffer_path with
      | None -> Sys.getcwd ()
      | Some buffer_path -> Filename.dirname buffer_path)
  in
  match lookup_clerk_toml dir with
  | None -> ".", []
  | Some (config, rel) ->
    let path_to_build = to_relative File.(dir / rel) in
    let all_include_dirs =
      match options.Global.input_src with
      | Stdin _ ->
        (* We add the test file directory as catala is unable to retrieve its
           dir *)
        List.sort_uniq String.compare
          (to_relative dir :: config.global.include_dirs)
      | _ -> config.global.include_dirs
    in
    let include_dirs =
      if prefix_build then
        List.map (fun p -> File.(path_to_build / "_build" / p)) all_include_dirs
      else List.map (File.( / ) path_to_build) all_include_dirs
    in
    let all_include_dirs =
      match options.Global.input_src with
      | Stdin _ ->
        (* We add the test file directory as catala is unable to retrieve its
           dir *)
        List.sort_uniq String.compare (to_relative dir :: include_dirs)
      | _ -> include_dirs
    in
    Message.debug "@[<h>Found %s dirs:@ %a@]"
      (if prefix_build then "build" else "include")
      Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
      all_include_dirs;
    path_to_build, List.map Global.raw_file all_include_dirs

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
    | None -> None
    | Some (_config, rel) -> Some (to_relative File.(dir / rel))
  in
  match options.Global.input_src with
  | FileName file | Contents (_, file) -> f (Filename.dirname file)
  | Stdin _ -> (
    match buffer_path with
    | None -> f (Sys.getcwd ())
    | Some buffer_path -> f (Filename.dirname buffer_path))

exception Unsupported of string

let unsupported fmt = Format.ksprintf (fun msg -> raise (Unsupported msg)) fmt

let implicit_stdlib_aliases, lookup_aliased_name =
  let en_names =
    [
      "Date";
      "Duration";
      "MonthYear";
      "Period";
      "Money";
      "Integer";
      "Decimal";
      "List";
    ]
  in
  let en_aliases = List.map (fun s -> s ^ "_en") en_names in
  let fr_aliases = List.map (fun s -> s ^ "_fr") en_names in
  let fr_names =
    [
      "Date";
      "Durée";
      "MoisAnnée";
      "Période";
      "Argent";
      "Entier";
      "Décimal";
      "Liste";
    ]
  in
  let en_implicit_aliases = en_names @ en_aliases in
  let fr_implicit_aliases = fr_names @ fr_aliases in
  let implicit_stdlib_aliases = function
    | `En -> en_implicit_aliases
    | `Fr -> fr_implicit_aliases
    | `Pl -> en_implicit_aliases
    | _ -> []
  in
  let en_alias_map = String.Map.of_list (List.combine en_aliases en_names) in
  let fr_alias_map = String.Map.of_list (List.combine fr_aliases fr_names) in
  let lookup_aliased_name lang s =
    match lang with
    | `En -> String.Map.find_opt s en_alias_map
    | `Fr -> String.Map.find_opt s fr_alias_map
    | `Pl -> String.Map.find_opt s en_alias_map
    | _ -> None
  in
  implicit_stdlib_aliases, lookup_aliased_name

let is_implicit_stdlib_alias lang alias =
  List.exists (fun a -> String.equal a alias) (implicit_stdlib_aliases lang)

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
        "Fichier écrit par l’éditeur de tests métier Catala. Ne pas modifier à la main.";
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
  function
  | `Fr -> fr_strings
  | `En -> en_strings
  | `Pl -> pl_strings
  | _ -> unsupported "unsupported language"

(* Runtime names, as an ordinary read produces them and the editor's option
   form expects them. The surface keyword is the writer's business. *)
let option_absent = EnumConstructor.to_string ConstantNames.none_constr
let option_present = EnumConstructor.to_string ConstantNames.some_constr

let mk_optional_enum_decl typ =
  {
    O.enum_name = EnumName.to_string ConstantNames.option_enum;
    constructors = [option_absent, None; option_present, Some typ];
    ctor_attrs = [];
  }

let get_typ_literal = function
  | TBool -> O.TBool
  | TUnit -> raise (Unsupported "unit type")
  | TInt -> O.TInt
  | TRat -> O.TRat
  | TMoney -> O.TMoney
  | TDate -> O.TDate
  | TDuration -> O.TDuration
  | TPos -> assert false

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

(** Default placeholder for uninitialized inputs: empty array for TArray,
    explicit Unset for everything else. *)
let unset_default_value (typ : O.typ) : O.value_def =
  let value =
    match typ with
    | TArray _ -> { O.value = O.Array [||]; attrs = [] }
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
    List.map
      (fun (v, (si : O.scope_input)) ->
        let default =
          if si.is_context then context_var_default
          else unset_default_value si.typ
        in
        v, { O.typ = si.typ; value = Some default })
      tested_scope.inputs
  in
  let test_outputs =
    List.map (fun (v, typ) -> v, { O.typ; value = None }) tested_scope.outputs
  in
  let description = "" in
  let title = tested_scope.name in
  {
    O.testing_scope;
    tested_scope;
    test_outputs;
    test_inputs;
    description;
    title;
  }

(* --- *)

let write_stdout f arg =
  let buf = Buffer.create 4096 in
  f buf arg;
  Buffer.output_buffer stdout buf

let print_test test = write_stdout J.write_test test
let print_tests test = write_stdout J.write_test_list test

let read_program includes path_to_build options =
  let stdlib =
    Some (Global.raw_file File.(path_to_build / "_build" / "libcatala"))
  in
  let prg, ctx = Driver.Passes.desugared options ~stdlib ~includes in
  let prg = Desugared.Disambiguate.program prg in
  prg, ctx

let rec generate_default_value lang (typ : O.typ) : O.runtime_value =
  let value =
    match typ with
    | TBool -> O.Bool false
    | TInt -> O.Integer 0
    | TRat -> O.Decimal 0.
    | TMoney -> O.Money 0
    | TDate -> O.Date { year = 2000; month = 1; day = 1 }
    | TDuration -> O.Duration { years = 0; months = 0; days = 0 }
    | TTuple l ->
      O.Array (List.map (generate_default_value lang) l |> Array.of_list)
    | TStruct decl ->
      O.Struct
        ( decl,
          List.map
            (fun (s, t) -> s, (generate_default_value lang) t)
            decl.fields )
    | TEnum decl ->
      let elt =
        let cn, ty =
          List.find_opt
            (function _, None -> true | _ -> false)
            decl.constructors
          |> function Some s -> s | None -> List.hd decl.constructors
        in
        cn, Option.map (generate_default_value lang) ty
      in
      O.Enum (decl, elt)
    | TOption typ -> Enum (mk_optional_enum_decl typ, (option_absent, None))
    | TArray _ -> O.Array [||]
    | TUnset -> O.Unset
    | TUnit -> raise (Unsupported "unit type")
    | TArrow _ -> raise (Unsupported "arrow type")
  in
  { value; attrs = [] }

let patch_paths
    (modl : ModuleName.t)
    ({ tested_scope; test_inputs; test_outputs; _ } as test : O.test) =
  let open O in
  let patch_name s =
    if String.contains s '.' then s
    else Format.sprintf "%s.%s" (ModuleName.to_string modl) s
  in
  let rec patch_enum_decl = function
    | { O.enum_name; constructors; ctor_attrs } ->
      {
        enum_name = patch_name enum_name;
        constructors =
          List.map (fun (c, t) -> c, Option.map patch_typ t) constructors;
        ctor_attrs;
      }
  and patch_struct_decl = function
    | { struct_name; fields } ->
      {
        struct_name = patch_name struct_name;
        fields = List.map (fun (fl, t) -> fl, patch_typ t) fields;
      }
  and patch_typ : O.typ -> O.typ =
   fun t ->
    match t with
    | TBool | TInt | TRat | TMoney | TDate | TDuration | TUnit | TUnset -> t
    | TTuple l -> TTuple (List.map patch_typ l)
    | TStruct sdecl -> TStruct (patch_struct_decl sdecl)
    | TEnum edecl -> TEnum (patch_enum_decl edecl)
    | TOption t -> TOption (patch_typ t)
    | TArray t -> TArray (patch_typ t)
    | TArrow (tl, t) -> TArrow (List.map patch_typ tl, patch_typ t)
  in
  let rec patch_value : O.runtime_value -> O.runtime_value =
   fun ({ value; attrs } as v) ->
    match value with
    | O.Bool _ | O.Money _ | O.Integer _ | O.Decimal _ | O.Date _ | O.Duration _
    | O.Empty | O.Unset | O.NotOverridden ->
      v
    | O.Enum (enum_decl, (cstr, rv_opt)) ->
      {
        value =
          O.Enum
            (patch_enum_decl enum_decl, (cstr, Option.map patch_value rv_opt));
        attrs;
      }
    | O.Struct (struct_decl, fields) ->
      {
        value =
          O.Struct
            ( patch_struct_decl struct_decl,
              List.map (fun (fl, v) -> fl, patch_value v) fields );
        attrs;
      }
    | O.Array t -> { value = O.Array (Array.map patch_value t); attrs }
  in
  let patch_value_def (x : O.value_def) =
    { x with value = patch_value x.value }
  in
  let patch_test_io : O.test_io -> O.test_io =
   fun { typ; value } ->
    { typ = patch_typ typ; value = Option.map patch_value_def value }
  in
  let tested_scope =
    {
      tested_scope with
      inputs =
        List.map
          (fun (x, (si : O.scope_input)) ->
            x, { si with typ = patch_typ si.typ })
          tested_scope.inputs;
      outputs = List.map (fun (x, t) -> x, patch_typ t) tested_scope.outputs;
    }
  in
  let test_inputs = List.map (fun (x, io) -> x, patch_test_io io) test_inputs in
  let test_outputs =
    List.map (fun (x, io) -> x, patch_test_io io) test_outputs
  in
  { test with tested_scope; test_inputs; test_outputs }

let generate_test
    tested_scope
    ?(enforce_module = true)
    ?(testing_scope = tested_scope ^ "_test")
    ?(with_default_values = false)
    ?program
    include_dirs
    options =
  (* [program]: a caller that already read the module hands it over. *)
  let prg, _ =
    match program with
    | Some p -> p
    | None ->
      let path_to_build, include_dirs =
        if include_dirs = [] then lookup_include_dirs options
        else ".", include_dirs
      in
      read_program include_dirs path_to_build options
  in
  let tested_scope =
    Ident.Map.find tested_scope prg.I.program_ctx.ctx_scope_index
  in
  let tested_module =
    if enforce_module then Option.map fst prg.I.program_module_name
    else
      Option.map fst prg.I.program_module_name
      |> function
      | None -> Some (ModuleName.fresh ("no_module", Pos.void))
      | Some m -> Some m
  in
  let test = get_scope_test prg testing_scope tested_scope ~tested_module in
  let test =
    (* As our root module is not the test file but the scope's file (which is
       not the case for read), qualified name do not have the expected module
       set. We patch types to retroactively add it so that the test structure is
       fully operational. *)
    Option.map (fun modl -> patch_paths modl test) tested_module
    |> Option.value ~default:test
  in
  if with_default_values then
    let test_inputs =
      List.map
        (fun (s, (io : O.test_io)) ->
          let is_context =
            List.assoc_opt s test.tested_scope.inputs
            |> Option.map (fun (si : O.scope_input) -> si.is_context)
            |> Option.value ~default:false
          in
          ( s,
            O.
              {
                io with
                value =
                  Some
                    (if is_context then context_var_default
                     else
                       {
                         value = generate_default_value prg.program_lang io.typ;
                         pos = None;
                       });
              } ))
        test.test_inputs
    in
    { test with test_inputs }
  else test

let generate_cmd
    tested_scope
    ?testing_scope
    include_dirs
    options
    with_default_values
    enforce_module =
  print_tests
    [
      generate_test ~with_default_values tested_scope ?testing_scope
        include_dirs options ~enforce_module;
    ]

exception InvalidTestingScope of string

let invalid_testing_scope fmt =
  Format.kasprintf (fun msg -> raise (InvalidTestingScope msg)) fmt

(* note: filters for both 'test' and 'testUI' attrs *)
let get_test_scopes prg =
  prg.I.program_root.module_scopes
  |> ScopeName.Map.filter (fun scope_name _scope ->
      Pos.has_attr (Mark.get (ScopeName.get_info scope_name)) Test
      && Pos.has_attr (Mark.get (ScopeName.get_info scope_name)) TestUi)
  |> ScopeName.Map.keys

(* `#[test]` without `#[testcase.testui]`: hand-written, may hold anything.
   The editor can only express literals and equalities, so writing such a file
   back would delete it. Must agree with [surface_scopes_by_ownership]. *)
let unowned_test_scopes prg =
  prg.I.program_root.module_scopes
  |> ScopeName.Map.filter (fun scope_name _scope ->
      Pos.has_attr (Mark.get (ScopeName.get_info scope_name)) Test
      && not (Pos.has_attr (Mark.get (ScopeName.get_info scope_name)) TestUi))
  |> ScopeName.Map.keys

(* Raw newlines in a pre-joined string escape the message box. *)
let error_mixed_ownership (unowned : string list) =
  Message.error
    "this file mixes tests the editor owns with tests written by \
     hand:@\n@[<v 2>  %a@]@\n\
     The editor can express only literal values and equalities, so reading it \
     would drop whatever else those tests contain, and writing it back would \
     delete them. Mark them `#[testcase.testui]` to hand them over, or keep \
     them in a file of their own."
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut Format.pp_print_string)
    unowned

let get_catala_test (prg, naming_ctx) testing_scope_name =
  let testing_scope =
    ScopeName.Map.find testing_scope_name prg.I.program_root.module_scopes
  in
  let info = Mark.get (ScopeName.get_info testing_scope_name) in
  let get_single_attr ~default pos f =
    match Pos.get_attrs pos f with [] -> default | x :: _ -> x
  in
  let description =
    get_single_attr ~default:"" info (function
      | TestDescription s -> Some s
      | _ -> None)
  in
  let title =
    get_single_attr ~default:"" info (function
      | TestTitle s -> Some s
      | _ -> None)
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
      (fun (var_str, (test_in : O.test_io)) ->
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
          let is_context =
            List.assoc_opt var_str base_test.tested_scope.inputs
            |> Option.map (fun (si : O.scope_input) -> si.is_context)
            |> Option.value ~default:false
          in
          match rules with
          | [] ->
            Some
              (if is_context then context_var_default
               else unset_default_value test_in.O.typ)
          | [(_, rule)] ->
            let e = Expr.unbox_closed rule.rule_cons in
            let value = get_value prg.program_lang prg.program_ctx e in
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
                O.value = get_value prg.program_lang prg.program_ctx value;
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
  { base_test with O.test_inputs; test_outputs; description; title }

let import_catala_tests (prg, naming_ctx) =
  List.map (get_catala_test (prg, naming_ctx)) (get_test_scopes prg)

(* Does [v] still inhabit [t]? (recovery of drifted tests). *)
let rec value_fits (t : O.typ) (v : O.runtime_value) : (unit, string) Result.t =
  let path segs msg =
    Error (if segs = "" then msg else Printf.sprintf "%s%s" segs msg)
  in
  let mismatch expected got = path "" (Printf.sprintf ": expected %s, got %s" expected got) in
  let under seg = function
    | Ok () -> Ok ()
    | Error e -> Error (seg ^ e)
  in
  match t, v.O.value with
  (* Value-less, so nothing to check. *)
  | _, (O.Unset | O.NotOverridden | O.Empty) -> Ok ()
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
    | None -> Ok ()
    | Some p -> under (Printf.sprintf ".%s" ctor) (value_fits ot p))
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
        path ("." ^ n) (Printf.sprintf ": not a field of %s" d.O.struct_name)))
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
                      (Printf.sprintf "%s: %s.%s%s" t.O.testing_scope where name msg)))
              record)
          [ "in", t.O.test_inputs; "out", t.O.test_outputs ])
      tests
  in
  if problems = [] then Ok () else Error problems

let read_test include_dirs (options : Global.options) buffer_path =
  let path_to_build, include_dirs =
    if include_dirs = [] then lookup_include_dirs ?buffer_path options
    else ".", include_dirs
  in
  let prg = read_program include_dirs path_to_build options in
  (match unowned_test_scopes (fst prg), get_test_scopes (fst prg) with
  | (_ :: _ as unowned), _ :: _ ->
    error_mixed_ownership (List.map ScopeName.to_string unowned)
  | _ -> ());
  let tests = import_catala_tests prg in
  match check_tests_fit tests with
  | Ok () -> write_stdout J.write_test_list tests
  | Error problems ->
    Message.error
      "this test no longer fits the scope it targets:@\n@[<v 2>  %a@]@\n\
       Its values were written against a different signature. Use@ \
       `catala testcase partial-read` to read them as authored."
      (Format.pp_print_list ~pp_sep:Format.pp_print_cut
         (fun ppf s -> Format.pp_print_string ppf s))
      problems

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
        when p = "testcase" :: path -> Some v
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
      if surface_has_attr [ "testcase"; "testui" ] m then Either.Left name
      else Either.Right name)
    (List.filter
       (fun ((decl : Surface.Ast.scope_decl), _) ->
         surface_has_attr [ "test" ] (Mark.get decl.scope_decl_name))
       (surface_scope_decls prg))

(* A bare constructor names no enum. Never a name; never printed as one. *)
let unknown_enum_name = "unknown"

let enum_name_of_path (p : Surface.Ast.path) =
  match p with
  | [] -> unknown_enum_name
  | p -> String.concat "." (List.map Mark.remove p)

let read_partial_test_one (scope_decl : Surface.Ast.scope_decl)
    (scope_use : Surface.Ast.scope_use) : (O.test, string) Result.t =
  let ( let*? ) = Result.bind in
  let open Surface.Ast in
  let vlit v = Some O.{ value = v; pos = None } in
  let exception Err of string in
  let rec convert_literal (e : expression) :
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
    (* `Mod.Enum.Ctor` carries the enum's full name, spelled as an ordinary
       read spells it. Bare, it names no enum. *)
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
      ok
        (O.TStruct struct_decl)
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
    | _ | (exception _) -> Error "unsupported expression"
  in
  let convert_definition : definition -> (string * O.test_io, string) Result.t =
    function
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
     literal is recognised; anything richer is left alone. *)
  let rec convert_assertion (e : expression) : (string * O.test_io) option =
    match Mark.remove e with
    (* `assertion (x = y)` carries its parentheses into the tree. *)
    | Paren inner -> convert_assertion inner
    | Binop ((Eq, _), lhs, rhs) -> (
      match Mark.remove lhs with
      | Dotted (_, ((_path, (field, _)), _)) -> (
        match convert_literal rhs with
        | Ok (typ, rv) -> Some (field, O.{ typ; value = vlit rv })
        | Error _ -> None)
      | _ -> None)
    | _ -> None
  in
  let convert_var_def = function
    | Definition d, _ -> begin
      match convert_definition d with
      | Error s -> raise (Err s)
      | Ok v -> Some v
    end
    | _ -> None
  in
  let*? test_inputs =
    try Ok (List.filter_map convert_var_def scope_use.scope_use_items)
    with Err s -> Error s
  in
  let recovered_outputs =
    List.filter_map
      (function Assertion e, _ -> convert_assertion e | _ -> None)
      scope_use.scope_use_items
  in
  let testing_scope = fst scope_decl.scope_decl_name in
  let*? tested_scope =
    List.find_map
      (function
        | ( ContextScope
              { scope_decl_context_scope_sub_scope = (p, (sname, _)), _; _ },
            _ ) ->
          Some
            {
              O.name = sname;
              module_name = List.hd (List.rev p) |> fst;
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
            }
        | _ -> None)
      scope_decl.scope_decl_context
    |> function None -> Error "tested scope not found" | Some v -> Ok v
  in
  Ok
    {
      O.testing_scope;
      tested_scope;
      test_inputs;
      test_outputs = recovered_outputs;
      description =
        Option.value ~default:""
          (surface_attr_string [ "test_description" ]
             (Mark.get scope_decl.scope_decl_name));
      title =
        Option.value ~default:""
          (surface_attr_string [ "test_title" ]
             (Mark.get scope_decl.scope_decl_name));
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
            List.find_opt
              (fun (u : Surface.Ast.scope_use) ->
                String.equal (Mark.remove u.scope_use_name) name)
              uses
          with
          | None -> None (* declared but never defined: not a test *)
          | Some use ->
            Some (name, read_partial_test_one decl use))
        decls
    in
    let tests =
      List.filter_map (function _, Ok t -> Some t | _ -> None) results
    in
    let errors =
      List.filter_map
        (function n, Error e -> Some (Printf.sprintf "%s: %s" n e) | _ -> None)
        results
    in
    if tests = [] && errors <> [] then Error (String.concat "; " errors)
    else Ok (tests, errors)

(* Rebuilding a broken test: one command producing everything the recovery
   view needs; none of this reasoning lives on the TypeScript side. *)

(* How a recovered value may be carried into a live slot. Ordered by how much
   each rule claims: [Wrap]/[Unwrap] only that an option and its payload
   correspond, [Fits] only what an ordinary read checks. Anything that infers
   intent belongs behind explicit consent -- a wrong carry is worse than an
   empty field, because nobody re-checks a field that looks answered. Pinned by
   the table in test/carry_test.ml. *)
(* Re-describe a value with the live type's declarations: a recovered one is
   inferred from a single literal (one constructor of a hundred, only the
   fields the test wrote, [unknown_enum_name]). Attributes are the value's own
   and stay. *)
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
        | _ -> payload
      in
      O.Enum (d, (ctor, payload))
    | O.TStruct d, O.Struct (_, fields) ->
      (* In declaration order, as an ordinary read has them: the same test
         must write the same bytes whichever reader it came through. *)
      let declared =
        List.filter_map
          (fun (n, ft) ->
            Option.map (fun fv -> n, adopt_typ ft fv) (List.assoc_opt n fields))
          d.O.fields
      in
      let undeclared =
        List.filter (fun (n, _) -> not (List.mem_assoc n d.O.fields)) fields
      in
      O.Struct (d, declared @ undeclared)
    | O.TArray et, O.Array elems ->
      O.Array (Array.map (adopt_typ et) elems)
    | O.TTuple ts, O.Array elems when List.length ts = Array.length elems ->
      O.Array (Array.mapi (fun i e -> adopt_typ (List.nth ts i) e) elems)
    | _ -> v.O.value
  in
  { v with O.value }

let carry_rule ~(old_typ : O.typ) ~(new_typ : O.typ) (v : O.runtime_value)
    : O.runtime_value option * O.carry_outcome =
  match new_typ, old_typ with
  (* [TUnset] is absence of evidence, not a differing type. First, because
     [value_fits] accepts [Unset] against anything. *)
  | _, O.TUnset -> None, O.WasUnset
  | O.TOption inner, _ when inner = old_typ ->
    let decl = mk_optional_enum_decl inner in
    ( Some
        {
          O.value = O.Enum (decl, (option_present, Some v));
          attrs = [];
        },
      O.Wrap )
  (* Option to option is neither: the fits check decides. *)
  | _, O.TOption inner when not (match new_typ with O.TOption _ -> true | _ -> false)
    -> (
    match v.O.value with
    | O.Enum (_, (_, Some payload)) when inner = new_typ -> Some payload, O.Unwrap
    | O.Enum (_, (_, None)) -> None, O.WasAbsentNowRequired
    | _ -> None, O.TypeChanged (old_typ, new_typ))
  (* Not type equality: a recovered type is inferred from one literal and never
     equals the live one, even when nothing changed. [Fits] promises exactly
     what an ordinary read checks. *)
  | _ when value_fits new_typ v = Ok () -> Some v, O.Fits
  | _ -> None, O.TypeChanged (old_typ, new_typ)

let carry_value ~(old_typ : O.typ) ~(new_typ : O.typ) (v : O.runtime_value) :
    O.runtime_value option * O.carry_outcome =
  let carried, outcome = carry_rule ~old_typ ~new_typ v in
  Option.map (adopt_typ new_typ) carried, outcome

(* The compiler's own diagnostic, as text: the one thing that gets the tester
   out. *)
let error_text (e : exn) : string =
  let of_content c =
    Message.pp_to_string ~ansi:false (fun ppf ->
        Message.Content.emit ~ppf c Message.Error)
  in
  match e with
  | Message.CompilerError c -> of_content c
  | Message.CompilerErrors ((c, _) :: _) -> of_content c
  | Unsupported msg -> "unsupported: " ^ msg
  | e -> Printexc.to_string e

let levenshtein (a : string) (b : string) : int =
  let la = String.length a and lb = String.length b in
  let prev = Array.init (lb + 1) Fun.id and cur = Array.make (lb + 1) 0 in
  for i = 1 to la do
    cur.(0) <- i;
    for j = 1 to lb do
      let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      cur.(j) <- min (min (prev.(j) + 1) (cur.(j - 1) + 1)) (prev.(j - 1) + cost)
    done;
    Array.blit cur 0 prev 0 (lb + 1)
  done;
  prev.(lb)

(* The nearest directory holding a clerk.toml, else [from_dir]. *)
let project_root (from_dir : string) : string =
  let rec up dir n =
    if n = 0 then dir
    else if Sys.file_exists (Filename.concat dir "clerk.toml") then dir
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then dir else up parent (n - 1)
  in
  up from_dir 12

(* Every Catala source under [dir]. [File.scan_tree] skips hidden and
   [_]-prefixed entries, the same convention as clerk's own discovery. *)
let catala_files_under (dir : string) : string list =
  let catala_file f =
    let e = Filename.extension f in
    if String.length e > 8 && String.starts_with ~prefix:".catala_" e then
      Some f
    else None
  in
  File.scan_tree catala_file dir
  |> Seq.concat_map (fun (_, _, files) -> List.to_seq files)
  |> List.of_seq

(* The name in a `> Module NAME` line, if the file declares one. *)
let module_decl_re =
  Re.(compile (seq [bol; str "> Module "; group (rep1 (compl [space]))]))

let declared_module (content : string) : string option =
  Option.map
    (fun g -> Re.Group.get g 1)
    (Re.exec_opt module_decl_re content)

(* The file declaring [> Module name], searched from the test outwards. Clerk
   cannot resolve from a file that no longer typechecks. *)
let find_module_file (name : string) (from_dir : string) : string option =
  let declares f =
    try declared_module (File.contents f) = Some name with _ -> false
  in
  (* Beside the test first: that is where a module almost always is. *)
  match List.find_opt declares (catala_files_under from_dir) with
  | Some f -> Some f
  | None -> List.find_opt declares (catala_files_under (project_root from_dir))

(* Scopes anywhere in the project that declare some of the test's field names,
   for a test whose module is gone. Surface-parsed only: no module need
   compile, and none but the one chosen ever will. *)
let rank_project_scope_candidates ~(from_dir : string) ~(wanted_module : string)
    ~(field_names : string list) : O.scope_candidate list =
  let total = List.length field_names in
  let mentions_a_field content =
    List.exists (fun f -> Re.execp (Re.compile (Re.str f)) content) field_names
  in
  let candidates_of file =
    match File.contents file with
    | exception _ -> []
    | content -> (
      match declared_module content with
      | None -> []
      | Some _ when not (mentions_a_field content) -> []
      | Some module_name -> (
        match
          Driver.Passes.surface
            (Global.enforce_options ~input_src:(Global.FileName file)
               ~language:(Some (Cli.file_lang file)) ())
        with
        | exception e ->
          Message.debug "candidates: skipping %s: %s" file (Printexc.to_string e);
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
  catala_files_under (project_root from_dir)
  |> List.concat_map candidates_of
  |> List.sort (fun (a : O.scope_candidate) (b : O.scope_candidate) ->
         match compare b.shared a.shared with
         | 0 -> (
           match
             compare
               (levenshtein wanted_module a.module_name)
               (levenshtein wanted_module b.module_name)
           with
           | 0 -> compare (a.module_name, a.name) (b.module_name, b.name)
           | c -> c)
         | c -> c)
  |> List.filteri (fun i _ -> i < 8)

(* "Scope" or "Module.Scope". *)
let parse_target (t : string) : string option * string =
  match String.rindex_opt t '.' with
  | Some i -> Some (String.sub t 0 i), String.sub t (i + 1) (String.length t - i - 1)
  | None -> None, t

(* An ordinary read, or None. For the working copy: `.updated` keeps clerk
   away from it, and also hides the language, hence [~lang]. *)
let read_tests_of_file ~(lang : Global.backend_lang) (file : string) :
    O.test list option =
  if not (Sys.file_exists file) then None
  else
    match
      let options =
        Global.enforce_options ~input_src:(Global.FileName file) ~language:(Some lang) ()
      in
      let path_to_build, include_dirs = lookup_include_dirs options in
      import_catala_tests (read_program include_dirs path_to_build options)
    with
    | tests -> Some tests
    | exception _ -> None

(* The module's scopes, ranked by shared field names then name distance.
   Ranked, not chosen: the tester picks. Same scopes as [list_scopes]. *)
let rank_scope_candidates (prg : I.program) ~(wanted : string)
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
             compare (levenshtein wanted a.name) (levenshtein wanted b.name)
           with
           | 0 -> compare a.name b.name
           | c -> c)
         | c -> c)

let rebuild_broken_test (options : Global.options) (target : string option) =
  let test_file = Global.input_src_file options.Global.input_src in
  let lang = Cli.file_lang test_file in
  let notes = ref [] in
  let note n = notes := n :: !notes in
  let workspace_file = Filename.basename test_file ^ ".updated" in
  let emit recovered rebuilt carried =
    write_stdout J.write_recovery
      {
        O.original = recovered;
        rebuilt = rebuilt;
        notes = List.rev !notes;
        working_copy = workspace_file;
        carry_outcomes = carried;
      }
  in
  (match surface_scopes_by_ownership (Driver.Passes.surface options) with
  | _ :: _, (_ :: _ as unowned) -> error_mixed_ownership unowned
  | _ -> ());
  match read_partial_tests options with
  | Error e -> Format.ksprintf failwith "Error: %s" e
  | Ok (recovered, errors) -> (
    List.iter (fun e -> Message.warning "partial read: %s" e) errors;
    match recovered with
    | [] -> emit [] [] []
    | first :: _ -> (
      let scope = first.O.tested_scope in
      let workspace =
        Filename.concat (Filename.dirname test_file) workspace_file
      in
      let saved = read_tests_of_file ~lang workspace in
      (* Explicit choice, else the scope a saved working copy targets, else
         the declared one. *)
      let target_module, target_name =
        match target, saved with
        | Some t, _ -> parse_target t
        | None, Some (t :: _) ->
          Some t.O.tested_scope.O.module_name, t.O.tested_scope.O.name
        | None, _ -> None, scope.O.name
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
        emit recovered [] []
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
          note (O.ModuleWontCompile { O.name = module_name; error = error_text e });
          emit recovered [] []
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
          emit recovered [] []
        | program -> (
        match generate_test target_name ~program [] module_options with
        | exception e ->
          note
            (O.Other
               {
                 O.name = module_name ^ "." ^ target_name;
                 error = error_text e;
               });
          emit recovered [] []
        | live ->
          let carried = ref [] in
          let record test io field outcome =
            carried :=
              { O.testing_scope = test; field; io; outcome }
              :: !carried
          in
          (* One entry per live field, so every blank field is explained.
             [when_missing] is what a field absent from the authored test
             means: an input was left unset, but an output was simply never
             asserted -- a healthy test, nothing to report. *)
          let carry_record ~io ~when_missing (t : O.test)
              (old_record : (string * O.test_io) list)
              (live_record : (string * O.test_io) list) =
            List.map
              (fun (name, (live_io : O.test_io)) ->
                match List.assoc_opt name old_record with
                | Some { O.value = Some vd; typ = old_typ } -> (
                  match
                    carry_value ~old_typ ~new_typ:live_io.typ vd.O.value
                  with
                  | Some v, outcome ->
                    record t.O.testing_scope io name outcome;
                    ( name,
                      { live_io with O.value = Some { O.value = v; pos = None } }
                    )
                  | None, outcome ->
                    record t.O.testing_scope io name outcome;
                    name, live_io)
                | _ ->
                  Option.iter (record t.O.testing_scope io name) when_missing;
                  name, live_io)
              live_record
          in
          let rebuilt =
            List.map
              (fun (t : O.test) ->
                {
                  live with
                  O.testing_scope = t.O.testing_scope;
                  title = t.O.title;
                  description = t.O.description;
                  test_inputs =
                    carry_record ~io:O.In ~when_missing:(Some O.WasUnset) t
                      t.O.test_inputs live.O.test_inputs;
                  test_outputs =
                    carry_record ~io:O.Out ~when_missing:None t
                      t.O.test_outputs live.O.test_outputs;
                })
              recovered
          in
          (* A saved working copy wins over a fresh rebuild. It targets the
             live scope, so an ordinary read works. A mark is answered once the
             field holds a value. *)
          let rebuilt, carried =
            match saved with
            | None | Some [] -> rebuilt, List.rev !carried
            | Some saved ->
              (* Which record answers a mark depends on the mark's side. *)
              let still_blank scope side field =
                match
                  List.find_opt
                    (fun (t : O.test) -> t.O.testing_scope = scope)
                    saved
                with
                | None -> true
                | Some t -> (
                  let record =
                    match side with
                    | O.In -> t.O.test_inputs
                    | O.Out -> t.O.test_outputs
                  in
                  match
                    List.assoc_opt field record
                    |> Option.fold ~none:None ~some:(fun (io : O.test_io) -> io.value)
                  with
                  | None -> true
                  | Some vd -> vd.O.value.O.value = O.Unset)
              in
              ( saved,
                List.rev !carried
                |> List.filter (fun (r : O.carry_record) ->
                       match r.O.outcome with
                       | O.WasUnset | O.TypeChanged _ | O.WasAbsentNowRequired ->
                         still_blank r.O.testing_scope r.O.io r.O.field
                       | _ -> true) )
          in
          emit recovered rebuilt carried))))

let read_partial_test (options : Global.options) =
  match read_partial_tests options with
  | Ok (tests, errors) ->
    List.iter (fun e -> Message.warning "partial read: %s" e) errors;
    write_stdout J.write_test_list tests
  | Error s -> Format.ksprintf failwith "Error: %s" s

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
  | _, O.Enum ({ enum_name = "Optional"; constructors; _ }, (constr, v)) ->
    if v = None then pp_print_string ppf strings.absent
    else
      let payload_typ =
        match typ with
        | Some (O.TOption inner) -> Some inner
        | _ -> List.assoc constr constructors
      in
      fprintf ppf "%s %s %a" strings.present strings.content_str
        (print_catala_value ~typ:payload_typ ~lang)
        (Option.get v)
  | Some (TEnum { enum_name; constructors; _ }), O.Enum (_en, (constr, Some v))
    when enum_name <> unknown_enum_name ->
    fprintf ppf "@[<hv 2>%s.%s %s %a@]" enum_name constr strings.content_str
      (print_catala_value ~typ:(List.assoc constr constructors) ~lang)
      v
  (* Name unknown: written bare, for Catala to infer as it did the first time. *)
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
  | _, O.Empty -> assert false

let print_catala_value_opt ~lang ppf (t_in : O.test_io) =
  let typ = t_in.typ in
  match t_in.O.value, typ with
  | Some { value = { value = O.Unset; _ }; _ }, TArray _ | None, TArray _ ->
    Format.fprintf ppf "[]"
  | Some { value = { value = O.Unset; _ }; _ }, _ | None, _ ->
    Format.fprintf ppf "impossible"
  | Some { value; _ }, typ -> print_catala_value ~typ:(Some typ) ~lang ppf value

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
  fprintf ppf "#[testcase.testui]@\n";
  fprintf ppf "#[testcase.test_description = %s]@\n"
    (String.quote t.description);
  fprintf ppf "#[testcase.test_title = %s]@\n" (String.quote t.title);
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
    J.read_test_list (Yojson.init_lexer ()) (Lexing.from_channel stdin)
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
  Format.fprintf ppf "%s@\n@\n" (get_lang_strings lang).header;
  let _opened =
    List.fold_left
      (fun opened test ->
        Format.pp_open_vbox ppf 0;
        let opened =
          let modules_to_open0 =
            Ident.Set.(
              diff
                (of_list
                   (test.O.tested_scope.module_name
                   :: test.O.tested_scope.module_deps))
                opened)
          in
          (* Filter out implicit stdlib aliases from Using lines. TODO: remove
             once the compiler provides active imports for the target module, so
             we can decide this precisely. *)
          let modules_to_open =
            Ident.Set.fold
              (fun m acc ->
                if is_implicit_stdlib_alias lang m then acc
                else Ident.Set.add m acc)
              modules_to_open0 Ident.Set.empty
          in
          Ident.Set.iter
            (fun modname ->
              Format.fprintf ppf "> %s %s@,"
                (get_lang_strings lang).using_module modname)
            modules_to_open;
          let opened' =
            String.Set.of_list (Ident.Set.elements modules_to_open)
          in
          String.Set.union opened' opened
        in
        write_catala_test ppf test lang;
        Format.pp_close_box ppf ();
        opened)
      String.Set.empty tests
  in
  ()

(* The tested scope's assertions only: another test's expectations on the same
   field names are not this test's. *)
let retrieve_assertions_values (dcalc_prg : typed Dcalc.Ast.program)
    (scope : ScopeName.t) : (StructField.t * (dcalc, typed) gexpr) list =
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
      | _, ScopeDef (name, _) when not (ScopeName.equal name scope) -> acc
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

let retrieve_program include_dirs options scope_name =
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
      Ident.Map.find_opt scope_name
        Desugared.Name_resolution.(naming_ctx.local.typedefs)
    with
    | Some (TScope (sname, _)) -> sname
    | _ -> Message.error "No scope %S was found in the program" scope_name
  in
  let dcalc_prg : ((dcalc, dcalc, typed) base_gexpr * typed mark) program =
    let prg =
      Scopelang.From_desugared.(
        translate_program desugared_prg (build_exceptions_graph desugared_prg))
    in
    let prg = Scopelang.Ast.type_program prg in
    Dcalc.From_scopelang.translate_program prg
  in
  desugared_prg, naming_ctx, testing_scope_name, dcalc_prg

let rec convert_atd_to_runtime_value : O.runtime_value -> Catala_runtime.Value.t
    =
 fun v ->
  let open Catala_runtime in
  let open Value in
  match v.value with
  | O.Bool b -> V (Bool, b)
  | Money m -> V (Money, Z.of_int m)
  | Integer i -> V (Integer, Z.of_int i)
  | Decimal d -> V (Decimal, Q.of_float d)
  | Date { year; month; day } -> V (Date, Dates_calc.make_date ~year ~month ~day)
  | Duration { years; months; days } ->
    V (Duration, Dates_calc.make_period ~years ~months ~days)
  | Enum (decl, (cstr_s, v_opt)) ->
    let v = Option.map convert_atd_to_runtime_value v_opt in
    let index =
      List.mapi (fun i (name, _) -> name, i) decl.constructors
      |> List.assoc cstr_s
    in
    V
      ( Enum { name = decl.enum_name; constr = (fun _ -> index, cstr_s, v) },
        (decl.enum_name, (cstr_s, v)) )
  | Struct (decl, fvl) ->
    let l = List.map (fun (s, v) -> s, convert_atd_to_runtime_value v) fvl in
    let ty = Struct { name = decl.struct_name; fields = (fun _ -> l) } in
    V (ty, (decl.struct_name, l))
  | Array l ->
    let l = Array.map convert_atd_to_runtime_value l in
    V (Array Fun.id, l)
  | Unset -> failwith "Cannot convert 'Unset' atd value to Catala runtime value"
  | NotOverridden ->
    failwith "Cannot convert 'NotOverridden' atd value to Catala runtime value"
  | Empty -> failwith "Cannot convert 'Empty' atd value to Catala runtime value"

let interpret_program dcalc_prg scope_name build_term_to_interp =
  Interpreter.load_runtime_modules
    ~hashf:Hash.(finalise ~monomorphize_types:false)
    (dcalc_prg : typed Dcalc.Ast.program);
  Message.report_delayed_errors_if_any ();
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
  let program_fun =
    Expr.unbox (Program.to_expr dcalc_prg scope_name)
    |> Interpreter.evaluate_expr dcalc_prg.decl_ctx dcalc_prg.lang
  in
  let to_interp = build_term_to_interp program_fun in
  let results =
    Interpreter.evaluate_expr dcalc_prg.decl_ctx dcalc_prg.lang to_interp
  in
  Message.report_delayed_errors_if_any ();
  results, !failed_asserts

let rec convert_to_json_input ({ value; _ } : O.runtime_value) : Yojson.Safe.t =
  let open O in
  let convert_runtime_raw = function
    | Bool b -> `Bool b
    | Money i -> `String (string_of_float (float i /. 100.))
    | Integer i -> `String (string_of_int i)
    | Decimal f -> `String (string_of_float f)
    | Date { year; month; day } ->
      `String (Format.sprintf "%04d-%02d-%02d" year month day)
    | Duration { years; months; days } ->
      `Assoc ["years", `Int years; "months", `Int months; "days", `Int days]
    | Enum (_decl, (c, None)) when c = option_absent -> `Null
    | Enum (_decl, ("Present", Some x)) -> convert_to_json_input x
    | Enum (_decl, (constr, None)) -> `String constr
    | Enum (_decl, (constr, Some v)) -> `Assoc [constr, convert_to_json_input v]
    | Struct (_decl, fl) ->
      `Assoc
        (List.filter_map
           (function
             | ( _,
                 ({ value = Enum (_decl, (_, None)); _ } :
                   O.runtime_value) ) ->
               None
             | fname, v -> Some (fname, convert_to_json_input v))
           fl)
    | Array l -> `List (Array.to_list l |> List.map convert_to_json_input)
    | Unset -> failwith "convert_to_json_input: cannot convert 'unset' values"
    | NotOverridden ->
      failwith "convert_to_json_input: cannot convert 'NotOverridden' values"
    | Empty -> failwith "convert_to_json_input: cannot convert 'empty' values"
  in
  convert_runtime_raw value

let run_with_inputs
    include_dirs
    options
    tested_scope_name
    (scope_input : Yojson.Safe.t) =
  let desugared_prg, _naming_ctx, scope_name, dcalc_prg =
    retrieve_program include_dirs options tested_scope_name
  in
  let test =
    get_scope_test desugared_prg "<abstract>" scope_name
      ~tested_module:(Some (ModuleName.fresh ("abstract", Pos.void)))
  in
  let input_expr =
    let in_struct =
      (ScopeName.Map.find scope_name dcalc_prg.decl_ctx.ctx_scopes)
        .in_struct_name
    in
    let ty = TStruct in_struct, Pos.void in
    let atd_test_inputs : O.runtime_value =
      Lexing.from_string (Yojson.Safe.to_string scope_input)
      |> J.read_test_inputs (Yojson.init_lexer ())
      |> fun fields ->
      {
        O.attrs = [];
        value =
          O.Struct
            ( (* Dummy declaration *)
              { O.struct_name = StructName.to_string in_struct; fields = [] },
              List.filter_map
                (fun (field_name, { O.value; typ = _ }) ->
                  let rv = (Option.get value).value in
                  match rv.O.value with
                  | O.NotOverridden -> None
                  | O.Unset ->
                    failwith
                      (Printf.sprintf
                         "run_with_inputs: input '%s' has Unset value"
                         field_name)
                  | _ -> Some (field_name, rv))
                fields );
      }
    in
    let encoding = Encoding.make_encoding dcalc_prg.decl_ctx ty in
    let module JsonE = Json_encoding.Make (Json_repr.Yojson) in
    let rval =
      JsonE.destruct encoding (convert_to_json_input atd_test_inputs)
    in
    Encoding.convert_to_dcalc dcalc_prg.decl_ctx
      (Typed { pos = Pos.void; ty })
      ty rval
    |> Expr.unbox
    |> Interpreter.addcustom
    |> Expr.box
  in
  let build_term program_fun =
    Expr.make_app (Expr.box program_fun) [input_expr]
      [Expr.ty input_expr]
      (Expr.pos program_fun)
    |> Expr.unbox
  in
  let result_struct, failed_asserts =
    interpret_program dcalc_prg scope_name build_term
  in
  let (actual_results : (StructField.t * (dcalc, typed) gexpr) list), out_struct
      =
    match result_struct with
    | EStruct { fields; name }, _ ->
      let b = StructField.Map.bindings fields in
      ( List.map (fun (f, e) -> f, Interpreter.delcustom e) b,
        StructName.Map.find name dcalc_prg.decl_ctx.ctx_structs )
    | _ -> assert false
  in
  let test_outputs =
    List.map
      (fun (field, value_expr) ->
        let pos = Some (get_source_position (Expr.pos value_expr)) in
        ( StructField.to_string field,
          {
            O.value =
              Some
                {
                  value = get_value dcalc_prg.lang dcalc_prg.decl_ctx value_expr;
                  pos;
                };
            typ =
              get_typ dcalc_prg.lang dcalc_prg.decl_ctx
                (StructField.Map.find field out_struct);
          } ))
      actual_results
  in
  let assert_failures = not (failed_asserts = []) in
  let test = O.{ test with test_outputs } in
  write_stdout J.write_test_run O.{ test; assert_failures; diffs = [] }

let run_test include_dirs options testing_scope =
  let desugared_prg, naming_ctx, testing_scope_name, dcalc_prg =
    retrieve_program include_dirs options testing_scope
  in
  let test = get_catala_test (desugared_prg, naming_ctx) testing_scope_name in
  let build_term program_fun =
    let _args, program_expr =
      match program_fun with
      | EAbs { binder; _ }, _ -> Bindlib.unmbind binder
      | _ -> assert false
    in
    program_expr
  in
  let result_struct, failed_asserts =
    interpret_program dcalc_prg testing_scope_name build_term
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
  let test_outputs =
    List.map
      (fun (field, value_expr) ->
        let pos = Some (get_source_position (Expr.pos value_expr)) in
        ( StructField.to_string field,
          {
            O.value =
              Some
                {
                  value = get_value dcalc_prg.lang dcalc_prg.decl_ctx value_expr;
                  pos;
                };
            typ =
              get_typ dcalc_prg.lang dcalc_prg.decl_ctx
                (StructField.Map.find field out_struct);
          } ))
      actual_results
  in
  let test = O.{ test with test_outputs } in
  let expected_results =
    retrieve_assertions_values dcalc_prg testing_scope_name
  in
  let diffs =
    compute_diff expected_results actual_results
    |> List.map (proj_diff (get_value dcalc_prg.lang dcalc_prg.decl_ctx))
  in
  let assert_failures = not (failed_asserts = []) in
  let test_run = { O.test; O.assert_failures; O.diffs } in
  write_stdout J.write_test_run test_run

(* The interpreter dynloads each imported module as a native plugin. `clerk
   build DIR/ocaml/M.cmxs` reports success without writing M.cmxs ("cmxs" is
   not a selectable extension in clerk's OCaml backend; the plugin is only
   reachable through the internal `@catala-obj/<mod>` target -- upstream bug),
   and a bare `clerk build` builds the project's declared targets, which for a
   Java project are no plugins at all. `clerk test FILE` interprets, so it
   pulls the plugins in even when FILE itself no longer compiles; running the
   file's tests is the price. *)
let build_runtime_plugins ?buffer_path (options : Global.options) =
  if Sys.file_exists "clerk.toml" then
    let file =
      let f = Global.input_src_file options.Global.input_src in
      if Sys.file_exists f then Some f
      else
        match buffer_path with
        | Some b when Sys.file_exists b -> Some b
        | _ -> None
    in
    match file with
    | Some f ->
      ignore
        (Sys.command
           (Printf.sprintf "clerk test %s >/dev/null 2>&1" (Filename.quote f)))
    | None ->
      (* Nothing to point at: the declared targets are the best guess left. *)
      ignore (Sys.command "clerk build >/dev/null 2>&1")

(* A test fed on stdin has no file clerk can build the plugins from, and
   [buffer_path] may be the broken original, which clerk refuses before
   building anything. Write the text beside it under a name clerk accepts,
   build, remove, and go on from the text. *)
let run_test_cmd include_dirs options test_scope_name scope_input_opt buffer_path
    =
  let options =
    match options.Global.input_src, buffer_path with
    | Global.Stdin _, Some b when Sys.file_exists "clerk.toml" ->
      let text = In_channel.input_all stdin in
      let tmp =
        Filename.concat (Filename.dirname b)
          (Filename.remove_extension (Filename.basename b)
          ^ "__run" ^ Filename.extension b)
      in
      Fun.protect
        ~finally:(fun () -> try Sys.remove tmp with _ -> ())
        (fun () ->
          Out_channel.with_open_text tmp (fun oc -> Out_channel.output_string oc text);
          ignore
            (Sys.command
               (Printf.sprintf "clerk test %s >/dev/null 2>&1" (Filename.quote tmp))));
      Global.enforce_options ~input_src:(Global.Contents (text, b)) ()
    | _ ->
      build_runtime_plugins ?buffer_path options;
      options
  in
  match scope_input_opt with
  | None -> run_test include_dirs options test_scope_name
  | Some json -> run_with_inputs include_dirs options test_scope_name json

let print_scopes scopes = write_stdout J.write_scope_def_list scopes

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
          if scope_inputs prg.program_lang prg.program_ctx sc = [] then
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

let serialize_inputs (scope_input : Yojson.Safe.t option) =
  let scope_input =
    match scope_input with
    | None -> failwith "serliaze-inputs command requires --input argument"
    | Some i -> i
  in
  Lexing.from_string (Yojson.Safe.to_string scope_input)
  |> J.read_test_inputs (Yojson.init_lexer ())
  |> function
  | fields ->
    let dummy_decl = { O.struct_name = "dummy"; fields = [] } in
    let value =
      O.Struct
        ( dummy_decl,
          List.filter_map
            (fun (field_name, { J.value; typ = _ }) ->
              let rv = (Option.get value).value in
              match rv.O.value with
              | O.NotOverridden -> None
              | O.Unset ->
                failwith
                  (Printf.sprintf "serialize_inputs: input '%s' has Unset value"
                     field_name)
              | _ -> Some (field_name, rv))
            fields )
    in
    let json = convert_to_json_input { value; attrs = [] } in
    Format.(
      fprintf std_formatter "%a@." (Yojson.Safe.pretty_print ~std:true) json)
