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
        let config = Clerk_lib.Clerk_config.read clerk_toml_path in
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
    | _ -> []
  in
  let en_alias_map = String.Map.of_list (List.combine en_aliases en_names) in
  let fr_alias_map = String.Map.of_list (List.combine fr_aliases fr_names) in
  let lookup_aliased_name lang s =
    match lang with
    | `En -> String.Map.find_opt s en_alias_map
    | `Fr -> String.Map.find_opt s fr_alias_map
    | _ -> None
  in
  implicit_stdlib_aliases, lookup_aliased_name

let is_implicit_stdlib_alias lang alias =
  List.exists (fun a -> String.equal a alias) (implicit_stdlib_aliases lang)

type lang_strings = {
  declaration_scope : string;
  output_scope : string;
  using_module : string;
  definition : string;
  assertion : string;
  equals : string;
  content : string;
  scope : string;
  present : string;
}

let get_lang_strings =
  let fr_strings =
    {
      declaration_scope = "déclaration champ d'application";
      output_scope = "résultat";
      using_module = "Usage de";
      definition = "définition";
      assertion = "assertion";
      equals = "égal à";
      content = "contenu";
      scope = "champ d'application";
      present = "Présent";
    }
  in
  let en_strings =
    {
      declaration_scope = "declaration scope";
      output_scope = "output";
      using_module = "Using";
      definition = "definition";
      assertion = "assertion";
      equals = "equals";
      content = "content";
      scope = "scope";
      present = "Present";
    }
  in
  function
  | `Fr -> fr_strings
  | `En -> en_strings
  | _ -> unsupported "unsupported language"

let mk_optional_enum_decl lang typ =
  {
    O.enum_name = EnumName.to_string ConstantNames.option_enum;
    constructors = ["Absent", None; (get_lang_strings lang).present, Some typ];
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
      if attrs = [] then None
      else Some (EnumConstructor.to_string constr, attrs))
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
    mk_optional_enum_decl lang typ
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
type Pos.attr += SigPin of string

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
    | ETuple args ->
      (* tuples carry as Array; the TTuple type disambiguates on print *)
      O.Array (Array.of_list (List.map (get_value lang decl_ctx) args))
    | EStruct { name; fields } ->
      O.Struct
        ( get_struct lang decl_ctx name,
          List.map
            (fun (field, v) ->
              StructField.to_string field, get_value lang decl_ctx v)
            (StructField.Map.bindings fields) )
    | EInj { name; e; _ } when EnumName.equal ConstantNames.option_enum name -> (
      match Typing.expr decl_ctx e |> Expr.unbox with
      | ELit LUnit, _ty ->
        let none_field = EnumConstructor.to_string ConstantNames.none_constr, None in
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

(* === Canonical signature projection (for migration drift detection) ===

   Produces a deterministic, representation-INDEPENDENT canonical text of a
   scope's I/O contract, then hashes it. The hash is computed over THIS canonical
   form, never over the stored ATD JSON, so the wire/storage representation may
   stay inlined/duplicated without affecting the hash (interning the model later
   is hash-stable). Properties:
   - nominal types (TStruct/TEnum) referenced BY NAME; their definitions collected
     once into a sorted def-set (kills inline duplication; memoization breaks any
     type recursion);
   - structural types (option/array/tuple/scalars) inlined;
   - inputs, outputs, struct fields and enum constructors SORTED by name
     (declaration order is cosmetic in Catala; a reorder must not change the hash);
   - tuple element order PRESERVED (it is positional);
   - inputs carry their is_context flag (reentrant default vs required input is a
     contract distinction); outputs ARE included (assertions ride on them);
   - attributes, source positions and module_deps EXCLUDED (not contract);
   - stamped with a scheme version so the rules can be revved deliberately. *)

let sig_scheme_version = "v1"

let scope_signature_canonical (sd : O.scope_def) : string =
  let buf = Buffer.create 1024 in
  (* `list-scopes` of a module leaves the scope's OWN locally-declared types
     unqualified ("Pair"), but a test importing that module sees them qualified
     ("Mod.Pair"). Both paths must canonicalize identically for drift detection,
     so we fully-qualify every nominal name to the scope's module. *)
  let qualify name =
    if String.equal sd.module_name "" || String.contains name '.' then name
    else sd.module_name ^ "." ^ name
  in
  let structs : (string, (string * O.typ) list) Hashtbl.t = Hashtbl.create 16 in
  let enums : (string, (string * O.typ option) list) Hashtbl.t =
    Hashtbl.create 16
  in
  let rec collect (t : O.typ) : unit =
    match t with
    | TBool | TInt | TRat | TMoney | TDate | TDuration | TUnit | TUnset -> ()
    | TOption t | TArray t -> collect t
    | TTuple tl -> List.iter collect tl
    | TArrow (tl, t) ->
      List.iter collect tl;
      collect t
    | TStruct { struct_name; fields } ->
      let struct_name = qualify struct_name in
      if not (Hashtbl.mem structs struct_name) then begin
        Hashtbl.add structs struct_name fields;
        List.iter (fun (_, t) -> collect t) fields
      end
    | TEnum { enum_name; constructors; _ } ->
      let enum_name = qualify enum_name in
      if not (Hashtbl.mem enums enum_name) then begin
        Hashtbl.add enums enum_name constructors;
        List.iter (fun (_, t) -> Option.iter collect t) constructors
      end
  in
  let rec ref_of (t : O.typ) : string =
    match t with
    | TBool -> "bool"
    | TInt -> "int"
    | TRat -> "rat"
    | TMoney -> "money"
    | TDate -> "date"
    | TDuration -> "duration"
    | TUnit -> "unit"
    | TUnset -> "unset"
    | TOption t -> "option(" ^ ref_of t ^ ")"
    | TArray t -> "array(" ^ ref_of t ^ ")"
    | TTuple tl -> "tuple(" ^ String.concat "," (List.map ref_of tl) ^ ")"
    | TArrow (tl, t) ->
      "arrow(" ^ String.concat "," (List.map ref_of tl) ^ "->" ^ ref_of t ^ ")"
    | TStruct { struct_name; _ } -> "@" ^ qualify struct_name
    | TEnum { enum_name; _ } -> "@" ^ qualify enum_name
  in
  let by_name (a, _) (b, _) = String.compare a b in
  List.iter (fun (_, (si : O.scope_input)) -> collect si.typ) sd.inputs;
  List.iter (fun (_, t) -> collect t) sd.outputs;
  Buffer.add_string buf (Printf.sprintf "sig/%s\n" sig_scheme_version);
  Buffer.add_string buf (Printf.sprintf "scope %s.%s\n" sd.module_name sd.name);
  List.sort by_name sd.inputs
  |> List.iter (fun (name, (si : O.scope_input)) ->
       Buffer.add_string buf
         (Printf.sprintf "in %s %s %s\n" name
            (if si.is_context then "ctx" else "inp")
            (ref_of si.typ)));
  List.sort by_name sd.outputs
  |> List.iter (fun (name, t) ->
       Buffer.add_string buf (Printf.sprintf "out %s %s\n" name (ref_of t)));
  let names tbl = Hashtbl.fold (fun k _ acc -> k :: acc) tbl [] |> List.sort String.compare in
  names enums
  |> List.iter (fun name ->
       Buffer.add_string buf (Printf.sprintf "def enum %s\n" name);
       Hashtbl.find enums name
       |> List.sort by_name
       |> List.iter (fun (ctor, t) ->
            Buffer.add_string buf
              (Printf.sprintf "  ctor %s %s\n" ctor
                 (match t with None -> "-" | Some t -> ref_of t))));
  names structs
  |> List.iter (fun name ->
       Buffer.add_string buf (Printf.sprintf "def struct %s\n" name);
       Hashtbl.find structs name
       |> List.sort by_name
       |> List.iter (fun (fld, t) ->
            Buffer.add_string buf
              (Printf.sprintf "  field %s %s\n" fld (ref_of t))));
  Buffer.contents buf

let scope_signature_hash (sd : O.scope_def) : string =
  Digest.to_hex (Digest.string (scope_signature_canonical sd))

(* The per-test signature pin: "<Module>.<Scope>@<hash>", stamped into the
   #[testcase.sig] attribute. Identifies the exact signature version the test
   targets, for migration drift detection. *)
let scope_signature_pin (sd : O.scope_def) : string =
  Printf.sprintf "%s.%s@%s" sd.module_name sd.name (scope_signature_hash sd)

(* Committed signature snapshot store: persist the scope_def under a
   content-addressed name so a drifted test can later be migrated against the
   exact signature it was authored with. Content-addressed (skip if present),
   so it is O(distinct signature versions), not O(tests). *)
let write_sig_snapshot dir (sd : O.scope_def) : unit =
  File.ensure_dir dir;
  let fname =
    Printf.sprintf "%s.%s@%s.sig.json" sd.module_name sd.name
      (scope_signature_hash sd)
  in
  let path = File.(dir / fname) in
  if not (File.exists path) then
    File.with_out_channel path
    @@ fun oc ->
    let buf = Buffer.create 1024 in
    J.write_scope_def buf sd;
    Buffer.output_buffer oc buf

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
    sig_pin = None;
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
    | TOption typ -> Enum (mk_optional_enum_decl lang typ, ("Absent", None))
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
    include_dirs
    options =
  let path_to_build, include_dirs =
    if include_dirs = [] then lookup_include_dirs options else ".", include_dirs
  in
  let prg, _ = read_program include_dirs path_to_build options in
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
  let sig_pin =
    get_single_attr ~default:None info (function
      | SigPin s -> Some (Some s)
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
  { base_test with O.test_inputs; test_outputs; description; title; sig_pin }

let import_catala_tests (prg, naming_ctx) =
  List.map (get_catala_test (prg, naming_ctx)) (get_test_scopes prg)

let read_test include_dirs (options : Global.options) buffer_path =
  let path_to_build, include_dirs =
    if include_dirs = [] then lookup_include_dirs ?buffer_path options
    else ".", include_dirs
  in
  let prg = read_program include_dirs path_to_build options in
  let tests = import_catala_tests prg in
  write_stdout J.write_test_list tests

type duration_units = { day : string; month : string; year : string }

type value_strings = {
  true_str : string;
  false_str : string;
  money_fmt : (int -> int -> unit, Format.formatter, unit) format;
  decimal_sep : char;
  content_str : string;
  duration_units : duration_units;
  present : string;
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
    }
  in
  function
  | `Fr -> fr_strings
  | `En -> en_strings
  | _ -> unsupported "unsupported language"

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
    if v = None then fprintf ppf "Absent"
    else
      fprintf ppf "%s %s %a" strings.present strings.content_str
        (print_catala_value ~typ:(List.assoc constr constructors) ~lang)
        (Option.get v)
  | Some (TEnum { enum_name; constructors; _ }), O.Enum (_en, (constr, Some v)) ->
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
  | Some (O.TStruct _sdecl), O.Struct (st, fields) ->
    fprintf ppf "@[<hv 2>%s {@ %a@;<1 -2>}@]" st.struct_name
      (pp_print_list ~pp_sep:pp_print_space (fun ppf (typ, (fld, v)) ->
           fprintf ppf "-- %s: %a" fld
             (print_catala_value ~typ:(Some typ) ~lang)
             v))
      (List.combine (List.map snd _sdecl.fields) fields)
  | _, O.Struct (st, fields) ->
    fprintf ppf "@[<hv 2>%s {@ %a@;<1 -2>}@]" st.struct_name
      (pp_print_list ~pp_sep:pp_print_space (fun ppf (fld, v) ->
           fprintf ppf "-- %s: %a" fld (print_catala_value ~typ:None ~lang) v))
      fields
  | Some (O.TTuple ts), O.Array vl ->
    fprintf ppf "@[<hov 1>(%a)@]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
         (fun ppf (t, v) -> print_catala_value ~typ:(Some t) ~lang ppf v))
      (List.combine ts (Array.to_list vl))
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

(* === Stub module synthesis (for migration value recovery) ===

   From a stored OLD scope signature, regenerate throwaway Catala stub modules
   that re-declare exactly the old types + scope, so a test written against the
   old contract typechecks again and the existing `read` can recover its values
   (even though the real module has since changed). Reuses the whole existing
   read/write pipeline; the only new code is this scope_def -> Catala source
   emitter. Returns (module_name, source) pairs.

   Supports all value-bearing types (literals, struct, enum, array, option,
   tuple). Arrow/unit types raise Unsupported (they can't appear in a
   value-bearing scope signature — get_typ rejects them upstream too). *)

let split_qualified name =
  match String.rindex_opt name '.' with
  | None -> None, name
  | Some i ->
    ( Some (String.sub name 0 i),
      String.sub name (i + 1) (String.length name - i - 1) )

let stub_modules (lang : Global.backend_lang) (sd : O.scope_def) :
    (string * string) list =
  let strings = get_lang_strings lang in
  let kw_struct, kw_enum, kw_data =
    match lang with
    | `Fr -> "déclaration structure", "déclaration énumération", "donnée"
    | `En -> "declaration structure", "declaration enumeration", "data"
    | _ -> unsupported "unsupported language"
  in
  let module_of name = fst (split_qualified name) in
  (* list-scopes leaves the scope's own (locally-declared) types unqualified;
     attribute those to the scope's module. *)
  let owner name =
    match module_of name with Some m -> m | None -> sd.module_name
  in
  let structs : (string, (string * O.typ) list) Hashtbl.t = Hashtbl.create 16 in
  let enums : (string, (string * O.typ option) list) Hashtbl.t =
    Hashtbl.create 16
  in
  let rec collect (t : O.typ) =
    match t with
    | TBool | TInt | TRat | TMoney | TDate | TDuration | TUnit | TUnset -> ()
    | TArray t | TOption t -> collect t
    | TTuple tl -> List.iter collect tl
    | TStruct { struct_name; fields } ->
      if not (Hashtbl.mem structs struct_name) then begin
        Hashtbl.add structs struct_name fields;
        List.iter (fun (_, t) -> collect t) fields
      end
    | TEnum { enum_name; constructors; _ } ->
      if not (Hashtbl.mem enums enum_name) then begin
        Hashtbl.add enums enum_name constructors;
        List.iter (fun (_, t) -> Option.iter collect t) constructors
      end
    | TArrow _ -> unsupported "stub: arrow type not supported"
  in
  List.iter (fun (_, (si : O.scope_input)) -> collect si.typ) sd.inputs;
  List.iter (fun (_, t) -> collect t) sd.outputs;
  let qualify current name =
    match split_qualified name with
    | Some m, base when String.equal m current -> base
    | None, base when String.equal current sd.module_name -> base
    | None, base -> sd.module_name ^ "." ^ base
    | _ -> name
  in
  let rec render current (t : O.typ) : string =
    match t with
    | TBool -> ( match lang with `Fr -> "booléen" | _ -> "boolean")
    | TInt -> ( match lang with `Fr -> "entier" | _ -> "integer")
    | TRat -> ( match lang with `Fr -> "décimal" | _ -> "decimal")
    | TMoney -> ( match lang with `Fr -> "argent" | _ -> "money")
    | TDate -> "date"
    | TDuration -> ( match lang with `Fr -> "durée" | _ -> "duration")
    | TArray t ->
      (match lang with `Fr -> "liste de " | _ -> "list of ") ^ render current t
    | TStruct { struct_name; _ } -> qualify current struct_name
    | TEnum { enum_name; _ } -> qualify current enum_name
    | TOption t ->
      (match lang with `Fr -> "optionnel de " | _ -> "optional of ")
      ^ render current t
    | TTuple tl ->
      "(" ^ String.concat ", " (List.map (render current) tl) ^ ")"
    | TUnit | TUnset | TArrow _ ->
      unsupported "stub: unsupported type in declaration"
  in
  let names_in tbl current =
    Hashtbl.fold
      (fun n _ acc -> if String.equal (owner n) current then n :: acc else acc)
      tbl []
    |> List.sort String.compare
  in
  let all_modules =
    let m = ref [sd.module_name] in
    let add n = m := owner n :: !m in
    Hashtbl.iter (fun n _ -> add n) structs;
    Hashtbl.iter (fun n _ -> add n) enums;
    List.sort_uniq String.compare !m
  in
  (* modules referenced by [current]'s own declarations (+ scope I/O) *)
  let deps_of current =
    let acc = ref [] in
    let rec visit = function
      | O.TStruct { struct_name = n; _ } | O.TEnum { enum_name = n; _ } ->
        let m = owner n in
        if not (String.equal m current) then acc := m :: !acc
      | O.TArray t | O.TOption t -> visit t
      | O.TTuple tl -> List.iter visit tl
      | _ -> ()
    in
    List.iter (fun n -> List.iter (fun (_, t) -> visit t) (Hashtbl.find structs n))
      (names_in structs current);
    List.iter
      (fun n ->
        List.iter (fun (_, t) -> Option.iter visit t) (Hashtbl.find enums n))
      (names_in enums current);
    if String.equal current sd.module_name then begin
      List.iter (fun (_, (si : O.scope_input)) -> visit si.typ) sd.inputs;
      List.iter (fun (_, t) -> visit t) sd.outputs
    end;
    List.sort_uniq String.compare !acc
  in
  let emit_module current =
    let b = Buffer.create 1024 in
    let p fmt = Printf.ksprintf (Buffer.add_string b) fmt in
    p "> Module %s\n\n" current;
    List.iter (fun d -> p "> %s %s\n" strings.using_module d) (deps_of current);
    p "\n```catala-metadata\n";
    List.iter
      (fun n ->
        let _, base = split_qualified n in
        p "%s %s:\n" kw_enum base;
        List.iter
          (fun (ctor, t) ->
            match t with
            | None -> p "  -- %s\n" ctor
            | Some t -> p "  -- %s %s %s\n" ctor strings.content (render current t))
          (Hashtbl.find enums n);
        p "\n")
      (names_in enums current);
    List.iter
      (fun n ->
        let _, base = split_qualified n in
        p "%s %s:\n" kw_struct base;
        List.iter
          (fun (fld, t) -> p "  %s %s %s %s\n" kw_data fld strings.content (render current t))
          (Hashtbl.find structs n);
        p "\n")
      (names_in structs current);
    if String.equal current sd.module_name then begin
      let _, sbase = split_qualified sd.name in
      p "%s %s:\n" strings.declaration_scope sbase;
      List.iter
        (fun (name, (si : O.scope_input)) ->
          let kw =
            if si.is_context then match lang with `Fr -> "contexte" | _ -> "context"
            else match lang with `Fr -> "entrée" | _ -> "input"
          in
          p "  %s %s %s %s\n" kw name strings.content (render current si.typ))
        sd.inputs;
      List.iter
        (fun (name, t) ->
          p "  %s %s %s %s\n" strings.output_scope name strings.content
            (render current t))
        sd.outputs;
      p "```\n\n```catala\n%s %s:\n" strings.scope sbase;
      let default_def name typ =
        let v = generate_default_value lang typ in
        p "  %s %s %s %s\n" strings.definition name strings.equals
          (Format.asprintf "%a" (print_catala_value ~typ:(Some typ) ~lang) v)
      in
      List.iter
        (fun (name, (si : O.scope_input)) ->
          if si.is_context then default_def name si.typ)
        sd.inputs;
      List.iter (fun (name, t) -> default_def name t) sd.outputs;
      p "```\n"
    end
    else p "```\n";
    Buffer.contents b
  in
  List.map (fun m -> m, emit_module m) all_modules

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
  (* Re-stamp the signature pin from the scope being targeted now: writing a
     test pins it to its current signature. *)
  fprintf ppf "#[testcase.sig = %s]@\n"
    (String.quote (scope_signature_pin t.tested_scope));
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

(* Emit a whole test file (Using headers + each test scope) to [ppf]. Shared by
   `write` and `migrate init`. *)
let emit_test_list ppf lang tests =
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

let write_catala sig_dir options outfile =
  let tests =
    J.read_test_list (Yojson.init_lexer ()) (Lexing.from_channel stdin)
  in
  Option.iter
    (fun dir ->
      List.iter (fun (t : O.test) -> write_sig_snapshot dir t.tested_scope) tests)
    sig_dir;
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
  with_out @@ fun ppf -> emit_test_list ppf lang tests

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
    | Enum (_decl, ("Absent", None)) -> `Null
    | Enum (_decl, ("Present", Some x)) -> convert_to_json_input x
    | Enum (_decl, (constr, None)) -> `String constr
    | Enum (_decl, (constr, Some v)) -> `Assoc [constr, convert_to_json_input v]
    | Struct (_decl, fl) ->
      `Assoc
        (List.filter_map
           (function
             | ( _,
                 ({ value = Enum (_decl, ("Absent", None)); _ } :
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
  let expected_results = retrieve_assertions_values dcalc_prg in
  let diffs =
    compute_diff expected_results actual_results
    |> List.map (proj_diff (get_value dcalc_prg.lang dcalc_prg.decl_ctx))
  in
  let assert_failures = not (failed_asserts = []) in
  let test_run = { O.test; O.assert_failures; O.diffs } in
  write_stdout J.write_test_run test_run

let run_test_cmd include_dirs options test_scope_name scope_input_opt =
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

(* Parse scope_def(s) from a JSON string, accepting EITHER a JSON array (e.g. the
   output of list-scopes) OR a single scope_def object (e.g. a committed snapshot
   file), so the two representations are interchangeable on these commands. *)
let parse_scope_defs content =
  let lexer = Yojson.init_lexer () in
  if
    let t = String.trim content in
    String.length t > 0 && t.[0] = '['
  then J.read_scope_def_list lexer (Lexing.from_string content)
  else [ J.read_scope_def lexer (Lexing.from_string content) ]

let read_scope_defs_stdin () = parse_scope_defs (In_channel.input_all stdin)

(* Reads scope_def(s) from [file] if given, else stdin (the output of list-scopes,
   the tested_scopes extracted from a read result, or a single snapshot file) and
   prints one "<module>.<name>\t<hash>" line per scope. With [with_canonical], also
   dumps the canonical projection text. *)
let sig_hash with_canonical file =
  let scopes =
    match file with
    | Some f -> parse_scope_defs (File.contents f)
    | None -> read_scope_defs_stdin ()
  in
  List.iter
    (fun (sd : O.scope_def) ->
      Printf.printf "%s.%s\t%s\n" sd.module_name sd.name
        (scope_signature_hash sd);
      if with_canonical then
        Printf.printf "%s\n" (scope_signature_canonical sd))
    scopes

(* Reads a scope_def_list from stdin and writes synthesized stub modules for the
   FIRST scope into [out_dir] as <Module>.catala_<ext>. Debug/validation entry
   point for stub synthesis (migration value recovery). *)
let stub_cmd out_dir options =
  let scopes = read_scope_defs_stdin () in
  match scopes with
  | [] -> failwith "stub: empty scope_def_list on stdin"
  | sd :: _ ->
    let lang =
      match options.Global.language with Some l -> l | None -> `Fr
    in
    let ext = match lang with `Fr -> "catala_fr" | `En -> "catala_en" | _ -> "catala" in
    List.iter
      (fun (modname, src) ->
        let path = Filename.concat out_dir (Printf.sprintf "%s.%s" modname ext) in
        let oc = open_out path in
        output_string oc src;
        close_out oc;
        Printf.eprintf "wrote %s\n" path)
      (stub_modules lang sd)

(* ============================================================================
   Migration triage: `migrate status` / `migrate init`.

   Classifies tests by signature drift WITHOUT typechecking the test (a drifted
   test would not compile). The pin and tested scope are read textually from the
   source; the live signature is resolved by loading only the test's import
   headers, so the live module typechecks even when the test scope is stale.
   ============================================================================ *)

(* Index of the first occurrence of [sub] in [s], or None. *)
let find_sub ~sub s =
  let n = String.length sub and m = String.length s in
  if n = 0 then Some 0
  else
    let rec go i =
      if i + n > m then None
      else if String.equal (String.sub s i n) sub then Some i
      else go (i + 1)
    in
    go 0

let contains_sub ~sub s = find_sub ~sub s <> None

(* Content of the first double-quoted run in [s], or None. *)
let first_quoted s =
  match String.index_opt s '"' with
  | None -> None
  | Some i -> (
    match String.index_from_opt s (i + 1) '"' with
    | None -> None
    | Some j -> Some (String.sub s (i + 1) (j - i - 1)))

(* A "<Module>.<Scope>"-shaped token (two dot-separated capitalised idents):
   used to recover the tested scope of an unpinned test from its subscope line. *)
let qualified_scope_token s =
  let is_id_start c = (c >= 'A' && c <= 'Z') in
  let is_id c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
    || c = '_'
  in
  let m = String.length s in
  let rec scan i =
    if i >= m then None
    else if is_id_start s.[i] then
      let j = ref i in
      while !j < m && (is_id s.[!j] || s.[!j] = '.') do incr j done;
      let tok = String.sub s i (!j - i) in
      (* must look like Mod.Scope: exactly one dot, both parts capitalised *)
      (match String.split_on_char '.' tok with
       | [a; b]
         when String.length a > 0 && String.length b > 0
              && is_id_start a.[0] && is_id_start b.[0] ->
         Some tok
       | _ -> scan !j)
    else scan (i + 1)
  in
  scan 0

(* One testing scope as recovered textually from source. *)
type parsed_test = {
  pt_test_scope : string;        (* testing scope name, e.g. "Calc_rich" *)
  pt_target : string option;     (* "<Module>.<Scope>" under test *)
  pt_pin : string option;        (* full "<Module>.<Scope>@<hash>" pin *)
}

(* Recover the test scopes of a source file without typechecking it. Walks the
   attribute block that precedes each `declaration scope` (EN) /
   `déclaration champ d'application` (FR), keeping the #[testcase.sig] pin and the
   #[testcase.testui] marker, then reads the following subscope line for the
   tested scope. *)
let parse_tests_textually content =
  let lines = String.split_on_char '\n' content in
  let scope_kw = "declaration scope " in
  let scope_kw_fr = "champ d'application " in
  let is_scope_decl l =
    contains_sub ~sub:scope_kw l
    || (contains_sub ~sub:"claration" l && contains_sub ~sub:scope_kw_fr l)
  in
  let decl_name l =
    (* token right after the scope keyword, trailing ':' stripped *)
    let after =
      match find_sub ~sub:scope_kw l with
      | Some i -> String.sub l (i + String.length scope_kw) (String.length l - i - String.length scope_kw)
      | None -> (
        match find_sub ~sub:scope_kw_fr l with
        | Some i -> String.sub l (i + String.length scope_kw_fr) (String.length l - i - String.length scope_kw_fr)
        | None -> l)
    in
    let after = String.trim after in
    match String.index_opt after ':' with
    | Some k -> String.trim (String.sub after 0 k)
    | None -> (
      match String.index_opt after ' ' with
      | Some k -> String.sub after 0 k
      | None -> after)
  in
  let rec walk lines (pending : string list) (acc : parsed_test list)
      (cur : parsed_test option) =
    match lines with
    | [] -> List.rev (match cur with Some t -> t :: acc | None -> acc)
    | line :: rest ->
      let t = String.trim line in
      if String.length t >= 2 && t.[0] = '#' && t.[1] = '[' then
        (* attribute line: accumulate, flushing any open subscope search *)
        let acc = match cur with Some c -> c :: acc | None -> acc in
        walk rest (line :: pending) acc None
      else if is_scope_decl line then begin
        let acc = match cur with Some c -> c :: acc | None -> acc in
        let is_test =
          List.exists (fun a -> contains_sub ~sub:"testcase.testui" a) pending
        in
        let pin =
          List.find_map
            (fun a ->
              if contains_sub ~sub:"testcase.sig" a then first_quoted a else None)
            pending
        in
        if is_test then
          let target =
            match pin with
            | Some p -> (
              match String.index_opt p '@' with
              | Some k -> Some (String.sub p 0 k)
              | None -> Some p)
            | None -> None
          in
          walk rest []
            (match cur with Some c -> c :: acc | None -> acc)
            (Some { pt_test_scope = decl_name line; pt_target = target; pt_pin = pin })
        else walk rest [] acc None
      end
      else begin
        (* inside a test scope, before its target is known, look for the
           "Mod.Scope" subscope reference; otherwise reset pending on blanks *)
        match cur with
        | Some c when c.pt_target = None -> (
          match qualified_scope_token line with
          | Some tok -> walk rest [] (acc) (Some { c with pt_target = Some tok })
          | None -> walk rest (if t = "" then [] else pending) acc cur)
        | _ -> walk rest (if t = "" then [] else pending) acc cur
      end
  in
  walk lines [] [] None

(* Resolve the live signature hash of [module_name].[scope_base] as seen from
   [test_file], by loading only that file's import headers (so the live module
   typechecks regardless of test drift). Cached per module across a run. *)
let live_hash_cache : (string, (string, string) result) Hashtbl.t =
  Hashtbl.create 16

let live_scope_hash ~test_file ~module_name ~scope_base =
  let key = module_name ^ "." ^ scope_base in
  match Hashtbl.find_opt live_hash_cache key with
  | Some r -> r
  | None ->
    let r =
      try
        let headers =
          File.contents test_file
          |> String.split_on_char '\n'
          |> List.filter (fun l ->
                 let l = String.trim l in
                 String.length l > 0 && l.[0] = '>')
        in
        let content = String.concat "\n" headers ^ "\n" in
        let options =
          Global.enforce_options
            ~input_src:(Global.Contents (content, test_file)) ()
        in
        let path_to_build, include_dirs = lookup_include_dirs options in
        let prg, _ = read_program include_dirs path_to_build options in
        let found =
          ModuleName.Map.fold
            (fun mn modul acc ->
              match acc with
              | Some _ -> acc
              | None ->
                if String.equal (ModuleName.to_string mn) module_name then
                  Some (mn, modul)
                else None)
            prg.I.program_modules None
        in
        match found with
        | None -> Error (Printf.sprintf "module %s not found" module_name)
        | Some (mn, modul) -> (
          let sc =
            ScopeName.Map.fold
              (fun sn sc acc ->
                match acc with
                | Some _ -> acc
                | None ->
                  if String.equal (ScopeName.base sn) scope_base then Some sc
                  else None)
              modul.I.module_scopes None
          in
          match sc with
          | None ->
            Error
              (Printf.sprintf "scope %s not found in module %s" scope_base
                 module_name)
          | Some sc ->
            Ok (scope_signature_hash (get_scope_def prg sc ~tested_module:mn)))
      with
      | Message.CompilerError _ ->
        Error (Printf.sprintf "live module %s does not compile" module_name)
      | e -> Error (Printexc.to_string e)
    in
    Hashtbl.replace live_hash_cache key r;
    r

(* Recursively collect Catala test files under [path] (file or directory). *)
let rec collect_catala_files path =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun name ->
           if String.length name > 0 && name.[0] = '.' then []
           else if String.equal name "_build" then []
           else collect_catala_files (Filename.concat path name))
  else
    let ok ext = Filename.check_suffix path ext in
    if ok ".catala_en" || ok ".catala_fr" || ok ".catala_pl" then [ path ]
    else []

(* Hash part of a "<Module>.<Scope>@<hash>" pin. *)
let pin_hash p =
  match String.index_opt p '@' with
  | Some k -> Some (String.sub p (k + 1) (String.length p - k - 1))
  | None -> None

let classify_test ~sig_dir ~test_file (pt : parsed_test) : O.sig_status_entry =
  let target = Option.value ~default:"?" pt.pt_target in
  let mk ?pin ?live ?reason state =
    {
      O.file = test_file;
      test_scope = pt.pt_test_scope;
      target_scope = target;
      state;
      pin;
      live;
      reason;
    }
  in
  match pt.pt_pin with
  | None -> mk `Unknown
  | Some pin -> (
    let phash = pin_hash pin in
    let module_name, scope_base =
      match String.index_opt target '.' with
      | Some k ->
        String.sub target 0 k, String.sub target (k + 1) (String.length target - k - 1)
      | None -> "", target
    in
    match live_scope_hash ~test_file ~module_name ~scope_base with
    | Error reason -> mk ?pin:phash ~reason `Blocked
    | Ok live ->
      if phash = Some live then mk ?pin:phash ~live `Fresh
      else
        (* drifted: migratable only if the pinned snapshot is recoverable *)
        let dir = Option.value ~default:(Filename.dirname test_file) sig_dir in
        let snap = File.(dir / (pin ^ ".sig.json")) in
        if File.exists snap then mk ?pin:phash ~live `Stale
        else
          mk ?pin:phash ~live
            ~reason:(Printf.sprintf "missing snapshot %s" (Filename.basename snap))
            `Blocked)

let state_label = function
  | `Fresh -> "fresh"
  | `Stale -> "stale"
  | `Unknown -> "unknown"
  | `Blocked -> "blocked"

let migrate_status json sig_dir path _options =
  let files = collect_catala_files path in
  let entries =
    List.concat_map
      (fun test_file ->
        let content = File.contents test_file in
        parse_tests_textually content
        |> List.map (classify_test ~sig_dir ~test_file))
      files
  in
  if json then write_stdout J.write_sig_status entries
  else begin
    let count st = List.length (List.filter (fun e -> e.O.state = st) entries) in
    Printf.printf "fresh:   %d\nstale:   %d\nunknown: %d\nblocked: %d\n"
      (count `Fresh) (count `Stale) (count `Unknown) (count `Blocked);
    List.iter
      (fun (e : O.sig_status_entry) ->
        if e.state <> `Fresh then
          Printf.printf "  %-8s %s  %s (%s)%s\n" (state_label e.state)
            e.target_scope e.test_scope (Filename.basename e.file)
            (match e.reason with Some r -> ": " ^ r | None -> ""))
      entries
  end

(* migrate init: seed a pin onto unpinned ("unknown") tests. Safe only when the
   file typechecks against the live module — then its values are valid by
   definition, so "adopt current" is correct. A file that does NOT typecheck is
   an unpinned-but-drifted case that needs real migration, not a seed, so we
   refuse it. Mechanically this is read|write over the seedable files: reading
   stamps the live signature into every test's tested_scope, and writing emits
   the pin (and the snapshot) from it. *)
let init_file ~sig_dir test_file =
  let parsed = parse_tests_textually (File.contents test_file) in
  if not (List.exists (fun pt -> pt.pt_pin = None) parsed) then `Nothing_to_seed
  else
    let unpinned = List.length (List.filter (fun pt -> pt.pt_pin = None) parsed) in
    let options = Global.enforce_options ~input_src:(Global.FileName test_file) () in
    let path_to_build, include_dirs = lookup_include_dirs options in
    match
      try `Ok (import_catala_tests (read_program include_dirs path_to_build options))
      with e -> `Err (Printexc.to_string e)
    with
    | `Err msg -> `Cannot_seed msg
    | `Ok [] -> `Cannot_seed "no tests recovered"
    | `Ok tests ->
      let dir = Option.value ~default:(Filename.dirname test_file) sig_dir in
      List.iter
        (fun (t : O.test) -> write_sig_snapshot dir t.tested_scope)
        tests;
      let lang = Catala_utils.Cli.file_lang test_file in
      File.with_out_channel test_file (fun oc ->
          let ppf = Format.formatter_of_out_channel oc in
          emit_test_list ppf lang tests;
          Format.pp_print_flush ppf ());
      `Seeded unpinned

let migrate_init sig_dir path _options =
  collect_catala_files path
  |> List.iter (fun test_file ->
         match init_file ~sig_dir test_file with
         | `Nothing_to_seed -> ()
         | `Seeded n ->
           Printf.printf "seeded %s (%d pin%s)\n" test_file n
             (if n = 1 then "" else "s")
         | `Cannot_seed _ ->
           Printf.printf
             "skipped %s: does not typecheck against the live module (needs \
              migration, not a seed)\n"
             test_file)

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
