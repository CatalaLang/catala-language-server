(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Vincent Botbol <vincent.botbol@inria.fr>

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
open Utils
open Scopelang.Ast

let hash_info (type a) (module M : Uid.Id with type t = a) (v : a) : int =
  Hashtbl.hash (M.get_info v)

module LTable = Stdlib.Map.Make (Int)

type lookup_entry = {
  declaration : Pos.t option;
  definitions : Pos.t list option;
  usages : Pos.t list option;
  types : Pos.t list option;
}

let pp_lookup_entry fmt { declaration; definitions; usages; types } =
  let open Format in
  let pp_pos fmt p = pp_print_string fmt @@ Pos.to_string_short p in
  fprintf fmt
    "declaration: %a@\n\
     @[<v 2>definitions:@ %a@]@\n\
     @[<v 2>usages:@ %a@]@\n\
     @[<v 2>types:@ %a@]" (pp_opt pp_pos) declaration
    (pp_opt (pp_print_list ~pp_sep:pp_print_cut pp_pos))
    definitions
    (pp_opt (pp_print_list ~pp_sep:pp_print_cut pp_pos))
    usages
    (pp_opt (pp_print_list ~pp_sep:pp_print_cut pp_pos))
    types

let empty_lookup =
  { declaration = None; definitions = None; usages = None; types = None }

type jump = { hash : int; name : string; typ : typ }

type mjump = {
  hash : int;
  name : string;
  alias : string option;
  interface : Surface.Ast.interface;
}

type var =
  | Topdef of jump
  | Definition of jump
  | Declaration of jump
  | Usage of jump
  | Type of jump
  | Literal of typ
  | Module_def of mjump
  | Module_use of mjump
  | Module_decl of mjump

let pp_module_jump fmt { interface; alias; hash; _ } =
  let open Format in
  fprintf fmt "%s#%d %a"
    (Mark.remove interface.intf_modname.module_name)
    hash
    (pp_opt (fun fmt alias -> fprintf fmt "(alias:%s)" alias))
    alias

let make_mjump
    (mname : ModuleName.t)
    (interface : Surface.Ast.interface)
    ?alias
    k =
  let mjump : mjump =
    let hash = Hashtbl.hash (Mark.get interface.intf_modname.module_name) in
    { hash; name = ModuleName.to_string mname; alias; interface }
  in
  match k with
  | `Def -> Module_def mjump
  | `Use -> Module_use mjump
  | `Decl -> Module_decl mjump

let pp_var ppf =
  let open Format in
  function
  | Topdef { name; hash; _ } -> fprintf ppf "topdef: %s#%d" name hash
  | Definition { name; hash; _ } -> fprintf ppf "definition: %s#%d" name hash
  | Declaration { name; hash; _ } -> fprintf ppf "declaration: %s#%d" name hash
  | Usage { name; hash; _ } -> fprintf ppf "usage: %s#%d" name hash
  | Type { name; hash; _ } -> fprintf ppf "type: %s#%d" name hash
  | Literal typ -> fprintf ppf "literal: %a" Print.typ_debug typ
  | Module_use mjump -> fprintf ppf "mod_usage: %a" pp_module_jump mjump
  | Module_decl mjump -> fprintf ppf "mod_decl: %a" pp_module_jump mjump
  | Module_def mjump -> fprintf ppf "mod_def: %a" pp_module_jump mjump

module PMap = Position_map.Make (struct
  type t = var

  let compare = compare
  let format = pp_var
end)

type variables = PMap.pmap
type t = { variables : variables; lookup_table : lookup_entry LTable.t }

let pp_table ppf { declaration; definitions; usages; types } =
  let open Format in
  fprintf ppf "decl: %a, def: %a, usage: %a, types: %a"
    (pp_print_option
       ~none:(fun fmt () -> fprintf fmt "none")
       (fun fmt _ -> fprintf fmt "some"))
    declaration
    (pp_print_option
       ~none:(fun fmt () -> fprintf fmt "none")
       (fun fmt _ -> fprintf fmt "some"))
    definitions
    (pp_print_option
       ~none:(fun fmt () -> fprintf fmt "none")
       (fun fmt _ -> fprintf fmt "some"))
    usages
    (pp_print_option
       ~none:(fun fmt () -> fprintf fmt "none")
       (fun fmt _ -> fprintf fmt "some"))
    types

let find key proj bindings =
  List.find_opt (fun (k, _) -> proj key = proj k) bindings

let populate_struct_def
    (ctx : Desugared.Name_resolution.context)
    (module_lookup : ModuleName.t -> Surface.Ast.interface * string option)
    name
    fields
    m
    k =
  let m =
    (* Populate struct's path components *)
    List.fold_left
      (fun acc mname ->
        let interface, alias = module_lookup mname in
        let mjump = make_mjump mname interface ?alias `Use in
        let pos = Mark.get (ModuleName.get_info mname) in
        PMap.add pos mjump acc)
      m (StructName.path name)
  in
  match
    find name StructName.to_string (StructName.Map.bindings ctx.structs)
  with
  | None ->
    (* Structure not found: should not happen *)
    Log.warn (fun m ->
        m "struct %a not found in context" StructName.format name);
    m
  | Some (struct_decl, (struct_typ, _)) ->
    (* Add a reference for the structure *)
    let struct_pos = StructName.get_info name |> Mark.get in
    let m =
      let name = StructName.to_string name in
      let hash = Hashtbl.hash (StructName.get_info struct_decl) in
      let typ =
        let pos_decl = Mark.get (StructName.get_info struct_decl) in
        Mark.add pos_decl (TStruct struct_decl)
      in
      let jump = { hash; name; typ } in
      PMap.(add struct_pos (Usage jump) m |> add struct_pos (Type jump))
    in
    (* Add a reference for all fields *)
    StructField.Map.fold
      (fun sfield e m ->
        match
          find sfield StructField.to_string
            (StructField.Map.bindings struct_typ)
        with
        | None ->
          Log.warn (fun m ->
              m "struct %a not found in context" StructName.format name);
          k e m
        | Some (decl_field, typ) ->
          let name = StructField.to_string sfield in
          let _, pos = StructField.get_info sfield in
          let hash = hash_info (module StructField) decl_field in
          let var = Definition { name; hash; typ } in
          let m = PMap.add pos var m in
          k e m)
      fields m

let populate_enum_inject
    (ctx : Desugared.Name_resolution.context)
    (module_lookup : ModuleName.t -> Surface.Ast.interface * string option)
    (enum_name : EnumName.t)
    (cons : EnumConstructor.t)
    m =
  let m =
    (* Populate enum's path components *)
    List.fold_left
      (fun acc mname ->
        let interface, alias = module_lookup mname in
        let mjump = make_mjump mname interface ?alias `Use in
        let pos = Mark.get (ModuleName.get_info mname) in
        PMap.add pos mjump acc)
      m (EnumName.path enum_name)
  in
  let enum_decl_opt =
    find enum_name EnumName.to_string (EnumName.Map.bindings ctx.enums)
  in
  match enum_decl_opt with
  | None -> m
  | Some (enum_decl, (enum_decl_typ, _vis)) -> (
    let typ =
      let pos_decl = Mark.get (EnumName.get_info enum_decl) in
      Mark.add pos_decl (TEnum enum_decl)
    in
    (* Add enum reference *)
    let m =
      let name = EnumName.to_string enum_name in
      let pos = Mark.get (EnumName.get_info enum_name) in
      let hash = Hashtbl.hash (EnumName.get_info enum_decl) in
      let jump = { name; hash; typ } in
      PMap.(add pos (Usage jump) m |> add pos (Type jump))
    in
    (* Add enum's constructor reference *)
    find cons EnumConstructor.to_string
      (EnumConstructor.Map.bindings enum_decl_typ)
    |> function
    | None -> m
    | Some (enum_constr_decl, _typ) ->
      let name = EnumConstructor.to_string cons in
      let hash = hash_info (module EnumConstructor) enum_constr_decl in
      let var = Usage { name; hash; typ } in
      let pos = Mark.get (EnumConstructor.get_info cons) in
      PMap.add pos var m)

let populate_scopecall ctx module_lookup pos scope args acc f =
  let ((name, decl_pos) as scope_info) = ScopeName.get_info scope in
  let acc =
    (* Populate scope's path components *)
    List.fold_left
      (fun acc mname ->
        let interface, alias = module_lookup mname in
        let mjump = make_mjump mname interface ?alias `Use in
        let pos = Mark.get (ModuleName.get_info mname) in
        PMap.add pos mjump acc)
      acc (ScopeName.path scope)
  in
  let typ : typ =
    let scope_ctx = Desugared.Name_resolution.get_scope_context ctx scope in
    TStruct scope_ctx.scope_out_struct, decl_pos
  in
  let hash = Hashtbl.hash scope_info in
  let var = Usage { name; hash; typ } in
  let acc = PMap.add pos var acc in
  ScopeVar.Map.fold
    (fun scope_var (def_pos, e) acc ->
      let name = ScopeVar.to_string scope_var in
      let typ =
        (* FIXME: [var_sig_typ] map doesn't contain valid var ids *)
        (ScopeVar.Map.to_seq ctx.var_typs
        |> Seq.find (fun (sv', _) -> ScopeVar.to_string sv' = name)
        |> Option.get
        |> snd)
          .Desugared.Name_resolution.var_sig_typ
      in
      let hash = hash_info (module ScopeVar) scope_var in
      let var = Definition { name; hash; typ } in
      let acc = PMap.add def_pos var acc in
      f e acc)
    args acc

let traverse_expr
    (ctx : Desugared.Name_resolution.context)
    (module_lookup : ModuleName.t -> Surface.Ast.interface * string option)
    (e : (scopelang, typed) gexpr)
    m =
  let open Shared_ast in
  let open Catala_utils in
  let rec f (bnd_ctx : Bindlib.ctxt) (e : (scopelang, typed) gexpr) acc =
    let (Typed { pos; ty = typ }) = Mark.get e in
    match Mark.remove e with
    | EDefault { excepts; just; cons } ->
      let acc =
        match Mark.remove just with
        (* ignore boolean conditions *)
        | ELit (LBool _) -> acc
        | _ -> f bnd_ctx just acc
      in
      let lfold x acc = List.fold_left (fun acc x -> f bnd_ctx x acc) acc x in
      acc |> lfold excepts |> f bnd_ctx cons
    | ELit _l -> PMap.add pos (Literal typ) acc
    | ELocation (ScopelangScopeVar { name }) ->
      let (scope_var : ScopeVar.t), pos = name in
      let name = ScopeVar.to_string scope_var in
      let hash = hash_info (module ScopeVar) scope_var in
      let var = Usage { name; hash; typ } in
      PMap.add pos var acc
    | ELocation (ToplevelVar { name; _ }) ->
      let (topdef_var : TopdefName.t), _ = name in
      let name = TopdefName.to_string topdef_var in
      let hash = Hashtbl.hash (TopdefName.get_info topdef_var) in
      let var = Usage { name; hash; typ } in
      PMap.add pos var acc
    | EStructAccess { name = _; e = sub_expr; field } ->
      let name = StructField.to_string field in
      let (Typed { pos = _; ty = _ }) = Mark.get sub_expr in
      let hash = hash_info (module StructField) field in
      let var = Usage { name; hash; typ } in
      let acc = PMap.add pos var acc in
      f bnd_ctx sub_expr acc
    | EStruct { name; fields } ->
      populate_struct_def ctx module_lookup name fields acc (f bnd_ctx)
    | EInj { name; e; cons } ->
      let acc = populate_enum_inject ctx module_lookup name cons acc in
      if Mark.remove e = ELit LUnit then
        (* Don't recurse when the next expression is nil *)
        acc
      else f bnd_ctx e acc
    | EAbs { binder; pos; tys } ->
      let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
      let xs_info =
        List.mapi (fun i (pos, tau) -> xs.(i), pos, tau) (List.combine pos tys)
      in
      let acc =
        List.fold_left
          (fun acc (var, pos, typ) ->
            let name =
              Bindlib.name_of var ^ string_of_int (Bindlib.uid_of var)
            in
            let hash = Hashtbl.hash name in
            let var = Definition { name; hash; typ } in
            PMap.add pos var acc)
          acc xs_info
      in
      f bnd_ctx body acc
    | EVar var ->
      let name = Bindlib.name_of var ^ string_of_int (Bindlib.uid_of var) in
      let hash = Hashtbl.hash name in
      let var = Usage { name; hash; typ } in
      PMap.add pos var acc
    | EMatch { name = _; e; cases } ->
      let acc = f bnd_ctx e acc in
      EnumConstructor.Map.fold
        (fun constr e acc ->
          let name = EnumConstructor.to_string constr in
          let hash = hash_info (module EnumConstructor) constr in
          let var = Usage { name; hash; typ } in
          PMap.add pos var acc |> f bnd_ctx e)
        cases acc
    | EScopeCall { scope; args } ->
      populate_scopecall ctx module_lookup pos scope args acc (f bnd_ctx)
    | EEmpty | EIfThenElse _ | EArray _ | EAppOp _ | EApp _ | ETuple _
    | ETupleAccess _ | EFatalError _ | EPureDefault _ | EErrorOnEmpty _ ->
      Expr.shallow_fold (f bnd_ctx) e acc
  in
  Expr.shallow_fold (f Bindlib.empty_ctxt) e m

let rec traverse_typ
    (ctx : Desugared.Name_resolution.context)
    (module_lookup : ModuleName.t -> Surface.Ast.interface * string option)
    ((typ, pos) : naked_typ * Pos.t)
    m : PMap.pmap =
  match typ with
  | TStruct struct_name ->
    let m =
      (* Populate struct's path components *)
      List.fold_left
        (fun acc mname ->
          let interface, alias = module_lookup mname in
          let mjump = make_mjump mname interface ?alias `Use in
          let pos = Mark.get (ModuleName.get_info mname) in
          PMap.add pos mjump acc)
        m
        (StructName.path struct_name)
    in
    let name = StructName.to_string struct_name in
    let hash = Hashtbl.hash (StructName.get_info struct_name) in
    let jump = { name; hash; typ = typ, pos } in
    let m = PMap.add pos (Type jump) m in
    PMap.add pos (Usage jump) m
  | TEnum enum_name ->
    let m =
      (* Populate enum's path components *)
      List.fold_left
        (fun acc mname ->
          let interface, alias = module_lookup mname in
          let mjump = make_mjump mname interface ?alias `Use in
          let pos = Mark.get (ModuleName.get_info mname) in
          PMap.add pos mjump acc)
        m (EnumName.path enum_name)
    in
    let name = EnumName.to_string enum_name in
    let hash = Hashtbl.hash (EnumName.get_info enum_name) in
    let jump = { name; hash; typ = typ, pos } in
    let m = PMap.add pos (Type jump) m in
    PMap.add pos (Usage jump) m
  | TArrow (tl, t) ->
    List.fold_right (traverse_typ ctx module_lookup) (t :: tl) m
  | TTuple tl -> List.fold_right (traverse_typ ctx module_lookup) tl m
  | TOption typ | TArray typ | TDefault typ ->
    traverse_typ ctx module_lookup typ m
  | TLit _lit -> PMap.add pos (Literal (typ, pos)) m
  | TAny | TClosureEnv -> m

let traverse_scope_def
    ctx
    (module_lookup : ModuleName.t -> Surface.Ast.interface * string option)
    (rule : typed rule)
    m : PMap.pmap =
  match rule with
  | ScopeVarDefinition { var; typ; io = _; e }
  | SubScopeVarDefinition { var; typ; var_within_origin_scope = _; e } ->
    let var, pos_l = var in
    let name = ScopeVar.to_string var in
    let hash = hash_info (module ScopeVar) var in
    let var = Definition { name; hash; typ } in
    let m = List.fold_right (fun p -> PMap.add p var) pos_l m in
    let m = traverse_typ ctx module_lookup typ m in
    traverse_expr ctx module_lookup e m
  | Assertion e -> traverse_expr ctx module_lookup e m

let traverse_scope_sig ctx module_lookup scope m : PMap.pmap =
  ScopeVar.Map.fold
    (fun scope_var var_ty m ->
      (* FIXME: subscope type definition is buggy *)
      let m = traverse_typ ctx module_lookup var_ty.svar_out_ty m in
      let name = ScopeVar.to_string scope_var in
      let pos = Mark.get (ScopeVar.get_info scope_var) in
      let hash = hash_info (module ScopeVar) scope_var in
      let var = Declaration { name; hash; typ = var_ty.svar_out_ty } in
      PMap.add pos var m)
    scope.scope_sig m

let traverse_scope
    ctx
    (module_lookup : ModuleName.t -> Surface.Ast.interface * string option)
    (scope : typed scope_decl)
    m : PMap.pmap =
  let m = traverse_scope_sig ctx module_lookup scope m in
  List.fold_right
    (traverse_scope_def ctx module_lookup)
    scope.scope_decl_rules m

let traverse_topdef
    ctx
    (module_lookup : ModuleName.t -> Surface.Ast.interface * string option)
    (topdef : TopdefName.t)
    ((e, typ, _vis) : typed expr * typ * visibility)
    m : PMap.pmap =
  let name = TopdefName.to_string topdef in
  let topdef_pos = snd (TopdefName.get_info topdef) in
  let hash = Hashtbl.hash (TopdefName.get_info topdef) in
  let topdef = Topdef { name; hash; typ } in
  let m = PMap.add topdef_pos topdef m in
  traverse_expr ctx module_lookup e m

let traverse_ctx
    (ctx : Desugared.Name_resolution.context)
    (module_lookup : ModuleName.t -> Surface.Ast.interface * string option)
    m : PMap.pmap =
  let m =
    StructName.Map.fold
      (fun struct_name (fields, _vis) m ->
        let name = StructName.to_string struct_name in
        let pos = Mark.get (StructName.get_info struct_name) in
        let hash = Hashtbl.hash (StructName.get_info struct_name) in
        let m =
          PMap.add pos
            (Declaration { name; hash; typ = TStruct struct_name, pos })
            m
        in
        StructField.Map.fold
          (fun sf typ m ->
            let m = traverse_typ ctx module_lookup typ m in
            let name = StructField.to_string sf in
            let pos = Mark.get (StructField.get_info sf) in
            let hash = hash_info (module StructField) sf in
            let var = Declaration { name; hash; typ } in
            PMap.add pos var m)
          fields m)
      ctx.structs m
  in
  let m =
    EnumName.Map.fold
      (fun enum_name (cstrs, _vis) m ->
        let name = EnumName.to_string enum_name in
        let pos = Mark.get (EnumName.get_info enum_name) in
        let hash = Hashtbl.hash (EnumName.get_info enum_name) in
        let enum_type = TEnum enum_name, pos in
        let m = PMap.add pos (Declaration { name; hash; typ = enum_type }) m in
        EnumConstructor.Map.fold
          (fun ecstr typ m ->
            let m = traverse_typ ctx module_lookup typ m in
            let name = EnumConstructor.to_string ecstr in
            let pos = Mark.get (EnumConstructor.get_info ecstr) in
            let hash = hash_info (module EnumConstructor) ecstr in
            let var = Declaration { name; hash; typ = enum_type } in
            PMap.add pos var m)
          cstrs m)
      ctx.enums m
  in
  m

let traverse
    (ctx : Desugared.Name_resolution.context)
    (module_lookup : ModuleName.t -> Surface.Ast.interface * string option)
    (prog : typed program)
    (variables : PMap.pmap) =
  let m =
    ModuleName.Map.fold
      (fun _m_name decl_map acc ->
        ScopeName.Map.fold
          (fun _sname scope acc ->
            traverse_scope_sig ctx module_lookup (Mark.remove scope) acc)
          decl_map acc)
      prog.program_modules variables
  in
  let m =
    TopdefName.Map.fold
      (traverse_topdef ctx module_lookup)
      prog.program_topdefs m
  in
  let all_scopes =
    ScopeName.Map.values prog.program_scopes |> List.map Mark.remove
  in
  let m = List.fold_right (traverse_scope ctx module_lookup) all_scopes m in
  traverse_ctx ctx module_lookup m

let add_scope_definitions
    (ctx : Desugared.Name_resolution.context)
    (surface : Surface.Ast.program)
    (variables : PMap.t) =
  let rec process vars_acc = function
    | Surface.Ast.CodeBlock (cb, _, _) ->
      List.fold_left
        (fun vars item ->
          match Mark.remove item with
          | Surface.Ast.ScopeUse s_use ->
            let pos =
              Mark.get s_use.scope_use_name
              |> fun p -> Pos.overwrite_law_info p []
            in
            let name = Mark.remove s_use.scope_use_name in
            let scope_uid =
              Desugared.Name_resolution.get_scope ctx s_use.scope_use_name
            in
            let typ : typ =
              let ty_pos = ScopeName.get_info scope_uid |> Mark.get in
              let scope_ctx =
                Desugared.Name_resolution.get_scope_context ctx scope_uid
              in
              TStruct scope_ctx.scope_out_struct, ty_pos
            in
            let hash = Hashtbl.hash (ScopeName.get_info scope_uid) in
            let var = Definition { name; hash; typ } in
            PMap.add pos var vars
          | ScopeDecl _ | StructDecl _ | EnumDecl _ | Topdef _ -> vars)
        vars_acc cb
    | LawHeading (_, ls) -> List.fold_left process vars_acc ls
    | LawInclude _ | ModuleDef _ | ModuleUse _ | LawText _ -> vars_acc
  in
  List.fold_left process variables surface.program_items

let populate_modules
    input_src
    (modules_itf : Surface.Ast.interface ModuleName.Map.t)
    (prog : typed program)
    (surface : Surface.Ast.program)
    (acc : PMap.t) :
    PMap.t * (ModuleName.t -> Surface.Ast.interface * string option) =
  let module C = Map.Make (String) in
  let convert_map =
    ModuleName.Map.fold
      (fun mname _ acc -> C.add (ModuleName.to_string mname) mname acc)
      prog.program_modules C.empty
  in
  let process_module_use
      acc
      { Surface.Ast.mod_use_name = name, pname; mod_use_alias = alias, palias }
      =
    try
      let mname = C.find name convert_map in
      let mname_pos = Mark.get (ModuleName.get_info mname) in
      let interface = ModuleName.Map.find mname modules_itf in
      let alias = if alias <> name then Some alias else None in
      let decl_mjump = make_mjump mname interface ?alias `Decl in
      let def_mjump = make_mjump mname interface ?alias `Def in
      let acc =
        if Option.is_some alias then PMap.add palias decl_mjump acc else acc
      in
      let acc = PMap.add pname decl_mjump acc in
      PMap.add mname_pos def_mjump acc
    with exn ->
      Log.warn (fun m ->
          m "exception %s while processing module use" (Printexc.to_string exn));
      acc
  in
  let itf_lookup =
    let itf_map =
      ModuleName.Map.mapi
        (fun mname itf ->
          let alias =
            List.find_map
              (fun { Surface.Ast.mod_use_name; mod_use_alias } ->
                if
                  ModuleName.to_string mname = fst mod_use_name
                  && mod_use_name <> mod_use_alias
                then Some (fst mod_use_alias)
                else None)
              surface.program_used_modules
          in
          itf, alias)
        modules_itf
    in
    fun mname -> ModuleName.Map.find mname itf_map
  in
  let acc =
    match surface.program_module with
    | None -> acc
    | Some { module_name; module_external = _ } -> (
      try
        let interface = Surface.Parser_driver.load_interface input_src in
        let pos = Mark.get module_name in
        let mname = ModuleName.fresh module_name in
        let def_mjump = make_mjump mname interface `Def in
        PMap.add pos def_mjump acc
      with exn ->
        (* Ignore errors, the current file might be *)
        Log.warn (fun m ->
            m "exception %s while loading current file's module signature"
              (Printexc.to_string exn));
        acc)
  in
  List.fold_left process_module_use acc surface.program_used_modules, itf_lookup

let populate
    input_src
    (ctx : Desugared.Name_resolution.context)
    (modules_itf : Surface.Ast.interface ModuleName.Map.t)
    (surface : Surface.Ast.program)
    (prog : typed program) : t =
  let variables, mod_lookup =
    populate_modules input_src modules_itf prog surface PMap.empty
  in
  let variables = traverse ctx mod_lookup prog variables in
  let variables = add_scope_definitions ctx surface variables in
  let add f = function None -> Some (f empty_lookup) | Some v -> Some (f v) in
  let add_def p =
    add (fun v ->
        {
          v with
          definitions =
            (match v.definitions with
            | None -> Some [p]
            | Some l -> Some (p :: l));
        })
  in
  let add_decl p = add (fun v -> { v with declaration = Some p }) in
  let add_usage p =
    add (fun v ->
        {
          v with
          usages =
            (match v.usages with None -> Some [p] | Some l -> Some (p :: l));
        })
  in
  let add_type p =
    add (fun v ->
        {
          v with
          types =
            (match v.types with None -> Some [p] | Some l -> Some (p :: l));
        })
  in
  let lookup_table =
    PMap.fold
      (fun p data tbl ->
        PMap.DS.fold
          (fun x tbl ->
            let res =
              match x with
              | Topdef jump ->
                Some ((fun m -> add_def p m |> add_decl p), jump.hash)
              | Definition jump -> Some (add_def p, jump.hash)
              | Declaration jump -> Some (add_decl p, jump.hash)
              | Usage jump -> Some (add_usage p, jump.hash)
              | Type jump -> Some (add_type p, jump.hash)
              | Module_use { hash; _ } -> Some (add_usage p, hash)
              | Module_decl { hash; _ } -> Some (add_decl p, hash)
              | Module_def { hash; _ } -> Some (add_def p, hash)
              | Literal _ -> None
            in
            match res with
            | Some (add, hash) -> LTable.update hash add tbl
            | _ -> tbl)
          data tbl)
      variables LTable.empty
  in
  { variables; lookup_table }

let lookup (tables : t) (p : Pos.t) : lookup_entry list =
  let proj = function
    | Topdef j | Definition j | Declaration j | Usage j | Type j ->
      LTable.find_opt j.hash tables.lookup_table
    | Module_use { hash; _ } | Module_decl { hash; _ } | Module_def { hash; _ }
      ->
      LTable.find_opt hash tables.lookup_table
    | Literal _ -> None
  in
  PMap.lookup p tables.variables
  |> function None -> [] | Some s -> List.filter_map proj (PMap.DS.elements s)

type type_lookup =
  | Expr of typ
  | Type of typ
  | Module of (Surface.Ast.interface * string option)

let lookup_type (tables : t) (p : Pos.t) :
    (Lsp.Types.Range.t * type_lookup list) option =
  let proj = function
    | Topdef j | Definition j | Declaration j | Usage j -> Expr j.typ
    | Type j -> Type j.typ
    | Literal typ -> Expr typ
    | Module_use { interface; alias; _ }
    | Module_decl { interface; alias; _ }
    | Module_def { interface; alias; _ } ->
      Module (interface, alias)
  in
  PMap.lookup_with_range p tables.variables
  |> function
  | None -> None | Some (r, d) -> Some (r, List.map proj (PMap.DS.elements d))

let var_to_symbol (p : Pos.t) (var : var) : Linol_lwt.SymbolInformation.t option
    =
  let open Linol_lwt in
  let res =
    match var with
    | Topdef v -> Some (v.name, SymbolKind.Constant)
    | Definition v -> Some (v.name, Function)
    | Declaration v -> Some (v.name, Interface)
    | Usage v -> Some (v.name, Variable)
    | Module_use v | Module_decl v | Module_def v -> Some (v.name, Module)
    | Type _ | Literal _ -> None
  in
  Option.map
    (fun (name, (kind : SymbolKind.t)) ->
      let location =
        Location.create ~range:(range_of_pos p)
          ~uri:(DocumentUri.of_path (Pos.get_file p))
      in
      Linol_lwt.SymbolInformation.create ~kind ~name ~location ())
    res
