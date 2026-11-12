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

open Linol_lwt
open Catala_utils
open Shared_ast
open Format
open Languages
open Locale

let pp_lit (lang : Languages.lang) fmt l =
  let types = lang.builtins.types in
  pp_print_string fmt
    (match l with
    | TUnit -> assert false
    | TBool -> types.boolean
    | TInt -> types.integer
    | TRat -> types.decimal
    | TMoney -> types.money
    | TDuration -> types.duration
    | TDate -> types.date
    | TPos -> types.position)

let pp_typ ((k, lang) : code * lang) fmt (ty : typ) =
  let untuplify = function [(TTuple tys, _)] -> tys | x -> x in
  let rec pp_typ fmt ty =
    match Mark.remove ty with
    | TLit l -> pp_lit lang fmt l
    | TVar v -> fprintf fmt "<%s>" (Bindlib.name_of v)
    | TForAll b ->
      let _, typ = Bindlib.unmbind b in
      (* We do not display the for-all information: it's too verbose *)
      pp_typ fmt typ
    | TArrow (t1, t2) ->
      fprintf fmt "@[<hov>%a@ →@ %a@]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ →@ ") pp_typ)
        (untuplify t1) pp_typ t2
    | TTuple tys ->
      fprintf fmt "@[<hov 2>(%a)@]"
        (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_typ)
        tys
    | TStruct s ->
      let s = Utils.rename_alias_in_path k StructName.map_info s in
      fprintf fmt "@[<hov 2>%a <struct>@]" StructName.format_shortpath s
    | TEnum e ->
      let e = Utils.rename_alias_in_path k EnumName.map_info e in
      fprintf fmt "@[<hov 2>%a <%s>@]" EnumName.format_shortpath e
        lang.keywords.enum
    | TOption o -> fprintf fmt "@[<hov 2>%s@ %a@]" lang.keywords.option pp_typ o
    | TArray a -> fprintf fmt "@[<hov 2>%s@ %a@]" lang.keywords.list pp_typ a
    | TDefault d -> pp_typ fmt d
    | TAbstract t ->
      fprintf fmt "@[<hov 2>%a <abstract>@]" AbstractType.format_shortpath t
    | TClosureEnv -> fprintf fmt "<closure_env>"
    | TError -> fprintf fmt "<error>"
  in
  pp_typ fmt ty

let pp_typ_no_box ((k, lang) : code * lang) fmt (ty : typ) =
  let untuplify = function [(TTuple tys, _)] -> tys | x -> x in
  let rec pp_typ fmt ty =
    match Mark.remove ty with
    | TLit l -> pp_lit lang fmt l
    | TVar v -> fprintf fmt "<%s>" (Bindlib.name_of v)
    | TForAll b ->
      let _, typ = Bindlib.unmbind b in
      (* We do not display the for-all information: it's too verbose *)
      pp_typ fmt typ
    | TArrow (t1, t2) ->
      fprintf fmt "%a@ → %a"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ →@ ") pp_typ)
        (untuplify t1) pp_typ t2
    | TTuple tys ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_typ)
        tys
    | TStruct s ->
      let s = Utils.rename_alias_in_path k StructName.map_info s in
      fprintf fmt "%a <struct>" StructName.format_shortpath s
    | TEnum e ->
      let e = Utils.rename_alias_in_path k EnumName.map_info e in
      fprintf fmt "%a <%s>" EnumName.format_shortpath e lang.keywords.enum
    | TOption o -> fprintf fmt "%s@ %a" lang.keywords.option pp_typ o
    | TArray a -> fprintf fmt "%s@ %a" lang.keywords.list pp_typ a
    | TDefault d -> fprintf fmt "%a" pp_typ d
    | TAbstract t -> fprintf fmt "%a <abstract>" AbstractType.format_shortpath t
    | TClosureEnv -> fprintf fmt "<closure_env>"
    | TError -> fprintf fmt "<error>"
  in
  pp_typ fmt ty

let expr_type ~markdown (((_, lang) : code * lang) as l) typ =
  let typ_s =
    if markdown then
      asprintf "```catala_code_%s@\n%a@\n```" lang.code (pp_typ l) typ
    else asprintf "%a" (pp_typ l) typ
  in
  typ_s

let pp_struct_field
    (((_, lang) : code * lang) as l)
    fmt
    ((field_name, typ) : StructField.t * typ) =
  Format.fprintf fmt "@[<hov 2>%s %s %s@ %a@]" lang.keywords.data
    (StructField.to_string field_name)
    lang.keywords.content (pp_typ_no_box l) typ

let pp_struct_code
    (((_, lang) : code * lang) as l)
    fmt
    ((struct_name : StructName.t), (field_map : typ StructField.Map.t)) =
  fprintf fmt "@[<v 2>%s %a:@ %a@]" lang.keywords.struct_
    StructName.format_shortpath struct_name
    (pp_print_list ~pp_sep:pp_print_cut (pp_struct_field l))
    (StructField.Map.bindings field_map)

let struct_code ~markdown (((_, lang) : code * lang) as l) field_map =
  let typ_s =
    if markdown then
      asprintf "```catala_code_%s@\n%a@\n```" lang.code (pp_struct_code l)
        field_map
    else asprintf "%a" (pp_struct_code l) field_map
  in
  typ_s

let pp_enum_field
    (((_, lang) : code * lang) as l)
    fmt
    ((field_name, typ) : EnumConstructor.t * typ) =
  match fst typ with
  | TLit TUnit ->
    Format.fprintf fmt "-- %s" (EnumConstructor.to_string field_name)
  | _ ->
    Format.fprintf fmt "-- %s %s %a"
      (EnumConstructor.to_string field_name)
      lang.keywords.content (pp_typ_no_box l) typ

let pp_enum_code
    (((_, lang) : code * lang) as l)
    fmt
    ((enum_name : EnumName.t), (field_map : typ EnumConstructor.Map.t)) =
  fprintf fmt "@[<v 2>%s %a:@ %a@]" lang.keywords.enum EnumName.format_shortpath
    enum_name
    (pp_print_list ~pp_sep:pp_print_cut (pp_enum_field l))
    (EnumConstructor.Map.bindings field_map)

let enum_code ~markdown (((_, lang) : code * lang) as l) field_map =
  let typ_s =
    if markdown then
      asprintf "```catala_code_%s@\n%a@\n```" lang.code (pp_enum_code l)
        field_map
    else asprintf "%a" (pp_enum_code l) field_map
  in
  typ_s

let data_type
    ~markdown
    (prg : Shared_ast.typed Scopelang.Ast.program)
    lang
    (typ : naked_typ Mark.pos) =
  let ctx = prg.program_ctx in
  match Mark.remove typ with
  | TLit _
  | TArrow (_, _)
  | TTuple _ | TOption _ | TArray _ | TDefault _ | TForAll _ | TVar _
  | TAbstract _ | TClosureEnv | TError ->
    expr_type ~markdown lang typ
  | TStruct sname -> (
    let struct_ctx = ctx.ctx_structs in
    StructName.Map.find_opt sname struct_ctx
    |> function
    | None -> expr_type ~markdown lang typ
    | Some field_map -> struct_code ~markdown lang (sname, field_map))
  | TEnum ename -> (
    let enum_ctx = ctx.ctx_enums in
    EnumName.Map.find_opt ename enum_ctx
    |> function
    | None -> expr_type ~markdown lang typ
    | Some field_map -> enum_code ~markdown lang (ename, field_map))

let svar_input_s_opt (lang : Languages.lang) = function
  | Catala_runtime.NoInput -> None
  | OnlyInput -> Some lang.keywords.input
  | Reentrant -> Some lang.keywords.context

let svar_output_s_opt lang b = if b then Some lang.keywords.output else None

let svar_io_s (lang : Languages.lang) (var_io : Desugared.Ast.io) =
  let { Desugared.Ast.io_output = out, _; io_input = input, _ } = var_io in
  match svar_input_s_opt lang input, svar_output_s_opt lang out with
  | Some s, Some s' -> Some (sprintf "%s %s" s s')
  | None, Some s | Some s, None -> Some s
  | None, None -> None (* internal *)

let pp_scope_var
    ?(skip_internals = true)
    (((_, lang) : code * lang) as l)
    fmt
    ((scope_var : ScopeVar.t), (scope_ty : Scopelang.Ast.scope_var_ty)) =
  let open Format in
  let var_name = ScopeVar.to_string scope_var in
  match svar_io_s lang scope_ty.svar_io with
  | None when skip_internals -> (* internal *) ()
  | None ->
    fprintf fmt "@[<hov 2>%s %s %s %a@]" lang.keywords.internal var_name
      lang.keywords.content (pp_typ_no_box l) scope_ty.svar_out_ty
  | Some io_s ->
    fprintf fmt "@[<hov 2>%s %s %s %a@]" io_s var_name lang.keywords.content
      (pp_typ_no_box l) scope_ty.svar_out_ty

let is_svar_internal (scope_ty : Scopelang.Ast.scope_var_ty) =
  (not (Mark.remove scope_ty.svar_io.io_output))
  && Mark.remove scope_ty.svar_io.io_input = Catala_runtime.NoInput

let pp_scope
    ?(skip_internals = true)
    (((_, lang) : code * lang) as l)
    fmt
    (scope_name, scope_vars) =
  let open Format in
  let short_scope_name = ScopeName.get_info scope_name |> fst in
  fprintf fmt "@[<v 2>%s %s:@ %a@]" lang.keywords.scope short_scope_name
    (pp_print_list ~pp_sep:pp_print_space (pp_scope_var ~skip_internals l))
    (ScopeVar.Map.bindings scope_vars
    |> List.filter (fun (_, v) ->
        (not skip_internals) || not (is_svar_internal v)))

let sig_type
    ~markdown
    (((_, lang) : code * lang) as l)
    scope_name
    (scope_vars : Scopelang.Ast.scope_var_ty ScopeVar.Map.t) =
  let typ_s =
    if markdown then
      asprintf "```catala_code_%s@\n%a@\n```" lang.code (pp_scope l)
        (scope_name, scope_vars)
    else asprintf "%a" (pp_scope l) (scope_name, scope_vars)
  in
  typ_s

let pp_topdef (((_, lang) : code * lang) as l) fmt (name, topdef_typ) =
  fprintf fmt "%s %s : %a" lang.keywords.declaration name (pp_typ l) topdef_typ

let pp_module
    (((_, lang) : code * lang) as l)
    (prg : Shared_ast.typed Scopelang.Ast.program)
    fmt
    (mcontent : Surface.Ast.module_content) =
  let open Format in
  let open Surface.Ast in
  let ctx = prg.program_ctx in
  (* From Surface.Ast :

     > Invariant: an interface shall only contain [*Decl] elements, or [Topdef]
     > elements with [topdef_expr = None] *)
  let pp_code_block fmt (code_item, _vis) =
    match Mark.remove code_item with
    | ScopeDecl sdecl ->
      fprintf fmt "%s %s" lang.keywords.scope
        (Mark.remove sdecl.scope_decl_name)
    | StructDecl sdecl ->
      fprintf fmt "%s %s" lang.keywords.struct_
        (Mark.remove sdecl.struct_decl_name)
    | EnumDecl edecl ->
      fprintf fmt "%s %s" lang.keywords.enum (Mark.remove edecl.enum_decl_name)
    | Topdef top_def -> (
      match
        TopdefName.Map.bindings ctx.ctx_topdefs
        |> List.find_opt (fun (topdef_name, _) ->
            String.equal
              (TopdefName.base topdef_name)
              (Mark.remove top_def.topdef_name))
      with
      | None -> () (* ?? *)
      | Some (_, (topdef_typ, _)) ->
        fprintf fmt "%a" (pp_topdef l)
          (Mark.remove top_def.topdef_name, topdef_typ))
    | AbstractTypeDecl t ->
      fprintf fmt "%s %s" lang.directives.external_ (Mark.remove t)
    | ScopeUse _ -> ()
  in
  match mcontent.module_items with
  | Interface items ->
    fprintf fmt "@[<v>%a@]"
      (pp_print_list ~pp_sep:pp_print_space pp_code_block)
      items
  | Code (ls : law_structure list) ->
    let rec loop = function
      | [] -> ()
      | LawInclude _ :: t | ModuleDef _ :: t | ModuleUse _ :: t | LawText _ :: t
        ->
        loop t
      | LawHeading (_, ls) :: t ->
        loop ls;
        loop t
      | CodeBlock { code; is_metadata; _ } :: t ->
        fprintf fmt "@[<v>%a@]"
          (pp_print_list ~pp_sep:pp_print_space pp_code_block)
          (List.map
             (fun item -> item, if is_metadata then Public else Private)
             code);
        loop t
    in
    loop ls

let module_type
    ~markdown
    prg
    (((k, lang) : code * lang) as l)
    ?alias
    (module_content : Surface.Ast.module_content) =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  let module_name =
    let name = fst module_content.module_modname.module_name in
    try Utils.lookup_alias_name k name
    with String.Map.Not_found _ | Not_found -> name
  in
  let () =
    fprintf ppf "**Module %s%a**@\n" module_name
      (Utils.pp_opt (fun fmt alias ->
           Format.fprintf fmt " (%s %s)" lang.directives.module_alias alias))
      alias;
    if markdown then fprintf ppf "```catala_code_%s@\n" lang.code;
    fprintf ppf "%a@\n" (pp_module l prg) module_content;
    if markdown then fprintf ppf "```"
  in
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let typ_to_content ~markdown prg lang (kind : Jump_table.type_lookup) =
  match kind with
  | Type typ | Expr typ -> data_type ~markdown prg lang typ
  | Scope scope_decl ->
    sig_type ~markdown lang scope_decl.scope_decl_name scope_decl.scope_sig
  | Module (itf, alias) -> module_type ~markdown prg lang ?alias itf

let typ_to_markdown prg lang (kind : Jump_table.type_lookup) : MarkupContent.t =
  let value = typ_to_content ~markdown:true prg lang kind in
  MarkupContent.create ~kind:Markdown ~value

let typ_to_raw_string prg lang (kind : Jump_table.type_lookup) : MarkedString.t
    =
  let value = typ_to_content ~markdown:false prg lang kind in
  { MarkedString.value; language = None }
