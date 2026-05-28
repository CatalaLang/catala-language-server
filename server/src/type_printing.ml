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

let locale_s locale =
  match locale with `En -> "en" | `Fr -> "fr" | `Pl -> assert false

let _for_all = function
  | `En -> "anything of type"
  | `Fr -> "n'importe quel de type"
  | `Pl -> assert false

let list_of = function
  | `En -> "list of"
  | `Fr -> "liste de"
  | `Pl -> assert false

let default = function
  | `En -> "default"
  | `Fr -> "défaut"
  | `Pl -> assert false

let enum = function `En -> "enum" | `Fr -> "énum" | `Pl -> assert false
let struct_s = function `Pl -> assert false | _ -> "structure"

let struct_header = function
  | `En -> struct_s `En
  | `Fr -> struct_s `Fr
  | `Pl -> assert false

let struct_data = function
  | `En -> "data"
  | `Fr -> "donnée"
  | `Pl -> assert false

let content = function
  | `En -> "content"
  | `Fr -> "contenu"
  | `Pl -> assert false

let depends_on = function
  | `En -> "depends on"
  | `Fr -> "dépend de"
  | `Pl -> assert false

let enum_s = function
  | `En -> "enumeration"
  | `Fr -> "énumération"
  | `Pl -> assert false

let enum_header = function
  | `En -> enum_s `En
  | `Fr -> enum_s `Fr
  | `Pl -> assert false

let scope_s = function
  | `En -> "scope"
  | `Fr -> "champ d'application"
  | `Pl -> assert false

let topdef_s = function
  | `En -> "declaration"
  | `Fr -> "déclaration"
  | `Pl -> assert false

let external_type_s = function
  | `En -> "external type"
  | `Fr -> "type externe"
  | `Pl -> assert false

let alias_s = function
  | `En -> "as"
  | `Fr -> "en tant que"
  | `Pl -> assert false

let option_s = function
  | `En -> "optional of"
  | `Fr -> "optionnel de"
  | `Pl -> assert false

let using_s = function
  | `En -> "Using"
  | `Fr -> "Usage de"
  | `Pl -> assert false

let pp_lit locale fmt l =
  fprintf fmt "%s"
  @@ (if locale = `En then fst else snd)
       (match l with
       | TUnit -> "unit", "unit"
       | TBool -> "boolean", "booléen"
       | TInt -> "integer", "entier"
       | TRat -> "decimal", "décimal"
       | TMoney -> "money", "argent"
       | TDuration -> "duration", "durée"
       | TDate -> "date", "date"
       | TPos -> "position", "position")

let pp_typ locale fmt (ty : typ) =
  let untuplify = function [(TTuple tys, _)] -> tys | x -> x in
  let rec pp_typ fmt ty =
    match Mark.remove ty with
    | TLit l -> pp_lit locale fmt l
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
      let s = Utils.rename_alias_in_path locale StructName.map_info s in
      fprintf fmt "@[<hov 2>%a <struct>@]" StructName.format_shortpath s
    | TEnum e ->
      let e = Utils.rename_alias_in_path locale EnumName.map_info e in
      fprintf fmt "@[<hov 2>%a <%s>@]" EnumName.format_shortpath e (enum locale)
    | TOption o -> fprintf fmt "@[<hov 2>%s@ %a@]" (option_s locale) pp_typ o
    | TArray a -> fprintf fmt "@[<hov 2>%s@ %a@]" (list_of locale) pp_typ a
    | TDefault d -> pp_typ fmt d
    | TAbstract t ->
      fprintf fmt "@[<hov 2>%a <abstract>@]" AbstractType.format_shortpath t
    | TClosureEnv -> fprintf fmt "<closure_env>"
    | TError -> fprintf fmt "<error>"
  in
  pp_typ fmt ty

let pp_typ_no_box locale fmt (ty : typ) =
  let untuplify = function [(TTuple tys, _)] -> tys | x -> x in
  let rec pp_typ fmt ty =
    match Mark.remove ty with
    | TLit l -> pp_lit locale fmt l
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
      let s = Utils.rename_alias_in_path locale StructName.map_info s in
      fprintf fmt "%a <struct>" StructName.format_shortpath s
    | TEnum e ->
      let e = Utils.rename_alias_in_path locale EnumName.map_info e in
      fprintf fmt "%a <%s>" EnumName.format_shortpath e (enum locale)
    | TOption o -> fprintf fmt "%s@ %a" (option_s locale) pp_typ o
    | TArray a -> fprintf fmt "%s@ %a" (list_of locale) pp_typ a
    | TDefault d -> fprintf fmt "%a" pp_typ d
    | TAbstract t -> fprintf fmt "%a <abstract>" AbstractType.format_shortpath t
    | TClosureEnv -> fprintf fmt "<closure_env>"
    | TError -> fprintf fmt "<error>"
  in
  pp_typ fmt ty

let expr_type ~markdown locale typ =
  let locale_s =
    match locale with `En -> "en" | `Fr -> "fr" | `Pl -> assert false
  in
  let typ_s =
    if markdown then
      asprintf "```catala_code_%s@\n%a@\n```" locale_s (pp_typ locale) typ
    else asprintf "%a" (pp_typ locale) typ
  in
  typ_s

let pp_struct_field locale fmt ((field_name, typ) : StructField.t * typ) =
  Format.fprintf fmt "@[<hov 2>%s %s %s@ %a@]" (struct_data locale)
    (StructField.to_string field_name)
    (content locale) (pp_typ_no_box locale) typ

let pp_struct_code
    locale
    fmt
    ((struct_name : StructName.t), (field_map : typ StructField.Map.t)) =
  fprintf fmt "@[<v 2>%s %a:@ %a@]" (struct_header locale)
    StructName.format_shortpath struct_name
    (pp_print_list ~pp_sep:pp_print_cut (pp_struct_field locale))
    (StructField.Map.bindings field_map)

let struct_code ~markdown locale field_map =
  let locale_s =
    match locale with `En -> "en" | `Fr -> "fr" | `Pl -> assert false
  in
  let typ_s =
    if markdown then
      asprintf "```catala_code_%s@\n%a@\n```" locale_s (pp_struct_code locale)
        field_map
    else asprintf "%a" (pp_struct_code locale) field_map
  in
  typ_s

let pp_enum_field locale fmt ((field_name, typ) : EnumConstructor.t * typ) =
  match fst typ with
  | TLit TUnit ->
    Format.fprintf fmt "-- %s" (EnumConstructor.to_string field_name)
  | _ ->
    Format.fprintf fmt "-- %s %s %a"
      (EnumConstructor.to_string field_name)
      (content locale) (pp_typ_no_box locale) typ

let pp_enum_code
    locale
    fmt
    ((enum_name : EnumName.t), (field_map : typ EnumConstructor.Map.t)) =
  fprintf fmt "@[<v 2>%s %a:@ %a@]" (enum_header locale)
    EnumName.format_shortpath enum_name
    (pp_print_list ~pp_sep:pp_print_cut (pp_enum_field locale))
    (EnumConstructor.Map.bindings field_map)

let enum_code ~markdown locale field_map =
  let locale_s =
    match locale with `En -> "en" | `Fr -> "fr" | `Pl -> assert false
  in
  let typ_s =
    if markdown then
      asprintf "```catala_code_%s@\n%a@\n```" locale_s (pp_enum_code locale)
        field_map
    else asprintf "%a" (pp_enum_code locale) field_map
  in
  typ_s

let data_type
    ~markdown
    (prg : Shared_ast.typed Scopelang.Ast.program)
    locale
    (typ : naked_typ Mark.pos) =
  let ctx = prg.program_ctx in
  match Mark.remove typ with
  | TLit _
  | TArrow (_, _)
  | TTuple _ | TOption _ | TArray _ | TDefault _ | TForAll _ | TVar _
  | TAbstract _ | TClosureEnv | TError ->
    expr_type ~markdown locale typ
  | TStruct sname -> (
    let struct_ctx = ctx.ctx_structs in
    StructName.Map.find_opt sname struct_ctx
    |> function
    | None -> expr_type ~markdown locale typ
    | Some field_map -> struct_code ~markdown locale (sname, field_map))
  | TEnum ename -> (
    let enum_ctx = ctx.ctx_enums in
    EnumName.Map.find_opt ename enum_ctx
    |> function
    | None -> expr_type ~markdown locale typ
    | Some field_map -> enum_code ~markdown locale (ename, field_map))

let svar_input_s_opt locale =
  let context_s = function
    | `En -> "context"
    | `Fr -> "contexte"
    | `Pl -> assert false
  in
  let input_s = function
    | `En -> "input"
    | `Fr -> "entrée"
    | `Pl -> assert false
  in
  function
  | Catala_runtime.NoInput -> None
  | OnlyInput -> Some (input_s locale)
  | Reentrant -> Some (context_s locale)

let svar_output_s_opt locale b =
  let output_s = function
    | `En -> "output"
    | `Fr -> "résultat"
    | `Pl -> assert false
  in
  if b then Some (output_s locale) else None

let svar_io_s locale (var_io : Desugared.Ast.io) =
  let { Desugared.Ast.io_output = out, _; io_input = input, _ } = var_io in
  match svar_input_s_opt locale input, svar_output_s_opt locale out with
  | Some s, Some s' -> Some (sprintf "%s %s" s s')
  | None, Some s | Some s, None -> Some s
  | None, None -> None (* internal *)

let svar_internal_s = function
  | `En -> "output"
  | `Fr -> "résultat"
  | `Pl -> assert false

let svar_state_s = function
  | `En -> "state"
  | `Fr -> "état"
  | `Pl -> assert false

let pp_scope_var
    ?(skip_internals = true)
    locale
    fmt
    ((scope_var : ScopeVar.t), (scope_ty : Scopelang.Ast.scope_var_ty)) =
  let open Format in
  let var_name = ScopeVar.to_string scope_var in
  match svar_io_s locale scope_ty.svar_io with
  | None when skip_internals -> (* internal *) ()
  | None ->
    fprintf fmt "@[<hov 2>%s %s %s %a@]" (svar_internal_s locale) var_name
      (content locale) (pp_typ_no_box locale) scope_ty.svar_out_ty
  | Some io_s ->
    fprintf fmt "@[<hov 2>%s %s %s %a@]" io_s var_name (content locale)
      (pp_typ_no_box locale) scope_ty.svar_out_ty

let is_svar_internal (scope_ty : Scopelang.Ast.scope_var_ty) =
  (not (Mark.remove scope_ty.svar_io.io_output))
  && Mark.remove scope_ty.svar_io.io_input = Catala_runtime.NoInput

let pp_scope ?(skip_internals = true) locale fmt (scope_name, scope_vars) =
  let open Format in
  let short_scope_name = ScopeName.get_info scope_name |> fst in
  fprintf fmt "@[<v 2>%s %s:@ %a@]" (scope_s locale) short_scope_name
    (pp_print_list ~pp_sep:pp_print_space (pp_scope_var ~skip_internals locale))
    (ScopeVar.Map.bindings scope_vars
    |> List.filter (fun (_, v) ->
        (not skip_internals) || not (is_svar_internal v)))

let sig_type
    ~markdown
    locale
    scope_name
    (scope_vars : Scopelang.Ast.scope_var_ty ScopeVar.Map.t) =
  let typ_s =
    if markdown then
      asprintf "```catala_code_%s@\n%a@\n```" (locale_s locale)
        (pp_scope locale) (scope_name, scope_vars)
    else asprintf "%a" (pp_scope locale) (scope_name, scope_vars)
  in
  typ_s

let pp_topdef locale fmt (name, topdef_typ) =
  fprintf fmt "%s %s : %a" (topdef_s locale) name (pp_typ locale) topdef_typ

let pp_module
    locale
    (prg : Shared_ast.typed Scopelang.Ast.program)
    fmt
    (mcontent : Surface.Ast.module_content) =
  let open Format in
  let open Surface.Ast in
  let ctx = prg.program_ctx in
  (* From Surface.Ast :

     > Invariant: an interface shall only contain [*Decl] elements, or [Topdef]
     > elements with [topdef_expr = None] *)
  let pp_code_block fmt code_item =
    match Mark.remove code_item with
    | ScopeDecl sdecl ->
      fprintf fmt "%s %s" (scope_s locale) (Mark.remove sdecl.scope_decl_name)
    | StructDecl sdecl ->
      fprintf fmt "%s %s" (struct_s locale) (Mark.remove sdecl.struct_decl_name)
    | EnumDecl edecl ->
      fprintf fmt "%s %s" (enum_s locale) (Mark.remove edecl.enum_decl_name)
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
        pp_topdef locale fmt (Mark.remove top_def.topdef_name, topdef_typ))
    | AbstractTypeDecl t ->
      fprintf fmt "%s %s" (external_type_s locale) (Mark.remove t)
    | ScopeUse _ -> ()
  in
  match mcontent.module_items with
  | Interface items ->
    fprintf fmt "@[<v>%a@]"
      (pp_print_list ~pp_sep:pp_print_space pp_code_block)
      (List.map Mark.remove items)
  | Code (ls : law_structure list) ->
    let rec loop = function
      | [] -> ()
      | LawInclude _ :: t | ModuleDef _ :: t | ModuleUse _ :: t | LawText _ :: t
        ->
        loop t
      | LawHeading (_, ls) :: t ->
        loop ls;
        loop t
      | CodeBlock (cb, _, _) :: t ->
        fprintf fmt "@[<v>%a@]"
          (pp_print_list ~pp_sep:pp_print_space pp_code_block)
          cb;
        loop t
    in
    loop ls

let module_type
    ~markdown
    prg
    locale
    ?alias
    (module_content : Surface.Ast.module_content) =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  let module_name =
    let name = fst module_content.module_modname.module_name in
    try Utils.lookup_alias_name locale name
    with String.Map.Not_found _ | Not_found -> name
  in
  let () =
    fprintf ppf "**Module %s%a**@\n" module_name
      (Utils.pp_opt (fun fmt alias ->
           Format.fprintf fmt " (%s %s)" (alias_s locale) alias))
      alias;
    if markdown then fprintf ppf "```catala_code_%s@\n" (locale_s locale);
    fprintf ppf "%a@\n" (pp_module locale prg) module_content;
    if markdown then fprintf ppf "```"
  in
  Buffer.contents buf

let typ_to_content ~markdown prg locale (kind : Jump_table.type_lookup) =
  match kind with
  | Type typ | Expr typ -> data_type ~markdown prg locale typ
  | Scope scope_decl ->
    sig_type ~markdown locale scope_decl.scope_decl_name scope_decl.scope_sig
  | Module (itf, alias) -> module_type ~markdown prg locale ?alias itf

let typ_to_markdown prg locale (kind : Jump_table.type_lookup) : MarkupContent.t
    =
  let value = typ_to_content ~markdown:true prg locale kind in
  MarkupContent.create ~kind:Markdown ~value

let typ_to_raw_string prg locale (kind : Jump_table.type_lookup) :
    MarkedString.t =
  let value = typ_to_content ~markdown:false prg locale kind in
  { MarkedString.value; language = None }
