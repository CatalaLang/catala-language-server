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

open Lsp
open Types
open Catala_utils
open Shared_ast
open Format

let for_all = function
  | Global.En -> "anything of type"
  | Fr -> "n'importe quel de type"
  | Pl -> assert false

let list_of = function
  | Global.En -> "list of"
  | Fr -> "liste de"
  | Pl -> assert false

let default = function
  | Global.En -> "default"
  | Fr -> "défaut"
  | Pl -> assert false

let enum = function Global.En -> "enum" | Fr -> "énum" | Pl -> assert false
let struct_s = function Global.Pl -> assert false | _ -> "structure"

let struct_header = function
  | Global.En -> "declaration " ^ struct_s En
  | Fr -> "déclaration " ^ struct_s Fr
  | Pl -> assert false

let struct_data = function
  | Global.En -> "data"
  | Fr -> "donnée"
  | Pl -> assert false

let content = function
  | Global.En -> "content"
  | Fr -> "contenu"
  | Pl -> assert false

let enum_s = function
  | Global.En -> "enumeration"
  | Fr -> "énumération"
  | Pl -> assert false

let enum_header = function
  | Global.En -> "declaration " ^ enum_s En
  | Fr -> "déclaration " ^ enum_s Fr
  | Pl -> assert false

let scope_s = function
  | Global.En -> "scope"
  | Fr -> "champ d'application"
  | Pl -> assert false

let topdef_s = function
  | Global.En -> "declaration"
  | Fr -> "déclaration"
  | Pl -> assert false

let alias_s = function
  | Global.En -> "as"
  | Fr -> "en tant que"
  | Pl -> assert false

let pp_lit locale fmt l =
  fprintf fmt "%s"
  @@ (if locale = Global.En then fst else snd)
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
  let rec pp_typ fmt ty =
    match Mark.remove ty with
    | TLit l -> pp_lit locale fmt l
    | TVar v -> pp_print_string fmt (Bindlib.name_of v)
    | TForAll b ->
      let _, typ = Bindlib.unmbind b in
      fprintf fmt "@[<hov 2>%s %a@]" (for_all locale) pp_typ typ
    | TArrow ([t1], t2) -> fprintf fmt "@[<hov>%a@ →@ %a@]" pp_typ t1 pp_typ t2
    | TArrow (t1, t2) ->
      fprintf fmt "@[<hov>(%a)@ →@ %a@]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_typ)
        t1 pp_typ t2
    | TTuple tys ->
      fprintf fmt "@[<hov 2>(%a)@]"
        (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_typ)
        tys
    | TStruct s -> fprintf fmt "@[<hov 2>%a <struct>@]" StructName.format s
    | TEnum e ->
      fprintf fmt "@[<hov 2>%a <%s>@]" EnumName.format e (enum locale)
    | TOption o -> fprintf fmt "@[<hov 2>%a@ <option>@]" pp_typ o
    | TArray a -> fprintf fmt "@[<hov 2>%s@ %a@]" (list_of locale) pp_typ a
    | TDefault d -> fprintf fmt "@[<hov 2>%a@ <%s>@]" pp_typ d (default locale)
    | TClosureEnv -> fprintf fmt "<closure_env>"
  in
  pp_typ fmt ty

let pp_typ_no_box locale fmt (ty : typ) =
  let rec pp_typ fmt ty =
    match Mark.remove ty with
    | TLit l -> pp_lit locale fmt l
    | TVar v -> pp_print_string fmt (Bindlib.name_of v)
    | TForAll b ->
      let _, typ = Bindlib.unmbind b in
      fprintf fmt "%s %a" (for_all locale) pp_typ typ
    | TArrow ([t1], t2) -> fprintf fmt "%a@ →@ %a" pp_typ t1 pp_typ t2
    | TArrow (t1, t2) ->
      fprintf fmt "(%a)@ →@ %a"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_typ)
        t1 pp_typ t2
    | TTuple tys ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_typ)
        tys
    | TStruct s -> fprintf fmt "%a <struct>" StructName.format s
    | TEnum e -> fprintf fmt "%a <%s>" EnumName.format e (enum locale)
    | TOption o -> fprintf fmt "%a@ <option>" pp_typ o
    | TArray a -> fprintf fmt "%s@ %a" (list_of locale) pp_typ a
    | TDefault d -> fprintf fmt "%a@ <%s>" pp_typ d (default locale)
    | TClosureEnv -> fprintf fmt "<closure_env>"
  in
  pp_typ fmt ty

let expr_type locale typ =
  let locale_s =
    match locale with Global.En -> "en" | Fr -> "fr" | Pl -> assert false
  in
  let typ_s =
    asprintf "```catala_type_%s@\n%a@\n```" locale_s (pp_typ locale) typ
  in
  MarkupContent.create ~kind:Markdown ~value:typ_s

let pp_struct_field locale fmt ((field_name, typ) : StructField.t * typ) =
  Format.fprintf fmt "@[<hov 2>%s %s %s@ %a@]" (struct_data locale)
    (StructField.to_string field_name)
    (content locale) (pp_typ_no_box locale) typ

let pp_struct_code
    locale
    fmt
    ((struct_name : StructName.t), (field_map : typ StructField.Map.t)) =
  fprintf fmt "@[<v 2>%s %s:@ %a@]" (struct_header locale)
    (StructName.to_string struct_name)
    (pp_print_list ~pp_sep:pp_print_cut (pp_struct_field locale))
    (StructField.Map.bindings field_map)

let struct_code locale field_map =
  let locale_s =
    match locale with Global.En -> "en" | Fr -> "fr" | Pl -> assert false
  in
  let typ_s =
    asprintf "```catala_code_%s@\n%a@\n```" locale_s (pp_struct_code locale)
      field_map
  in
  MarkupContent.create ~kind:Markdown ~value:typ_s

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
  fprintf fmt "@[<v 2>%s %s:@ %a@]" (enum_header locale)
    (EnumName.to_string enum_name)
    (pp_print_list ~pp_sep:pp_print_cut (pp_enum_field locale))
    (EnumConstructor.Map.bindings field_map)

let enum_code locale field_map =
  let locale_s =
    match locale with Global.En -> "en" | Fr -> "fr" | Pl -> assert false
  in
  let typ_s =
    asprintf "```catala_code_%s@\n%a@\n```" locale_s (pp_enum_code locale)
      field_map
  in
  MarkupContent.create ~kind:Markdown ~value:typ_s

let data_type
    (prg : Shared_ast.typed Scopelang.Ast.program)
    locale
    (typ : naked_typ Mark.pos) =
  let ctx = prg.program_ctx in
  match Mark.remove typ with
  | TLit _
  | TArrow (_, _)
  | TTuple _ | TOption _ | TArray _ | TDefault _ | TForAll _ | TVar _
  | TClosureEnv ->
    expr_type locale typ
  | TStruct sname -> (
    let struct_ctx = ctx.ctx_structs in
    StructName.Map.find_opt sname struct_ctx
    |> function
    | None -> expr_type locale typ
    | Some field_map -> struct_code locale (sname, field_map))
  | TEnum ename -> (
    let enum_ctx = ctx.ctx_enums in
    EnumName.Map.find_opt ename enum_ctx
    |> function
    | None -> expr_type locale typ
    | Some field_map -> enum_code locale (ename, field_map))

let svar_input_s_opt locale =
  let context_s = function
    | Global.En -> "context"
    | Fr -> "contexte"
    | Pl -> assert false
  in
  let input_s = function
    | Global.En -> "input"
    | Fr -> "entrée"
    | Pl -> assert false
  in
  function
  | Runtime.NoInput -> None
  | OnlyInput -> Some (input_s locale)
  | Reentrant -> Some (context_s locale)

let svar_output_s_opt locale b =
  let output_s = function
    | Global.En -> "output"
    | Fr -> "résultat"
    | Pl -> assert false
  in
  if b then Some (output_s locale) else None

let svar_io_s locale (var_io : Desugared.Ast.io) =
  let { Desugared.Ast.io_output = out, _; io_input = input, _ } = var_io in
  match svar_input_s_opt locale input, svar_output_s_opt locale out with
  | Some s, Some s' -> Some (sprintf "%s %s" s s')
  | None, Some s | Some s, None -> Some s
  | None, None -> None (* internal *)

let pp_scope_var
    locale
    fmt
    ((scope_var : ScopeVar.t), (scope_ty : Scopelang.Ast.scope_var_ty)) =
  let open Format in
  let var_name = ScopeVar.to_string scope_var in
  match svar_io_s locale scope_ty.svar_io with
  | None -> (* internal *) ()
  | Some io_s ->
    fprintf fmt "@[<hov 2>%s %s %s %a@]" io_s var_name (content locale)
      (pp_typ_no_box locale) scope_ty.svar_out_ty

let is_svar_internal (scope_ty : Scopelang.Ast.scope_var_ty) =
  (not (Mark.remove scope_ty.svar_io.io_output))
  && Mark.remove scope_ty.svar_io.io_input = Runtime.NoInput

let pp_scope locale fmt (scope_name, scope_vars) =
  let open Format in
  let short_scope_name = ScopeName.get_info scope_name |> fst in
  fprintf fmt "@[<v 2>%s %s:@ %a@]" (scope_s locale) short_scope_name
    (pp_print_list ~pp_sep:pp_print_space (pp_scope_var locale))
    (ScopeVar.Map.bindings scope_vars
    |> List.filter (fun (_, v) -> not (is_svar_internal v)))

let sig_type
    locale
    scope_name
    (scope_vars : Scopelang.Ast.scope_var_ty ScopeVar.Map.t) =
  let locale_s =
    match locale with Global.En -> "en" | Fr -> "fr" | Pl -> assert false
  in
  let typ_s =
    asprintf "```catala_code_%s@\n%a@\n```" locale_s (pp_scope locale)
      (scope_name, scope_vars)
  in
  MarkupContent.create ~kind:Markdown ~value:typ_s

let pp_module locale fmt (mcontent : Surface.Ast.module_content) =
  let open Format in
  let open Surface.Ast in
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
    | Topdef top_def ->
      fprintf fmt "%s %s" (topdef_s locale) (Mark.remove top_def.topdef_name)
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

let module_type locale ?alias (module_content : Surface.Ast.module_content) =
  let locale_s =
    match locale with Global.En -> "en" | Fr -> "fr" | Pl -> assert false
  in
  let typ_s =
    asprintf "**Module %s%a**@\n```catala_code_%s@\n%a@\n```"
      (fst module_content.module_modname.module_name)
      (Utils.pp_opt (fun fmt alias ->
           Format.fprintf fmt " (%s %s)" (alias_s locale) alias))
      alias locale_s (pp_module locale) module_content
  in
  MarkupContent.create ~kind:Markdown ~value:typ_s

let typ_to_markdown ?prg locale (kind : Jump_table.type_lookup) =
  match prg, kind with
  | Some prg, Type typ -> data_type prg locale typ
  | _, (Type typ | Expr typ) -> expr_type locale typ
  | _, Scope (sn, scope_var_map) -> sig_type locale sn scope_var_map
  | _, Module (itf, alias) -> module_type locale ?alias itf
