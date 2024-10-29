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

let any = function
  | Global.En -> "any"
  | Fr -> "n'importe quel"
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
       | TDate -> "date", "date")

let pp_typ locale fmt (ty : typ) =
  let rec pp_typ fmt ty =
    match Mark.remove ty with
    | TLit l -> pp_lit locale fmt l
    | TAny -> fprintf fmt "%s" (any locale)
    | TArrow ([t1], t2) -> fprintf fmt "@[<hov>%a → %a@]" pp_typ t1 pp_typ t2
    | TArrow (t1, t2) ->
      fprintf fmt "@[<hov>(%a) → %a@]"
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

let typ_to_markdown locale typ =
  let locale_s =
    match locale with Global.En -> "en" | Fr -> "fr" | Pl -> assert false
  in
  let typ_s =
    asprintf "```catala_type_%s@\n%a@\n```" locale_s (pp_typ locale) typ
  in
  MarkupContent.create ~kind:Markdown ~value:typ_s
