(* This file is part of the Catala compiler, a specification language
   for tax and social benefits computation rules. Copyright (C) 2026
   Inria, contributors: Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License. You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. *)

open Locale

let () =
  let langs =
    List.map (fun (_, l) -> l.code, l.name) (Languages.languages ())
  in
  let format_lang_obj ppf (code, name) =
    Format.fprintf ppf "{ config_id: '%s', code: '%s', name: '%s' }"
      ("catala_" ^ code) code name
  in
  Format.printf "/* Generated file, do not edit */@\n@\n";
  Format.printf
    "export type CatalaLang = { config_id: string; code: string; name: string \
     };@\n\
     @\n";
  Format.printf "@[<v 2>export const catalaLangs : CatalaLang[] = [@ %a@]@\n]"
    Format.(
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") format_lang_obj)
    langs
