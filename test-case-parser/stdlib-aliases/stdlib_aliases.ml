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

(* Standard library modules a file may use without [> Using], by name and by
   their language-suffixed alias. Shared by the language server and the testcase
   plugin. TODO: officialize this list in catala/clerk directly *)

open Catala_utils

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

let en_aliases = List.map (fun s -> s ^ "_en") en_names
let fr_aliases = List.map (fun s -> s ^ "_fr") en_names
let en_map = String.Map.of_list (List.combine en_aliases en_names)
let fr_map = String.Map.of_list (List.combine fr_aliases fr_names)

(** The module an alias stands for, e.g. [Date_fr] ↦ [Date]. *)
let target (lang : Global.backend_lang) alias =
  String.Map.find_opt alias
    (match lang with `Fr -> fr_map | `En | `Pl -> en_map)

(** Names and aliases that need no [> Using] line. *)
let implicit : Global.backend_lang -> string list = function
  | `Fr -> fr_names @ fr_aliases
  | `En | `Pl -> en_names @ en_aliases
