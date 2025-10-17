(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
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
open Linol_lsp

module Doc_id = struct
  type doc_id = File.t
  type t = doc_id

  let of_lsp_uri (document : Uri0.t) : doc_id =
    Uri0.to_path document |> Uri.pct_decode

  let of_catala_pos (pos : Pos.t) : doc_id = Pos.get_file pos
  let to_lsp_uri file = Uri0.of_path file
  let format = Format.pp_print_string
  let compare = String.compare
  let equal = String.equal

  module Map = File.Map
  module Set = File.Set

  let of_file = Fun.id
end
