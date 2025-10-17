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

module Doc_id : sig
  type doc_id = private Catala_utils.File.t
  type t = doc_id

  val of_lsp_uri : Linol_lsp.Uri0.t -> doc_id
  val of_catala_pos : Catala_utils.Pos.t -> doc_id
  val to_lsp_uri : doc_id -> Linol_lsp.Uri0.t
  val format : Format.formatter -> doc_id -> unit
  val compare : doc_id -> doc_id -> int
  val equal : doc_id -> doc_id -> bool

  module Map : Catala_utils.Map.S with type key = doc_id
  module Set : Set.S with type elt = doc_id

  val of_file : Catala_utils.File.t -> doc_id
end
