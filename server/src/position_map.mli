(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria, contributor:
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

open Server_types
open Catala_utils

module type Data = sig
  type t

  val compare : t -> t -> int
  val format : Format.formatter -> t -> unit
end

module MakeTree (D : Data) : sig
  module DS : Set.S with type elt = D.t

  type itv = (int * int) * (int * int)

  type node = Node of { itv : itv; data : DS.t; children : tree }
  and tree = node list

  type t = tree
end

module Make (D : Data) : sig
  module DS : Set.S with type elt = D.t
  module Tree : module type of MakeTree (D)

  type acc
  type pmap = Tree.t Doc_id.Map.t (* Exposed for testing *)

  val format : Format.formatter -> pmap -> unit
  val format_entry : Format.formatter -> Pos.t * D.t -> unit

  (** Construction *)

  val empty_acc : acc
  val add : Pos.t -> D.t -> acc -> acc
  val add_all : Pos.t -> D.t list -> acc -> acc
  val finalize : acc -> pmap

  (** Queries *)

  val lookup : Pos.t -> pmap -> DS.t option
  val lookup_with_range : Pos.t -> pmap -> (Range.t * DS.t) option
  val lookup_best_on_line : Doc_id.t -> int -> pmap -> D.t option
  val lookup_on_line : Doc_id.t -> int -> pmap -> DS.t option
  val files : pmap -> Doc_id.t list

  (** Iterators *)

  val fold : (Pos.t -> DS.t -> 'acc -> 'acc) -> pmap -> 'acc -> 'acc

  val fold_on_file :
    Doc_id.doc_id -> (Pos.t -> DS.t -> 'acc -> 'acc) -> pmap -> 'acc -> 'acc

  val iter : (Pos.t -> DS.t -> unit) -> pmap -> unit
end
