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

open Server_types

module Scan_item : sig
  type t = Clerk_scan.item

  val compare : Clerk_scan.item -> Clerk_scan.item -> int
  val format : Format.formatter -> Clerk_scan.item -> unit
end

module ScanItemFiles : sig
  type elt = Clerk_scan.item
  type t = Set.Make(Scan_item).t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val split : elt -> t -> t * bool * t
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

type project_file = {
  file : Clerk_scan.item;
  including_files : ScanItemFiles.t;
  used_by : ScanItemFiles.t;
}

type project_kind =
  | Clerk of {
      clerk_root_dir : string;
      clerk_config : Clerk_config.config_file;
    }
  | No_clerk

type project = {
  project_dir : string;
  project_kind : project_kind;
  project_files : project_file Doc_id.Map.t;
}

module Projects : Set.S with type elt = project

type projects = Projects.t
type t = projects

val empty : projects
val format_project : Format.formatter -> project -> unit
val format_projects : Format.formatter -> Projects.t -> unit

val lookup_project : Doc_id.t -> Projects.t -> Projects.elt option
(** TODO *)

val init :
  notify_back:Linol_lwt.Jsonrpc2.notify_back ->
  Linol_lwt.InitializeParams.t ->
  projects

val find_file_in_project : Doc_id.t -> project -> project_file option

val reload_project :
  notify_back:Linol_lwt.Jsonrpc2.notify_back ->
  project ->
  Projects.t ->
  Projects.t

exception Project_not_found

val find_or_populate_project :
  notify_back:Linol_lwt.Jsonrpc2.notify_back ->
  Doc_id.t ->
  Projects.t ->
  project_file * Projects.elt * [> `Changed of Projects.t | `Unchanged ]
