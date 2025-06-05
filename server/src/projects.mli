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

module ScanItemFiles : Set.S with type elt = Clerk_scan.item

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
