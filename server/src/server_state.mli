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

type 'result document_state = {
  document_id : Doc_id.t;
  contents : string option;
  project : Projects.project;
  project_file : Projects.project_file;
  last_valid_result : 'result option;
  errors : Diagnostic.t Utils.RangeMap.t Doc_id.Map.t;
}

val make_empty_document :
  ?contents:string ->
  Doc_id.doc_id ->
  Projects.project ->
  Projects.project_file ->
  'a document_state

type 'result server_state = {
  projects : Projects.t;
  documents : 'result document_state Doc_id.Map.t;
}

type 'result locked_server_state

val use :
  'result locked_server_state -> ('result server_state -> 'a Lwt.t) -> 'a Lwt.t

val use_if_ready :
  'result locked_server_state ->
  ('result server_state -> 'a Lwt.t) ->
  'a option Lwt.t

val use_now :
  'result locked_server_state -> ('result server_state -> 'a Lwt.t) -> 'a Lwt.t

val use_and_update :
  'result locked_server_state ->
  ('result server_state -> ('a * 'result server_state) Lwt.t) ->
  'a Lwt.t

val delayed_update :
  ?delay:float ->
  Doc_id.doc_id ->
  'result locked_server_state ->
  ('result server_state -> 'result server_state Lwt.t) ->
  unit Lwt.t

val make : unit -> 'result locked_server_state
