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

open Shared_ast
open Catala_utils
open Server_types

type processing_result = {
  prg : typed Scopelang.Ast.program;
  used_modules : ModuleName.t File.Map.t;
  jump_table : Jump_table.t Lazy.t;
}

type buffer_state = Saved | Modified of { contents : string }

type document_state = {
  document_id : Doc_id.t;
  locale : Global.backend_lang;
  buffer_state : buffer_state;
  project : Projects.project;
  project_file : Projects.project_file;
  last_valid_result : processing_result option;
}

val make_document :
  buffer_state ->
  Doc_id.doc_id ->
  Projects.project ->
  Projects.project_file ->
  document_state

type server_state = {
  projects : Projects.t;
  open_documents : document_state Doc_id.Map.t;
  diagnostics : diagnostics;
}

type locked_server_state

val use : locked_server_state -> (server_state -> 'a Lwt.t) -> 'a Lwt.t

val use_if_ready :
  locked_server_state -> (server_state -> 'a Lwt.t) -> 'a option Lwt.t

val use_when_ready :
  locked_server_state -> (server_state -> 'a Lwt.t) -> 'a Lwt.t

val use_now : locked_server_state -> (server_state -> 'a Lwt.t) -> 'a Lwt.t

val use_and_update :
  locked_server_state -> (server_state -> ('a * server_state) Lwt.t) -> 'a Lwt.t

val delayed_update :
  ?delay:float ->
  Doc_id.doc_id ->
  locked_server_state ->
  (server_state -> server_state Lwt.t) ->
  unit Lwt.t

val make : unit -> locked_server_state
