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

type document_status = Opened | Closed

type document_state = {
  document_id : Doc_id.t;
  document_status : document_status;
  diagnostics : Linol_lwt.Jsonrpc2.Diagnostic.t list;
  project : Projects.project;
  process_result : State.file;
}

let format_document_state ppf { document_id; document_status; _ } =
  Format.fprintf ppf "%a (status: %s)" Doc_id.format document_id
    (match document_status with Opened -> "opened" | Closed -> "closed")

type server_state = {
  lock : Lwt_mutex.t;
  projects : Projects.t;
  documents : document_state Doc_id.Map.t;
}
