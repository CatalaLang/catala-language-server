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
open Lwt.Syntax

type 'a document_state = {
  document_id : Doc_id.t;
  contents : string option;
  saved : bool;
  project : Projects.project;
  project_file : Projects.project_file;
  last_valid_result : 'a option;
  errors : Diagnostic.t Utils.RangeMap.t Doc_id.Map.t;
}

let make_document ?contents ~saved document_id project project_file =
  {
    document_id;
    contents;
    project;
    saved;
    project_file;
    last_valid_result = None;
    errors = Doc_id.Map.empty;
  }

type 'a server_state = {
  projects : Projects.t;
  open_documents : 'a document_state Doc_id.Map.t;
}

type 'result delayed_state =
  | Ready of 'result server_state
  | Delayed of {
      doc_id : Doc_id.t;
      curr_server_state : 'result server_state;
      delayed_action : unit Lwt.t;
      action : 'result server_state -> 'result server_state Lwt.t;
    }

type 'result locked_server_state = {
  lock : Lwt_mutex.t;
  mutable delayed_state : 'result delayed_state;
}

let use s (f : 'result server_state -> 'a Lwt.t) : 'a Lwt.t =
  Lwt_mutex.with_lock s.lock
  @@ fun () ->
  match s.delayed_state with
  | Ready state -> f state
  | Delayed { doc_id = _; curr_server_state; delayed_action; action } ->
    (* The delayed action is necessarily sleeping otherwise it wouldn't be
       delayed so we can cancel the sleeping thread and force the computation *)
    Lwt.cancel delayed_action;
    Log.debug (fun m -> m "forcing delayed action");
    let* server_state = action curr_server_state in
    s.delayed_state <- Ready server_state;
    f server_state

let use_if_ready s (f : 'result server_state -> 'a Lwt.t) : 'a option Lwt.t =
  Lwt_mutex.with_lock s.lock
  @@ fun () ->
  match s.delayed_state with
  | Ready state ->
    let* x = f state in
    Lwt.return_some x
  | Delayed _ -> Lwt.return_none

let use_now s (f : 'result server_state -> 'a Lwt.t) : 'a Lwt.t =
  Lwt_mutex.with_lock s.lock
  @@ fun () ->
  match s.delayed_state with
  | Ready state -> f state
  | Delayed { curr_server_state; _ } ->
    Log.debug (fun m -> m "processing on potentially outdated document state");
    f curr_server_state

let use_and_update
    s
    (f : 'result server_state -> ('a * 'result server_state) Lwt.t) : 'a Lwt.t =
  Lwt_mutex.with_lock s.lock
  @@ fun () ->
  let* ret, new_state =
    match s.delayed_state with
    | Ready state -> f state
    | Delayed { doc_id = _; curr_server_state; delayed_action; action } ->
      (* The delayed action is necessarily sleeping otherwise it wouldn't be
         delayed so we can cancel the sleeping thread and force the
         computation *)
      Lwt.cancel delayed_action;
      Log.debug (fun m -> m "forcing delayed action");
      let* server_state = action curr_server_state in
      f server_state
  in
  s.delayed_state <- Ready new_state;
  Lwt.return ret

let make_delayed_action ?(delay = 1.) s action =
  Log.debug (fun m -> m "start waiting on delayed action");
  let* () = Lwt_unix.sleep delay in
  Lwt_mutex.with_lock s.lock
  @@ fun () ->
  let curr_state =
    match s.delayed_state with
    | Ready _state -> assert false
    | Delayed { curr_server_state = state; _ } -> state
  in
  Log.debug (fun m -> m "executing delayed action");
  let* new_state = action curr_state in
  s.delayed_state <- Ready new_state;
  Lwt.return_unit

let make_delayed_action ?delay s action =
  let t = make_delayed_action ?delay s action in
  Lwt.on_cancel t (fun () -> Log.debug (fun m -> m "canceling delayed action"));
  t

let delayed_update
    ?delay
    doc_id
    s
    (f : 'result server_state -> 'result server_state Lwt.t) : unit Lwt.t =
  Lwt_mutex.with_lock s.lock
  @@ fun () ->
  match s.delayed_state with
  | Ready state ->
    let delayed_action = make_delayed_action ?delay s f in
    s.delayed_state <-
      Delayed { doc_id; curr_server_state = state; delayed_action; action = f };
    Lwt.return_unit
  | Delayed { doc_id = doc_id'; curr_server_state; delayed_action; action = _ }
    when Doc_id.equal doc_id doc_id' ->
    (* If both updates are towards the same document, we cancel the existing
       one *)
    Lwt.cancel delayed_action;
    let delayed_action = make_delayed_action ?delay s f in
    s.delayed_state <-
      Delayed { doc_id; curr_server_state; delayed_action; action = f };
    Lwt.return_unit
  | Delayed { doc_id = _; curr_server_state; delayed_action; action } ->
    (* If the delayed update is on a different document, we finish up the first
       one before creating a new one *)
    Lwt.cancel delayed_action;
    Log.debug (fun m -> m "forcing delayed action");
    let* new_server_state = action curr_server_state in
    let delayed_action = make_delayed_action ?delay s f in
    s.delayed_state <-
      Delayed
        {
          doc_id;
          curr_server_state = new_server_state;
          delayed_action;
          action = f;
        };
    Lwt.return_unit

let make () =
  let state =
    { projects = Projects.empty; open_documents = Doc_id.Map.empty }
  in
  { lock = Lwt_mutex.create (); delayed_state = Ready state }
