(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
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

open Utils
open Lsp.Types

type t = Diagnostic.t

let warn_r range message =
  Lsp.Types.Diagnostic.create ~range ~severity:Warning ~source:"catala-lsp"
    ~message ()

let error_r range message =
  Lsp.Types.Diagnostic.create ~range ~severity:Error ~source:"catala-lsp"
    ~message ()

let warn_p pos msg = warn_r (range_of_pos pos) msg
let error_p pos msg = error_r (range_of_pos pos) msg

let diag_r (severity : DiagnosticSeverity.t) range msg =
  match severity with
  | Error -> error_r range msg
  | Warning -> warn_r range msg
  | Information | Hint -> failwith "diag_r: TODO"

let diag_p (severity : DiagnosticSeverity.t) pos msg = diag_r severity pos msg
