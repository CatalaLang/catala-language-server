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

open Lsp.Types
open Catala_utils

let is_included (p : Pos.t) p' =
  (* true if p is included in p' *)
  Pos.get_file p = Pos.get_file p'
  && Pos.get_start_line p >= Pos.get_start_line p'
  && Pos.get_end_line p <= Pos.get_end_line p'
  && Pos.get_start_column p >= Pos.get_start_column p'
  && Pos.get_end_column p <= Pos.get_end_column p'

let lsp_pos line character = Lsp.Types.Position.create ~line ~character
let lsp_range start end_ = Lsp.Types.Range.create ~start ~end_
let start_pos = lsp_pos 1 1
let start_range = lsp_range start_pos start_pos

let pos_of_range file ({ start; end_ } : Range.t) : Pos.t =
  Pos.from_info file (succ start.line) (succ start.character) (succ end_.line)
    (succ end_.character)

let pos_to_loc (pos : Pos.t) : Linol_lwt.Position.t * Linol_lwt.Position.t =
  let open Pos in
  ( {
      line = pred @@ get_start_line pos;
      character = pred @@ get_start_column pos;
    },
    { line = pred @@ get_end_line pos; character = pred @@ get_end_column pos }
  )

let range_of_pos (pos : Pos.t) : Range.t =
  let start, end_ = pos_to_loc pos in
  { Range.start; end_ }

let unclosed_range_of_pos (pos : Pos.t) : Range.t =
  let start, end_ = pos_to_loc pos in
  { Range.start; end_ = { end_ with character = end_.character + 100_000 } }
