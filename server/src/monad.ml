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

module Option = struct
  let ( let* ) = Option.bind
  let ( let*? ) x f = match x with None -> None | Some x -> f x

  let ( let*?! ) (x, log) f =
    match x with
    | None ->
      log ();
      None
    | Some x -> f x

  let nil_msg = Fun.id
  let return = Option.some
  let some = Option.some
  let ok = some
  let none = Option.none
  let somek x = some x, nil_msg
end
