(* This file is part of the Catala project. Copyright (C) 2024 Inria.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

  let test_case_parser _options : unit =
    Printf.printf "in test_case_parser module\n"
  
  let term = 
    let open Cmdliner.Term in
    const test_case_parser

  (* For now, can be invoked through 
     `catala test-case-parser --plugin-dir _build/default/ <FILE>`
     but we need to figure out distribution through vscode or otherwise. *)
   let () =
     Driver.Plugin.register "test-case-parser" term
