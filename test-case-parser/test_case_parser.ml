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
open Stdlib
open Catala_utils
open Shared_ast

let test_case_parser includes options : unit =
  let prg, _type_ordering = Driver.Passes.desugared options ~includes in
  let modname =
    match prg.program_module_name with
    | Some (m, _) -> ModuleName.to_string m
    | None -> "<unnamed>"
  in
  print_endline modname;
  ModuleName.Map.iter
    (fun k _ -> print_endline (ModuleName.to_string k))
    prg.program_modules;
  ScopeName.Map.iter
    (fun k _ -> print_endline (ScopeName.to_string k))
    prg.program_root.module_scopes

let term =
  let open Cmdliner.Term in
  const test_case_parser $ Cli.Flags.include_dirs

(* For now, can be invoked through
   `catala test-case-parser --plugin-dir _build/default/ <FILE>`
   but we need to figure out distribution through vscode or otherwise. *)
let () = Driver.Plugin.register "test-case-parser" term
