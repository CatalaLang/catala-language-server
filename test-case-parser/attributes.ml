open Catala_utils
open Shared_ast

type Pos.attr +=
  | TestUi
  | Uid of string
  | TestDescription of string
  | TestTitle of string
  | ArrayItemLabel of string

let string_value ~name ~pos = function
  | String (s, _) -> s
  | _ -> Message.error ~pos "testcase.%s expects a string" name

let register () =
  let on_expressions = function
    | Desugared.Name_resolution.Expression _ -> true
    | _ -> false
  in
  let on_scopes = function
    | Desugared.Name_resolution.ScopeDecl -> true
    | _ -> false
  in
  let string_attr name contexts make =
    Driver.Plugin.register_attribute ~plugin:"testcase" ~path:[name] ~contexts
      (fun ~pos v -> Some (make (string_value ~name ~pos v)))
  in
  Driver.Plugin.register_attribute ~plugin:"testcase" ~path:["testui"]
    ~contexts:on_scopes (fun ~pos:_ _ -> Some TestUi);
  string_attr "uid" on_expressions (fun s -> Uid s);
  string_attr "test_description" on_scopes (fun s -> TestDescription s);
  string_attr "test_title" on_scopes (fun s -> TestTitle s);
  string_attr "array_item_label" on_expressions (fun s -> ArrayItemLabel s)

let is_editor_test pos = Pos.has_attr pos Test && Pos.has_attr pos TestUi
let non_empty = function "" -> None | s -> Some s

let title pos =
  Pos.get_attr pos (function TestTitle s -> non_empty s | _ -> None)

let description pos =
  Pos.get_attr pos (function TestDescription s -> non_empty s | _ -> None)

let tested_scope (testing_scope : Desugared.Ast.scope) =
  match ScopeVar.Map.bindings testing_scope.scope_sub_scopes with
  | [sub] -> Ok sub
  | subs -> Error (List.length subs)
