(** The [testcase.*] attributes, shared by the plugin, the LSP and the DAP. *)

open Catala_utils
open Shared_ast

type Pos.attr +=
  | TestUi
  | Uid of string
  | TestDescription of string
  | TestTitle of string
  | ArrayItemLabel of string

val register : unit -> unit

val is_editor_test : Pos.t -> bool
(** [#[test]] and [#[testcase.testui]]: a test the editor owns. *)

val title : Pos.t -> string option
val description : Pos.t -> string option

val tested_scope : Desugared.Ast.scope -> (ScopeVar.t * ScopeName.t, int) result
(** The scope an editor test tests: its only sub-scope. [Error n] when it has
    [n] sub-scopes. *)
