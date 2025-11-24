(*****************)

open Server_types
open Server_state
open Catala_utils

val pp_range : Format.formatter -> Range.t -> unit

val lookup_suggestions :
  Doc_id.t -> diagnostics -> Range.t -> (Range.t * string list) option

val lookup_suggestions_by_pos :
  Doc_id.t ->
  diagnostics ->
  Linol_lwt.Position.t ->
  (Range.t * string list) option

val all_symbols_as_warning :
  Doc_id.doc_id ->
  processing_result option ->
  (Doc_id.doc_id * Diagnostic.t list) list

val of_position : Pos.t -> string * Range.t

val generic_lookup :
  ?doc_id:Doc_id.doc_id ->
  document_state ->
  Linol_lwt.Position.t ->
  (Jump_table.lookup_entry -> 'a list option) ->
  'a list option

val lookup_declaration :
  ?doc_id:Doc_id.doc_id ->
  document_state ->
  Linol_lwt.Position.t ->
  (string * Range.t) list option

val lookup_def :
  doc_id:Doc_id.doc_id ->
  document_state ->
  Linol_lwt.Position.t ->
  (string * Range.t) list option

val lookup_usages :
  ?doc_id:Doc_id.doc_id ->
  document_state ->
  Linol_lwt.Position.t ->
  (string * Range.t) list option

val get_hover_type :
  ?markdown:bool ->
  document_state ->
  Linol_lwt.Position.t ->
  Linol_lwt.Hover.t option

val lookup_type_declaration :
  document_state -> Linol_lwt.Position.t -> (string * Range.t) option

val lookup_document_symbols :
  document_state -> Linol_lwt.SymbolInformation.t list

val lookup_lenses : document_state -> Linol_lwt.CodeLens.t list option
