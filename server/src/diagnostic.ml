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
