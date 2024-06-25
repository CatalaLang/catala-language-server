type t = Lsp.Types.Diagnostic.t

open Lsp.Types

let lsp_pos line character = Lsp.Types.Position.create ~line ~character
let lsp_range start end_ = Lsp.Types.Range.create ~start ~end_
let start_pos = lsp_pos 1 1
let start_range = lsp_range start_pos start_pos

let pos_to_loc (pos : Catala_utils.Pos.t) :
    Linol_lwt.Position.t * Linol_lwt.Position.t =
  let open Catala_utils.Pos in
  ( {
      line = pred @@ get_start_line pos;
      character = pred @@ get_start_column pos;
    },
    { line = pred @@ get_end_line pos; character = pred @@ get_end_column pos }
  )

let warn_r range message =
  Lsp.Types.Diagnostic.create ~range ~severity:Warning ~source:"catala-lsp"
    ~message ()

let error_r range message =
  Lsp.Types.Diagnostic.create ~range ~severity:Error ~source:"catala-lsp"
    ~message ()

let range_of_pos (pos : Catala_utils.Pos.t) : Range.t =
  let start, end_ = pos_to_loc pos in
  { Range.start; end_ }

let unclosed_range_of_pos (pos : Catala_utils.Pos.t) : Range.t =
  let start, end_ = pos_to_loc pos in
  { Range.start; end_ = { end_ with character = end_.character + 100_000 } }

let warn_p pos msg = warn_r (range_of_pos pos) msg
let error_p pos msg = error_r (range_of_pos pos) msg

let diag_r (severity : DiagnosticSeverity.t) range msg =
  match severity with
  | Error -> error_r range msg
  | Warning -> warn_r range msg
  | Information | Hint -> failwith "diag_r: TODO"

let diag_p (severity : DiagnosticSeverity.t) pos msg = diag_r severity pos msg
