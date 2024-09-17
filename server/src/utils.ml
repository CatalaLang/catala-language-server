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

let send_notification ?(type_ = MessageType.Warning) ~notify_back message =
  let message = Format.sprintf "Catala LSP: %s" message in
  let notif = Lsp.Server_notification.ShowMessage { message; type_ } in
  notify_back#send_notification notif

let check_catala_format_availability () =
  let open Lwt.Infix in
  Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null
    ("", [| "catala-format" |])
  >>= function
  | WEXITED 2 -> Lwt.return true
  | WEXITED 127 (* Not found *) | WEXITED _ -> Lwt.return false
  | WSIGNALED _ -> Lwt.return_false
  | WSTOPPED _ -> Lwt.return_false

let try_format_document ~notify_back ~doc_content ~doc_path :
    TextEdit.t list option Lwt.t =
  let open Lwt.Syntax in
  let language =
    match String.sub doc_path (String.length doc_path - 2) 2 with
    | "fr" -> "catala_fr"
    | "pl" -> "catala_pl"
    | "en" | _ | (exception _) -> "catala_en"
  in
  let stdin_r, stdin_w = Lwt_unix.pipe_out ~cloexec:true () in
  let stdin = Lwt_io.of_fd ~mode:Lwt_io.output stdin_w in
  let* () = Lwt_io.write stdin doc_content in
  let* () = Lwt_io.close stdin in
  let stderr_r, stderr_w = Lwt_unix.pipe_in ~cloexec:true () in
  let proc =
    Lwt_process.open_process_in ~timeout:5. ~stdin:(`FD_move stdin_r)
      ~stderr:(`FD_move stderr_w)
      ("", [| "catala-format"; "-l"; language |])
  in
  let stderr = Lwt_io.of_fd ~mode:Lwt_io.input stderr_r in
  let read ic =
    Lwt.finalize (fun () -> Lwt_io.read ic) (fun () -> Lwt_io.close ic)
  in
  let* r = proc#status in
  match r with
  | Unix.WSIGNALED _ -> Lwt.return_none
  | Unix.WSTOPPED _ -> Lwt.return_none
  | Unix.WEXITED 0 ->
    (* Everything went fine *)
    Log.info (fun m -> m "document formatting successful");
    let* formatted_content = read proc#stdout in
    if formatted_content = "" then (
      (* Don't do anything if the stdout is empty, it's fishy.. *)
      Log.info (fun m ->
          m
            "no formatted output: the document is either empty or something \
             went wrong");
      Lwt.return_none)
    else
      let eof_range =
        let l = String.split_on_char '\n' doc_content in
        let l = List.rev l in
        let len = List.length l in
        Position.create ~character:(String.length (List.hd l)) ~line:len
      in
      let range =
        Range.create ~start:{ line = 0; character = 0 } ~end_:eof_range
      in
      Lwt.return_some [TextEdit.create ~newText:formatted_content ~range]
  | Unix.WEXITED n ->
    Log.info (fun m -> m "failed to format document '%s'" doc_path);
    let* err = read stderr in
    if err = "" then
      let* () =
        Format.kasprintf
          (send_notification ~type_:MessageType.Warning ~notify_back)
          "Code formatting failed: catala-format exited with error code %d" n
      in
      Lwt.return_none
    else
      let lines = String.split_on_char '\n' err in
      let take_n l n =
        let rec loop acc = function
          | [], _ | _, 0 -> List.rev acc
          | h :: t, n -> loop (h :: acc) (t, pred n)
        in
        loop [] (l, n)
      in
      let l =
        if List.length lines > 10 then
          take_n lines 5 @ ["..."] @ (take_n (List.rev lines) 5 |> List.rev)
        else lines
      in
      let* () =
        Format.kasprintf
          (send_notification ~type_:MessageType.Warning ~notify_back)
          "Code formatting failed.\nReason:\n%s" (String.concat "\n" l)
      in
      Lwt.return_none
