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

let pp_opt pp fmt =
  let open Format in
  function None -> fprintf fmt "<none>" | Some v -> fprintf fmt "%a" pp v

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
  Lwt.catch
    (fun () ->
      Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null
        ("", [| "catala-format" |])
      >>= function
      | WEXITED 2 -> Lwt.return true
      | WEXITED 127 (* Not found *) | WEXITED _ -> Lwt.return false
      | WSIGNALED _ -> Lwt.return_false
      | WSTOPPED _ -> Lwt.return_false)
    (fun _ -> Lwt.return_false)

let write_string oc s =
  let open Lwt.Syntax in
  let rec inner nb_write pos len =
    if len = 0 then Lwt.return_unit
    else
      let* nb_write' = Lwt_io.write_from_string oc s pos len in
      if nb_write' = 0 then Lwt.fail End_of_file
      else inner (nb_write + nb_write') (pos + nb_write') (len - nb_write')
  in
  inner 0 0 (String.length s)

let lookup_catala_format_config_path
    (notify_back : Linol_lwt.Jsonrpc2.notify_back) =
  let open Lwt.Syntax in
  let r, w = Lwt.task () in
  let param =
    ConfigurationParams.create
      ~items:[ConfigurationItem.create ~section:"catala.catalaFormatPath" ()]
  in
  let* _req_id =
    notify_back#send_request (Lsp.Server_request.WorkspaceConfiguration param)
      (fun e ->
        match e with
        | Ok (`String x :: _) ->
          Lwt.wakeup w (Some x);
          Lwt.return_unit
        | Ok _ | Error _ ->
          Lwt.wakeup w None;
          Lwt.return_unit)
  in
  r

let try_format_document ~notify_back ~doc_content ~doc_path :
    TextEdit.t list option Lwt.t =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let language =
        match String.sub doc_path (String.length doc_path - 2) 2 with
        | "fr" -> "catala_fr"
        | "pl" -> "catala_pl"
        | "en" | _ | (exception _) -> "catala_en"
      in
      let* catala_format_path = lookup_catala_format_config_path notify_back in
      begin
        match catala_format_path with
        | None ->
          Lwt_process.with_process_full ~timeout:5.
            ("", [| "catala-format"; "-l"; language |])
        | Some path ->
          Lwt_process.with_process_full ~timeout:5.
            (path, [| "catala-format"; "-l"; language |])
      end
      @@ fun proc ->
      let read ic =
        Lwt.finalize (fun () -> Lwt_io.read ic) (fun () -> Lwt_io.close ic)
      in
      let writer =
        Lwt.finalize
          (fun () -> write_string proc#stdin doc_content)
          (fun () -> Lwt_io.close proc#stdin)
      in
      let stdout_reader = read proc#stdout in
      let stderr_reader = read proc#stderr in
      let* r = proc#status in
      match r with
      | Unix.WSIGNALED _ -> Lwt.return_none
      | Unix.WSTOPPED _ -> Lwt.return_none
      | Unix.WEXITED 0 ->
        let* () = writer in
        (* Everything went fine *)
        Log.info (fun m -> m "document formatting successful");
        let* formatted_content = stdout_reader in
        if formatted_content = "" then (
          (* Don't do anything if the stdout is empty, it's fishy.. *)
          Log.info (fun m ->
              m
                "no formatted output: the document is either empty or \
                 something went wrong");
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
        let* error_output = stderr_reader in
        if error_output = "" then
          let* () =
            Format.kasprintf
              (send_notification ~type_:MessageType.Warning ~notify_back)
              "Code formatting failed: catala-format exited with error code %d"
              n
          in
          Lwt.return_none
        else
          let lines = String.split_on_char '\n' error_output in
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
          Lwt.return_none)
    (fun _ -> Lwt.return_none)

let try_format_document ~notify_back ~doc_content ~doc_path =
  Lwt.catch
    (fun () -> try_format_document ~notify_back ~doc_content ~doc_path)
    (fun exn ->
      let open Lwt.Syntax in
      let* () =
        Format.kasprintf
          (send_notification ~type_:MessageType.Warning ~notify_back)
          "Code formatting failed.\nUncaught exception:\n%s"
          (Printexc.to_string exn)
      in
      Lwt.return_none)

let join_paths ?(abs = true) dir path =
  let open File in
  let dir = path_to_list dir in
  let path = path_to_list path in
  let rec loop acc = function
    | [], [] -> acc
    | r, [] | [], r -> List.rev_append r acc
    | h :: t, (h' :: t' as r) ->
      if h <> h' then loop (h :: acc) (t, r) else loop (h :: acc) (t, t')
  in
  loop [] (dir, path)
  |> List.rev
  |> List.fold_left ( / ) (if abs then "/" else "")
