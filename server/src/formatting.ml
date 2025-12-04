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

module Diag = Diagnostic
open Linol_lwt
open Catala_utils
open Utils
open Server_types

let to_catala_format_lang : Global.backend_lang -> string = function
  | En -> "catala_en"
  | Fr -> "catala_fr"
  | Pl -> "catala_pl"

let try_format_document ~notify_back ~doc_content (doc_id : Doc_id.t) :
    TextEdit.t list option Lwt.t =
  let open Lwt.Syntax in
  let ( let*? ) x f =
    let* opt = x in
    match opt with None -> Lwt.return_none | Some x -> f x
  in
  let doc_path = (doc_id :> File.t) in
  let*? language =
    match Clerk_scan.get_lang (doc_id :> string) with
    | None ->
      let* () =
        send_notification ~type_:MessageType.Warning ~notify_back
          "Code formatting failed: cannot infer file language. Make sure the \
           file extension is valid."
      in
      Lwt.return_none
    | Some lang -> Lwt.return_some (to_catala_format_lang lang)
  in
  Lwt.catch
    (fun () ->
      let* catala_format_path = lookup_catala_format_config_path notify_back in
      begin
        let path = Option.value ~default:"" catala_format_path in
        Lwt_process.with_process_full ~timeout:10.
          ( path,
            [|
              "catala-format";
              "--language";
              language;
              "--buffer-name";
              (doc_id :> string);
            |] )
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
      | Unix.WEXITED n -> (
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
          try
            let* () =
              Scanf.sscanf error_output
                "catala-format: Parsing error between lines %d:%d and %d:%d"
                (fun sl sc el ec ->
                  let pos : Pos.t =
                    Pos.from_info (doc_id :> string) sl sc el ec
                  in
                  let diag =
                    Diag.error_p pos (`String "catala-format: Parsing error")
                  in
                  notify_back#set_uri (Doc_id.to_lsp_uri doc_id);
                  notify_back#send_diagnostic [diag])
            in
            Lwt.return_none
          with Scanf.Scan_failure _ ->
            let* () =
              send_notification ~type_:MessageType.Warning ~notify_back
                error_output
            in
            Lwt.return_none))
    (fun _ -> Lwt.return_none)

let try_format_document ~notify_back ~doc_content (doc_id : Doc_id.t) =
  Lwt.catch
    (fun () -> try_format_document ~notify_back ~doc_content doc_id)
    (fun exn ->
      let open Lwt.Syntax in
      let* () =
        Format.kasprintf
          (send_notification ~type_:MessageType.Warning ~notify_back)
          "Code formatting failed.\nUncaught exception:\n%s"
          (Printexc.to_string exn)
      in
      Lwt.return_none)
