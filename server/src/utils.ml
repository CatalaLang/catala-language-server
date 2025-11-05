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

open Linol_lsp
open Linol_lwt
open Catala_utils
open Server_types

let never_ending = fst (Lwt.wait ())

let pp_opt pp fmt =
  let open Format in
  function None -> fprintf fmt "<none>" | Some v -> fprintf fmt "%a" pp v

let pp_list pp fmt =
  let open Format in
  fprintf fmt "[@ %a@ ]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ;@ ") pp)

let pp_string_list fmt = pp_list Format.pp_print_string fmt

let is_included (p : Pos.t) p' =
  (* true if p is included in p' *)
  Pos.get_file p = Pos.get_file p'
  && Pos.get_start_line p >= Pos.get_start_line p'
  && Pos.get_end_line p <= Pos.get_end_line p'
  && Pos.get_start_column p >= Pos.get_start_column p'
  && Pos.get_end_column p <= Pos.get_end_column p'

module RangeSet = Stdlib.Set.Make (struct
  type t = Range.t

  let compare = compare
end)

module RangeMap = Stdlib.Map.Make (struct
  type t = Range.t

  let compare = compare
end)

let dummy_range =
  Range.create
    ~start:{ line = 0; character = 0 }
    ~end_:{ line = 0; character = 0 }

let lsp_pos line character = Position.create ~line ~character
let lsp_range start end_ = Range.create ~start ~end_
let start_pos = lsp_pos 1 1
let start_range = lsp_range start_pos start_pos

let pos_of_range doc_id ({ start; end_ } : Range.t) : Pos.t =
  Pos.from_info
    (doc_id :> File.t)
    (succ start.line) (succ start.character) (succ end_.line)
    (succ end_.character)

let pos_to_loc (pos : Pos.t) : Position.t * Position.t =
  let open Pos in
  ( {
      line = pred @@ get_start_line pos;
      character = pred @@ get_start_column pos;
    },
    { line = pred @@ get_end_line pos; character = pred @@ get_end_column pos }
  )

let range_of_pos (pos : Pos.t) : Range.t =
  let start, end_ = pos_to_loc pos in
  { start; end_ }

let location_of_pos (pos : Pos.t) : Location.t =
  let start, end_ = pos_to_loc pos in
  let range = { Range.start; end_ } in
  Location.create ~range ~uri:(DocumentUri.of_path (Pos.get_file pos))

let unclosed_range_of_pos (pos : Pos.t) : Range.t =
  let start, end_ = pos_to_loc pos in
  if start.line = end_.line || end_.character > 0 then
    { Range.start; end_ = { line = end_.line + 1; character = 0 } }
  else { Range.start; end_ }

let send_notification
    ?(type_ = MessageType.Warning)
    ~(notify_back : Jsonrpc2.notify_back)
    message =
  let message = Format.sprintf "Catala LSP: %s" message in
  let notif = Server_notification.ShowMessage { message; type_ } in
  notify_back#send_notification notif

let lookup_catala_format_config_path (notify_back : Jsonrpc2.notify_back) =
  let open Lwt.Syntax in
  let r, w = Lwt.task () in
  let param =
    ConfigurationParams.create
      ~items:[ConfigurationItem.create ~section:"catala.catalaFormatPath" ()]
  in
  let* _req_id =
    notify_back#send_request (WorkspaceConfiguration param) (fun e ->
        match e with
        | Ok (`String x :: _) ->
          Lwt.wakeup w (Some x);
          Lwt.return_unit
        | Ok _ | Error _ ->
          Lwt.wakeup w None;
          Lwt.return_unit)
  in
  r

let check_catala_format_availability ~notify_back =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let* path_opt = lookup_catala_format_config_path notify_back in
      let path = Option.value ~default:"catala-format" path_opt in
      let* status =
        Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null ("", [| path |])
      in
      match status with
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

let lookup_catala_enable_project_scan ~(notify_back : Jsonrpc2.notify_back) :
    bool option Lwt.t =
  let open Linol_lwt in
  let r, w = Lwt.task () in
  let param =
    ConfigurationParams.create
      ~items:
        [ConfigurationItem.create ~section:"catala-lsp.enableProjectScan" ()]
  in
  let* _req_id =
    notify_back#send_request (WorkspaceConfiguration param) (fun e ->
        match e with
        | Ok (`Bool x :: _) ->
          Lwt.wakeup w (Some x);
          Lwt.return_unit
        | Ok _ | Error _ ->
          Lwt.wakeup w None;
          Lwt.return_unit)
  in
  r

let try_format_document ~notify_back ~doc_content (doc_id : Doc_id.t) :
    TextEdit.t list option Lwt.t =
  let open Lwt.Syntax in
  let doc_path = (doc_id :> File.t) in
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
          let* () =
            send_notification ~type_:MessageType.Warning ~notify_back
              error_output
          in
          Lwt.return_none)
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

let lookup_clerk_toml (path : string) =
  let from_dir =
    if Sys.is_directory path then path else Filename.dirname path
  in
  let open Catala_utils in
  let find_in_parents cwd predicate =
    let home = try Sys.getenv "HOME" with Not_found -> "" in
    let rec lookup dir =
      if predicate dir then Some dir
      else if dir = home then None
      else
        let parent = Filename.dirname dir in
        if parent = dir then None else lookup parent
    in
    match lookup cwd with Some rel -> Some rel | None -> None
  in
  try
    begin
      match
        find_in_parents from_dir (fun dir -> File.(exists (dir / "clerk.toml")))
      with
      | None ->
        Log.debug (fun m -> m "no 'clerk.toml' config file found");
        None
      | Some dir -> (
        Log.debug (fun m ->
            m "found config file at: '%s'" (Filename.concat dir "clerk.toml"));
        try
          let config = Clerk_config.read File.(dir / "clerk.toml") in
          let include_dirs =
            List.map (fun p -> join_paths dir p) config.global.include_dirs
          in
          let config =
            { config with global = { config.global with include_dirs } }
          in
          Some (config, dir)
        with Message.CompilerError c ->
          Log.err (fun m ->
              let pp fmt = Message.Content.emit ~ppf:fmt c Error in
              m "error while parsing config file: %t" pp);
          None)
    end
  with _ ->
    Log.err (fun m -> m "failed to lookup config file");
    None

let list_scopes file : Shared_ast.ScopeName.t list =
  let open Shared_ast in
  let open Surface.Ast in
  let prg = Surface.Parser_driver.parse_top_level_file (Global.FileName file) in
  let rec loop acc = function
    | CodeBlock (code_block, _, _) ->
      List.fold_left
        (fun acc -> function
          | ScopeDecl sdecl ->
            let has_no_input =
              List.for_all
                (function
                  | ContextData
                      {
                        scope_decl_context_item_attribute =
                          { scope_decl_context_io_input = Input, _; _ };
                        _;
                      } ->
                    false
                  | _ -> true)
                (List.map Mark.remove sdecl.scope_decl_context)
            in
            if has_no_input then
              let scopename = Mark.remove sdecl.scope_decl_name in
              ScopeName.fresh [] (scopename, Pos.void) :: acc
            else acc
          | _ -> acc)
        acc
        (List.map Mark.remove code_block)
    | LawHeading (_, ls) -> List.fold_left loop acc ls
    | _ -> acc
  in
  List.fold_left loop [] prg.program_items

let list_testable_scopes file : Shared_ast.ScopeName.t list =
  let open Shared_ast in
  let open Surface.Ast in
  let prg = Surface.Parser_driver.parse_top_level_file (Global.FileName file) in
  let rec loop acc = function
    | CodeBlock (code_block, _, true) ->
      List.fold_left
        (fun acc -> function
          | ScopeDecl sdecl ->
            let has_no_input =
              List.for_all
                (function
                  | ContextData
                      {
                        scope_decl_context_item_attribute =
                          { scope_decl_context_io_input = Input, _; _ };
                        _;
                      } ->
                    false
                  | _ -> true)
                (List.map Mark.remove sdecl.scope_decl_context)
            in
            let has_function_input_or_output =
              List.exists
                (function
                  | ContextData
                      {
                        scope_decl_context_item_typ = Func _, _;
                        scope_decl_context_item_attribute =
                          ( { scope_decl_context_io_input = Input, _; _ }
                          | { scope_decl_context_io_output = true, _; _ } );
                        _;
                      } ->
                    true
                  | _ -> false)
                (List.map Mark.remove sdecl.scope_decl_context)
            in
            if (not has_no_input) && not has_function_input_or_output then
              let scopename = Mark.remove sdecl.scope_decl_name in
              ScopeName.fresh [] (scopename, Pos.void) :: acc
            else acc
          | _ -> acc)
        acc
        (List.map Mark.remove code_block)
    | LawHeading (_, ls) -> List.fold_left loop acc ls
    | _ -> acc
  in
  List.fold_left loop [] prg.program_items

let get_timestamp ?(no_brackets = false) () =
  let open Ptime in
  let now = Ptime_clock.now () in
  let _, ((hh, mm, ss), (_tz_offset_s : tz_offset_s)) =
    to_date_time ~tz_offset_s:0 now
  in
  let cs =
    Int64.div (Ptime.(frac_s now |> Span.to_d_ps) |> snd) 1_000_000_0000L
  in
  if no_brackets then Format.asprintf "%02d:%02d:%02d.%02Ld" hh mm ss cs
  else Format.asprintf "[%02d:%02d:%02d.%02Ld]" hh mm ss cs
