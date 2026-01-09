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

open Utils
open Diagnostic
open Linol_lwt
open Catala_utils
open Server_types
open Shared_ast

let err_severity = function
  | Catala_utils.Message.Lexing | Parsing | Typing | Generic | AssertFailure ->
    DiagnosticSeverity.Error
  | Warning -> Warning

let to_diagnostic range lsp_error =
  let open Catala_utils.Message in
  let severity = err_severity lsp_error.kind in
  let message =
    try Format.asprintf "%t" lsp_error.message
    with exn ->
      (* FIXME: the pretty-printer crashes due to shady UTF-8 byte access *)
      Log.warn (fun m ->
          m "exception during error message decoding: %s"
            (Printexc.to_string exn));
      "Cannot display the error description, save the file first."
  in
  diag_r severity range (`String message)

exception Failed_to_load_interface of Surface.Ast.module_use

let module_usage_error ({ mod_use_name; _ } : Surface.Ast.module_use) =
  let pos = Mark.get mod_use_name in
  let module_name = Mark.remove mod_use_name in
  {
    Message.kind = Generic;
    message =
      (fun fmt -> Format.fprintf fmt "Failed to parse module %s" module_name);
    pos = Some pos;
    suggestion = None;
  }

let process
    ~(resolve_file_content : string -> string Global.input_src)
    {
      Server_state.document_id = doc_id;
      buffer_state;
      project;
      project_file;
      _;
    } : Server_state.processing_result option * diagnostics =
  let file = (doc_id :> File.t) in
  let input_src =
    match buffer_state with
    | Saved -> Global.FileName file
    | Modified { contents } -> Contents (contents, file)
  in
  let input_src, resolve_included_file =
    let { Projects.file = _; including_files; _ } = project_file in
    let module S = Projects.ScanItemFiles in
    if S.is_empty including_files then input_src, None
    else begin
      Log.info (fun m ->
          m "found document included in files: %a" Utils.pp_string_list
            (S.elements including_files
            |> List.map (fun { Clerk_scan.file_name; _ } -> file_name)));
      let including_file = S.choose including_files in
      if S.cardinal including_files > 1 then
        Log.info (fun m -> m "found multiple document inclusion");
      Log.info (fun m ->
          m "found document inclusion in %s - processing this one instead"
            including_file.file_name);
      FileName including_file.file_name, Some resolve_file_content
    end
  in
  let options =
    Catala_utils.Global.enforce_options
      ~path_rewrite:(fun rf -> (rf :> string))
      ~input_src ()
  in
  let l = ref [] in
  let on_error e =
    match e with
    | { Message.kind = Generic; pos = None; _ } -> ()
    | _ -> l := e :: !l
  in
  let () = Catala_utils.Message.register_lsp_error_notifier on_error in
  let errors, result =
    try
      (* Resets the lexing context to a fresh one *)
      Surface.Lexer_common.context := Law;
      let prg =
        Surface.Parser_driver.parse_top_level_file ?resolve_included_file
          input_src
      in
      let (prg as surface) = prg in
      match surface.Surface.Ast.program_module with
      | Some { module_external = true; _ } ->
        (* If the module is external, we skip it as the translation from
           desugared would trigger an error *)
        Log.debug (fun m -> m "skipping external module interface");
        [], None
      | _ ->
        let root_dir, clerk_config =
          match project.project_kind with
          | Clerk { clerk_root_dir; clerk_config } ->
            clerk_root_dir, clerk_config
          | No_clerk -> project.project_dir, Clerk_config.default_config
        in
        let includes =
          List.map Global.raw_file clerk_config.global.include_dirs
        in
        let mod_uses, modules =
          let check stdlib k =
            if File.exists stdlib then
              Driver.load_modules options
                ~stdlib:(Some (Global.raw_file stdlib))
                includes prg
            else k ()
          in

          let build_stdlib_path = File.(root_dir / "_build" / "libcatala") in
          check build_stdlib_path
          @@ fun () ->
          Log.debug (fun m -> m "Stdlib not found - calling `clerk start`");
          let _ = File.process_out "clerk" ["start"] in
          check build_stdlib_path
          @@ fun () ->
          try
            Driver.load_modules options
              ~stdlib:(Some (Global.raw_file root_dir))
              includes prg
          with e ->
            on_error
              {
                Message.kind = Generic;
                message =
                  (fun fmt ->
                    Format.fprintf fmt
                      "Did not find the Catala stdlib path, please build your \
                       project");
                pos = Some (Pos.from_info (doc_id :> string) 1 1 1 1);
                suggestion = None;
              };
            raise e
        in
        let ctx =
          Desugared.Name_resolution.form_context (prg, mod_uses) modules
        in
        let modules_content : Surface.Ast.module_content Uid.Module.Map.t =
          Uid.Module.Map.map (fun elt -> fst elt) modules
        in
        let ctx, modules_contents = ctx, modules_content in
        let prg =
          Desugared.From_surface.translate_program ctx modules_contents prg
        in
        Message.report_delayed_errors_if_any ();
        let prg = Desugared.Disambiguate.program prg in
        Message.report_delayed_errors_if_any ();
        let () = Desugared.Linting.lint_program prg in
        let exceptions_graphs =
          Scopelang.From_desugared.build_exceptions_graph prg
        in
        let prg =
          Scopelang.From_desugared.translate_program prg exceptions_graphs
          |> Scopelang.Ast.type_program
        in
        let () =
          let _type_ordering =
            Scopelang.Dependency.check_type_cycles
              prg.program_ctx.ctx_abstract_types prg.program_ctx.ctx_structs
              prg.program_ctx.ctx_enums
          in
          let prg = Dcalc.From_scopelang.translate_program prg in
          ignore @@ Typing.program ~internal_check:true prg
        in
        let jump_table =
          lazy (Jump_table.populate input_src ctx modules_contents surface prg)
        in
        let used_modules : ModuleName.t File.Map.t =
          Ident.Map.bindings mod_uses |> File.Map.of_list
        in
        let result = { Server_state.prg; used_modules; jump_table } in
        Message.report_delayed_errors_if_any ();
        Log.info (fun m -> m "successful validation of %a" Doc_id.format doc_id);
        !l, Some result
    with e ->
      let errors =
        let e = match e with Fun.Finally_raised e -> e | e -> e in
        match e with
        | Catala_utils.Message.CompilerError er -> [er]
        | Catala_utils.Message.CompilerErrors er_and_bt_l ->
          List.map fst er_and_bt_l
        | Failed_to_load_interface mod_use ->
          let lsp_err = module_usage_error mod_use in
          on_error lsp_err;
          []
        | e ->
          on_error
            {
              Message.kind = Generic;
              message =
                (fun fmt ->
                  Format.fprintf fmt
                    "generic exception while processing document: %s"
                    (Printexc.to_string e));
              pos = None;
              suggestion = None;
            };
          [
            Format.ksprintf Message.Content.of_string "generic exception: %s"
              (Printexc.to_string e);
          ]
      in
      let err_len = List.length errors in
      Log.debug (fun m ->
          m "%d error(s) while processing document and %d diagnostics to send"
            err_len (List.length !l));
      List.iteri
        (fun i er ->
          let pp_err ppf = Message.Content.emit ~ppf er Error in
          if err_len > 1 then
            Log.debug (fun m -> m "error (%d/%d): %t" (succ i) err_len pp_err)
          else Log.debug (fun m -> m "error: %t" pp_err))
        errors;
      List.rev !l, None
  in
  let diagnostics =
    List.fold_left
      (fun doc_map (err : Catala_utils.Message.lsp_error) ->
        let uri, range =
          match err.pos, err.kind with
          | None, _ -> doc_id, dummy_range
          | Some pos, Lexing ->
            Doc_id.of_catala_pos pos, unclosed_range_of_pos pos
          | Some pos, _ -> Doc_id.of_catala_pos pos, range_of_pos pos
        in
        let diag =
          { range; lsp_error = Some err; diag = to_diagnostic range err }
        in
        Doc_id.Map.update uri
          (function
            | None -> Some (Range.Map.singleton range diag)
            | Some x -> Some (Range.Map.add range diag x))
          doc_map)
      (Doc_id.Map.singleton doc_id Range.Map.empty)
      errors
  in
  result, diagnostics
