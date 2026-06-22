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
open Server_state
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

let protect ~on_error f =
  try Ok (f ())
  with e -> (
    let e = match e with Fun.Finally_raised e -> e | e -> e in
    match e with
    | Catala_utils.Message.CompilerError er -> Error [er]
    | Catala_utils.Message.CompilerErrors er_l -> Error er_l
    | Failed_to_load_interface mod_use ->
      let lsp_err = module_usage_error mod_use in
      on_error lsp_err;
      Error []
    | e ->
      on_error
        {
          Message.kind = Generic;
          message =
            (fun fmt ->
              Format.fprintf fmt
                "Generic exception while processing document: %s"
                (Printexc.to_string e));
          pos = None;
          suggestion = None;
        };
      Error
        [
          Format.ksprintf Message.Content.of_string "Generic exception: %s"
            (Printexc.to_string e);
        ])

let process_pending_errors ~on_error () =
  protect ~on_error Message.report_delayed_errors_if_any
  |> Result.map_error (fun _ -> ())

let lsp_errors_to_diag doc_id errors =
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

let surface_to_scopelang
    ~on_error
    ~get_errors
    ~(get_module_content : Surface.Parser_driver.module_loading)
    ?resolve_included_file
    (doc_id : Doc_id.t)
    options
    project
    k : processing_result =
  let load_module = get_module_content in
  let open Projects in
  let handle = function
    | Ok r -> r
    | Error _errs ->
      Server_state.Faulty (lsp_errors_to_diag doc_id (get_errors ()))
  in
  handle
  @@ protect ~on_error
  @@ fun () ->
  (* Resets the lexing context to a fresh one *)
  Surface.Lexer_common.context := Law;
  let surface =
    Surface.Parser_driver.parse_top_level_file ?resolve_included_file
      options.Global.input_src
  in
  match surface.Surface.Ast.program_module with
  | Some { module_external = true; _ } ->
    (* If the module is external, we skip it as the translation from desugared
       would trigger an error *)
    Log.debug (fun m -> m "skipping external module interface");
    Server_state.Skipped
  | _ -> (
    let root_dir, clerk_config =
      match project.project_kind with
      | Clerk { clerk_root_dir; clerk_config } -> clerk_root_dir, clerk_config
      | No_clerk -> project.project_dir, Clerk_config.default_config
    in
    let includes = List.map Global.raw_file clerk_config.global.include_dirs in
    let mod_uses, modules =
      let check stdlib k =
        if File.exists stdlib then
          Surface.Parser_driver.load_modules options
            ~stdlib:(Some (Global.raw_file stdlib))
            ~load_module includes surface
        else k ()
      in
      let build_stdlib_path =
        File.(root_dir / clerk_config.global.build_dir / "libcatala")
      in
      check build_stdlib_path
      @@ fun () ->
      let cmd, args =
        ( "clerk",
          [
            "start";
            "--build-dir";
            File.(root_dir / clerk_config.global.build_dir);
            "--target-dir";
            File.(root_dir / clerk_config.global.target_dir);
          ] )
      in
      Log.debug (fun m ->
          m "Stdlib not found - calling `clerk %s`" (String.concat " " args));
      let _ = File.process_out cmd args in
      check build_stdlib_path
      @@ fun () ->
      try
        Surface.Parser_driver.load_modules options
          ~stdlib:(Some (Global.raw_file build_stdlib_path))
          ~load_module includes surface
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
      Desugared.Name_resolution.form_context (surface, mod_uses) modules
    in
    let modules_content : Surface.Ast.module_content Uid.Module.Map.t =
      Uid.Module.Map.map (fun elt -> fst elt) modules
    in
    let ctx, modules_contents = ctx, modules_content in
    let desugared =
      Desugared.From_surface.translate_program ctx modules_contents surface
    in
    let prg = Desugared.Disambiguate.program desugared in
    let () = Desugared.Linting.lint_program prg in
    let exceptions_graphs =
      Scopelang.From_desugared.build_exceptions_graph prg
    in
    let prg =
      Scopelang.From_desugared.translate_program prg exceptions_graphs
      |> Scopelang.Ast.type_program
    in
    let jump_table =
      lazy
        (Jump_table.populate options.input_src ctx modules_contents surface prg)
    in
    let used_modules : ModuleName.t File.Map.t =
      Ident.Map.bindings mod_uses |> File.Map.of_list
    in
    match process_pending_errors ~on_error () with
    | Error () ->
      Partial
        ( lsp_errors_to_diag doc_id (get_errors ()),
          { Server_state.surface; desugared; prg; used_modules; jump_table } )
    | Ok () ->
      k { Server_state.surface; desugared; prg; used_modules; jump_table })

let process
    ~(resolve_file_content : string -> string Global.input_src)
    ~(get_module_content : Surface.Parser_driver.module_loading)
    { document_id = doc_id; buffer_state; project; project_file; _ } :
    processing_result =
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
            |> List.map (fun { Scan.file_name; _ } -> file_name)));
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
  let get_errors () = !l in
  let on_error e =
    match e with
    | { Message.kind = Generic; pos = None; _ } -> ()
    | _ -> l := e :: !l
  in
  let () = Catala_utils.Message.register_lsp_error_notifier on_error in
  (* From surface to scopelang *)
  surface_to_scopelang ~on_error ~get_errors ~get_module_content
    ?resolve_included_file doc_id options project
  @@ fun ({ prg; _ } as valid_result) ->
  let handle = function
    | Ok () -> Valid valid_result
    | Error _errs ->
      Partial (lsp_errors_to_diag doc_id (get_errors ()), valid_result)
  in
  (* Linting *)
  handle
  @@ protect ~on_error
  @@ fun () ->
  let _type_ordering =
    Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_abstract_types
      prg.program_ctx.ctx_structs prg.program_ctx.ctx_enums
  in
  let prg = Dcalc.From_scopelang.translate_program prg in
  ignore @@ Typing.program ~internal_check:true prg;
  Message.report_delayed_errors_if_any ()

let process ~resolve_file_content ~get_module_content document =
  let r = process ~resolve_file_content ~get_module_content document in
  let nb_diags diags =
    Doc_id.Map.fold
      (fun _d rm i -> Server_types.Range.Map.cardinal rm + i)
      diags 0
  in
  (match r with
  | Skipped ->
    Log.info (fun m ->
        m "Skipped validation of %a" Doc_id.format document.document_id)
  | Faulty diags ->
    Log.info (fun m ->
        m "Faulty validation of %a (%d errors)" Doc_id.format
          document.document_id (nb_diags diags))
  | Partial (diags, _) ->
    Log.info (fun m ->
        m "Partial validation of %a (%d non-fatal errors)" Doc_id.format
          document.document_id (nb_diags diags))
  | Valid _ ->
    Log.info (fun m ->
        m "Successful validation of %a" Doc_id.format document.document_id));
  r
