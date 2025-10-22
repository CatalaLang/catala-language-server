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

let ( let*? ) = Option.bind
let ( let*?! ) (x, default) f = match x with None -> default | Some x -> f x

type processing_result = {
  prg : typed Scopelang.Ast.program;
  used_modules : ModuleName.t File.Map.t;
  included_files : Doc_id.Set.t;
  jump_table : Jump_table.t Lazy.t;
}

type file = {
  doc_id : Doc_id.t;
  locale : Catala_utils.Global.backend_lang;
  result : processing_result option;
  errors :
    (Range.t * Catala_utils.Message.lsp_error) Utils.RangeMap.t Doc_id.Map.t;
}

type t = file

let err_severity = function
  | Catala_utils.Message.Lexing | Parsing | Typing | Generic | AssertFailure ->
    DiagnosticSeverity.Error
  | Warning -> Warning

let pp_range fmt { Range.start; end_ } =
  let open Format in
  let pp_pos fmt { Position.line; character } =
    fprintf fmt "l:%d, c:%d" line character
  in
  fprintf fmt "start:(%a), end:(%a)" pp_pos start pp_pos end_

let create (doc_id : Doc_id.t) ?locale result =
  let locale =
    match locale with
    | Some x -> x
    | None -> Catala_utils.Cli.file_lang (doc_id :> File.t)
  in
  { doc_id; locale; result; errors = Doc_id.Map.empty }

let add_suggestions file doc_id range err =
  let errors =
    Doc_id.Map.update doc_id
      (function
        | None -> Some (RangeMap.singleton range (range, err))
        | Some rmap -> Some (RangeMap.add range (range, err) rmap))
      file.errors
  in
  { file with errors }

let lookup_suggestions file range =
  Option.bind (Doc_id.Map.find_opt file.doc_id file.errors)
  @@ fun rmap ->
  RangeMap.find_opt range rmap
  |> function
  | None -> None
  | Some (range, err) -> (
    match err.suggestion with
    | None | Some [] -> None
    | Some suggs -> Some (range, suggs))

let lookup_suggestions_by_pos file pos =
  let range = { Range.start = pos; end_ = pos } in
  lookup_suggestions file range

(* This is use for debugging: call this function in all_diagnostics to underline
   all processed symbols in LSP. Useful to determine which expression wasn't
   properly handled. *)
let all_symbols_as_warning file =
  match file.result with
  | None -> []
  | Some { jump_table = (lazy { variables; lookup_table }); _ } ->
    (* Displays the full position map in logs *)
    (* Log.info (fun m -> m "%a@." Jump_table.PMap.pp variables); *)
    (* Generates warning diagnostic for each symbol *)
    [
      ( file.doc_id,
        Jump_table.LTable.bindings lookup_table
        |> List.map snd
        |> List.concat_map
             (fun { Jump_table.declaration; definitions; usages; types } ->
               let build r = diag_r Warning (range_of_pos r) (`String "abc") in
               let declaration = Option.map (fun x -> [x]) declaration in
               [declaration; definitions; usages; types]
               |> List.filter_map (function
                    | None -> None
                    | Some r -> (
                      List.filter
                        (fun r ->
                          Catala_utils.Pos.get_file r = (file.doc_id :> File.t))
                        r
                      |> function [] -> None | r -> Some r))
               |> List.concat
               |> List.map build) );
    ]
    @ [
        ( file.doc_id,
          Jump_table.PMap.fold_on_file file.doc_id
            (fun r v acc ->
              let msg =
                Format.asprintf "%a : @[<h>%a@]"
                  Format.(
                    pp_print_list ~pp_sep:pp_print_space Jump_table.pp_var)
                  (Jump_table.PMap.DS.elements v)
                  Catala_utils.Pos.format_loc_text r
              in
              diag_r Warning (range_of_pos r) (`String msg) :: acc)
            variables [] );
      ]

let all_diagnostics file : Diagnostic.t RangeMap.t Doc_id.Map.t =
  let open Catala_utils.Message in
  Doc_id.Map.map
    (fun rmap ->
      RangeMap.mapi
        (fun range (_range, err) ->
          let severity = err_severity err.kind in
          let message =
            try Format.asprintf "%t" err.message
            with exn ->
              (* FIXME: the pretty-printer crashes due to shady UTF-8 byte
                 access *)
              Log.warn (fun m ->
                  m "exception during error message decoding: %s"
                    (Printexc.to_string exn));
              "Cannot display the error description, save the file first."
          in
          diag_r severity range (`String message))
        rmap)
    file.errors

let of_position pos = Catala_utils.Pos.get_file pos, Utils.range_of_pos pos

let generic_lookup
    ?doc_id
    file
    (p : Position.t)
    (f : Jump_table.lookup_entry -> 'a option) =
  let*? { jump_table = (lazy jt); _ } = file.result in
  let open Option in
  let uri = Option.value doc_id ~default:file.doc_id in
  let p = Utils.(lsp_range p p |> pos_of_range (uri :> File.t)) in
  let open Jump_table in
  let l = lookup jt p in
  List.filter_map f l |> List.concat |> function [] -> None | l -> Some l

let lookup_declaration ?doc_id f p =
  generic_lookup ?doc_id f p (fun { declaration; _ } ->
      Option.map (fun x -> [x]) declaration)
  |> Option.map (List.map of_position)

let lookup_def ~doc_id f p =
  generic_lookup ~doc_id f p (fun { definitions; _ } -> definitions)
  |> Option.map (List.map of_position)
  |> function
  | Some l -> Some l
  | None ->
    (* If no definition is found, we default to referencing the declaration. In
       most case, it is relevant hence a better UX. *)
    lookup_declaration ~doc_id f p

let lookup_usages ?doc_id f p =
  generic_lookup ?doc_id f p (fun { usages; _ } -> usages)
  |> Option.map (List.map of_position)

let get_hover_type ?(markdown = false) f p =
  let p = Utils.(lsp_range p p |> pos_of_range (f.doc_id :> File.t)) in
  let*? { jump_table = (lazy jt); prg; _ } = f.result in
  let*? range, lookup_s = Jump_table.lookup_type jt p in
  let kind =
    try Jump_table.Ord_lookup.max_elt lookup_s with _ -> assert false
  in
  if markdown then
    let md = Type_printing.typ_to_markdown ~prg f.locale kind in
    Some (Hover.create ~range ~contents:(`MarkupContent md) ())
  else
    let md = Type_printing.typ_to_raw_string ~prg f.locale kind in
    Some (Hover.create ~range ~contents:(`MarkedString md) ())

let lookup_type_declaration f p =
  let p = Utils.(lsp_range p p |> pos_of_range (f.doc_id :> File.t)) in
  let*? { jump_table = (lazy jt); _ } = f.result in
  let*? _r, lookup_s = Jump_table.lookup_type jt p in
  let open Shared_ast in
  let elt =
    try Jump_table.Ord_lookup.max_elt lookup_s with _ -> assert false
  in
  match (elt : Jump_table.type_lookup) with
  | Expr (TStruct s, _) | Type (TStruct s, _) ->
    let _, pos = StructName.get_info s in
    Some (of_position pos)
  | Expr (TEnum e, _) | Type (TEnum e, _) ->
    let _, pos = EnumName.get_info e in
    Some (of_position pos)
  | Module ({ Surface.Ast.module_modname = itf; _ }, _) ->
    Some (of_position @@ Mark.get itf.module_name)
  | Scope (scope_decl_name, _) ->
    Some (of_position @@ Mark.get (ScopeName.get_info scope_decl_name))
  | Expr _ | Type _ -> None

let lookup_document_symbols file =
  let*?! { jump_table = (lazy jt); _ } = file.result, [] in
  Jump_table.PMap.fold_on_file file.doc_id
    (fun p vl acc ->
      Jump_table.PMap.DS.fold
        (fun v acc ->
          match Jump_table.var_to_symbol p v with
          | None -> acc
          | Some v -> v :: acc)
        vl acc)
    jt.variables []

let lookup_lenses file =
  let*? { jump_table = (lazy jt); _ } = file.result in
  let mk_lens scope range =
    let arguments =
      [
        `Assoc
          [
            "uri", `String (file.doc_id :> string);
            "scope", `String (ScopeName.base scope);
          ];
      ]
    in
    let run_command =
      Command.create ~arguments ~command:"catala.runScope" ~title:"▶ Run scope"
        ()
    in
    let debug_command =
      Command.create ~arguments ~command:"catala.debugScope"
        ~title:"🛠 Debug scope" ()
    in
    [
      CodeLens.create ~command:run_command ~range ();
      CodeLens.create ~command:debug_command ~range ();
    ]
  in
  let scope_lenses =
    let is_input_or_context_var = function
      | {
          Scopelang.Ast.svar_io = { io_input = (NoInput | Reentrant), _; _ };
          _;
        } ->
        true
      | _ -> false
    in
    Jump_table.PMap.fold_on_file file.doc_id
      (fun p vl acc ->
        Jump_table.PMap.DS.fold
          (fun var acc ->
            match var with
            | Scope_decl { scope_decl_name; scope_sig; _ } ->
              if
                ScopeVar.Map.for_all
                  (fun _scope_var v -> is_input_or_context_var v)
                  scope_sig
              then mk_lens scope_decl_name (range_of_pos p) @ acc
              else acc
            | _ -> acc)
          vl acc)
      jt.variables []
  in
  Some scope_lenses

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

(* TODO: memoize module interfaces - invalidate them on save *)
let load_modules ~stdlib_path config_dir includes program :
    ModuleName.t Ident.Map.t
    * (Surface.Ast.module_content * ModuleName.t Ident.Map.t) ModuleName.Map.t
    * ModuleName.t File.Map.t =
  let includes =
    List.map File.Tree.build includes
    |> List.fold_left File.Tree.union File.Tree.empty
  in
  let stdlib_root_module lang =
    let lang = if Global.has_localised_stdlib lang then lang else Global.En in
    "Stdlib_" ^ Cli.language_code lang
  in
  if program.Surface.Ast.program_used_modules <> [] then
    Message.debug "Loading module interfaces...";
  (* Recurse into program modules, looking up files in [using] and loading
     them *)
  let stdlib_includes = File.Tree.build stdlib_path in
  let stdlib_use file =
    let pos = Pos.from_info file 0 0 0 0 in
    let lang = Cli.file_lang file in
    {
      Surface.Ast.mod_use_name = stdlib_root_module lang, pos;
      Surface.Ast.mod_use_alias = "Stdlib", pos;
    }
  in
  let err_req_pos chain =
    List.map (fun mpos -> "Module required from", mpos) chain
  in
  let find_module in_stdlib req_chain (mname, mpos) =
    let required_from_file = Pos.get_file mpos in
    let includes =
      if in_stdlib then stdlib_includes
      else
        File.Tree.union includes
          (File.Tree.build (File.dirname required_from_file))
    in
    match
      List.filter_map
        (fun (ext, _) -> File.Tree.lookup includes (mname ^ ext))
        [".catala_fr", "fr"; ".catala_en", "en"; ".catala_pl", "pl"]
    with
    | [] ->
      if in_stdlib then
        Message.error
          "The standard library module @{<magenta>%s@}@ could@ not@ be@ found@ \
           at@ %a"
          mname File.format
          (stdlib_path :> string)
      else
        Message.error
          ~extra_pos:(err_req_pos (mpos :: req_chain))
          "Required module not found: @{<blue>%s@}" mname
    | [f] -> f
    | ms ->
      Message.error
        ~extra_pos:(err_req_pos (mpos :: req_chain))
        "Required module @{<blue>%s@} matches multiple files:@;<1 2>%a" mname
        (Format.pp_print_list ~pp_sep:Format.pp_print_space File.format)
        ms
  in
  let rec aux is_stdlib req_chain seen uses :
      (ModuleName.t * Surface.Ast.module_content * ModuleName.t Ident.Map.t)
      option
      File.Map.t
      * ModuleName.t Ident.Map.t =
    List.fold_left
      (fun (seen, use_map) use ->
        let f = find_module is_stdlib req_chain use.Surface.Ast.mod_use_name in
        match File.Map.find_opt f seen with
        | Some (Some (modname, _, _)) ->
          ( seen,
            Ident.Map.add
              (Mark.remove use.Surface.Ast.mod_use_alias)
              modname use_map )
        | Some None ->
          Message.error
            ~extra_pos:
              (err_req_pos (Mark.get use.Surface.Ast.mod_use_name :: req_chain))
            "Circular module dependency"
        | None ->
          let f = if is_stdlib then f else Utils.join_paths config_dir f in
          let module_content =
            if Global.options.whole_program then
              Surface.Parser_driver.load_interface_and_code (Global.FileName f)
            else Surface.Parser_driver.load_interface (Global.FileName f)
          in
          let modname =
            ModuleName.fresh
              module_content.Surface.Ast.module_modname.module_name
          in
          let seen = File.Map.add f None seen in
          let seen, file_use_map =
            aux is_stdlib
              (Mark.get use.Surface.Ast.mod_use_name :: req_chain)
              seen module_content.Surface.Ast.module_submodules
          in
          ( File.Map.add f (Some (modname, module_content, file_use_map)) seen,
            Ident.Map.add
              (Mark.remove use.Surface.Ast.mod_use_alias)
              modname use_map ))
      (seen, Ident.Map.empty) uses
  in
  let file =
    match program.Surface.Ast.program_module with
    | Some m -> Pos.get_file (Mark.get m.module_name)
    | None -> List.hd program.Surface.Ast.program_source_files
  in
  let modules_map file_map =
    File.Map.fold
      (fun _ info acc ->
        match info with
        | None -> acc
        | Some (mname, intf, use_map) ->
          ModuleName.Map.add mname (intf, use_map) acc)
      file_map ModuleName.Map.empty
  in
  let stdlib_files, stdlib_uses =
    let stdlib_files, stdlib_use_map =
      aux true [Pos.from_info file 0 0 0 0] File.Map.empty [stdlib_use file]
    in
    let stdlib_modules = modules_map stdlib_files in
    let _, (_, stdlib_uses) =
      (* Uses from the stdlib are "flattened" to the parent module, but can
         still be overriden *)
      ModuleName.Map.choose stdlib_modules
    in
    ( stdlib_files,
      Ident.Map.union (fun _ _ m -> Some m) stdlib_uses stdlib_use_map )
  in
  let file_module_map, root_uses =
    aux false [] stdlib_files program.Surface.Ast.program_used_modules
  in
  let file_module_map =
    File.Map.mapi
      (fun file ->
        Option.map (fun (mname, intf, use_map) ->
            ( mname,
              intf,
              if File.Map.mem file stdlib_files then use_map
              else Ident.Map.union (fun _ _ m -> Some m) stdlib_uses use_map )))
      file_module_map
  in
  let mod_uses : ModuleName.t Ident.Map.t =
    Ident.Map.union (fun _ _ m -> Some m) stdlib_uses root_uses
  in
  let modules :
      (Surface.Ast.module_content * ModuleName.t Ident.Map.t) ModuleName.Map.t =
    modules_map file_module_map
  in
  let used_modules : ModuleName.t File.Map.t =
    Ident.Map.union (fun _ _ m -> Some m) stdlib_uses root_uses
    |> Ident.Map.bindings
    |> File.Map.of_list
  in
  mod_uses, modules, used_modules

let process_document ?contents (document : file Server_state.document_state) : t
    =
  let { Server_state.document_id = doc_id; project; project_file; _ } =
    document
  in
  let file = (doc_id :> File.t) in
  let locale = Catala_utils.Cli.file_lang file in
  let input_src =
    match contents with
    | None -> Global.FileName file
    | Some c -> Contents (c, file)
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
      let resolve_included_file path =
        if File.equal path file then input_src
        else Global.FileName (File.clean_path path)
      in
      FileName including_file.file_name, Some resolve_included_file
    end
  in
  let _ = Catala_utils.Global.enforce_options ~input_src () in
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
        let mod_uses, modules, used_modules =
          let check stdlib_path k =
            if File.exists stdlib_path then
              load_modules ~stdlib_path root_dir
                clerk_config.global.include_dirs prg
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
            load_modules ~stdlib_path:root_dir root_dir
              clerk_config.global.include_dirs prg
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
        let prg = Desugared.Disambiguate.program prg in
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
            Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_structs
              prg.program_ctx.ctx_enums
          in
          let prg = Dcalc.From_scopelang.translate_program prg in
          ignore @@ Typing.program ~internal_check:true prg
        in
        let jump_table =
          lazy (Jump_table.populate input_src ctx modules_contents surface prg)
        in
        let included_files =
          let scan_item = Clerk_scan.catala_file (doc_id :> File.t) locale in
          List.map
            (fun f ->
              let file = Mark.remove f |> File.clean_path in
              assert (not (Filename.is_relative file));
              Doc_id.of_file file)
            scan_item.included_files
          |> Doc_id.Set.of_list
        in
        let result = { prg; used_modules; included_files; jump_table } in
        Log.info (fun m -> m "successful validation of %a" Doc_id.format doc_id);
        !l, Some result
    with e ->
      let errors =
        let e = match e with Fun.Finally_raised e -> e | e -> e in
        match e with
        | Catala_utils.Message.CompilerError er -> [er]
        | Catala_utils.Message.CompilerErrors er_l -> er_l
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
  let file = create doc_id result in
  List.fold_left
    (fun f (err : Catala_utils.Message.lsp_error) ->
      let uri, range =
        match err.pos, err.kind with
        | None, _ -> doc_id, dummy_range
        | Some pos, Lexing ->
          Doc_id.of_catala_pos pos, unclosed_range_of_pos pos
        | Some pos, _ -> Doc_id.of_catala_pos pos, range_of_pos pos
      in
      add_suggestions f uri range err)
    file errors
