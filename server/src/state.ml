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
open Clerk_lib

module RangeMap = Stdlib.Map.Make (struct
  type t = Range.t

  let compare = compare
end)

module UriMap = Map.Make (String)

type file = {
  uri : string;
  locale : Catala_utils.Global.backend_lang;
  scopelang_prg : Shared_ast.typed Scopelang.Ast.program option;
  jump_table : Jump.t option;
  errors : (Range.t * Catala_utils.Message.lsp_error) RangeMap.t UriMap.t;
}

type t = file

let err_severity = function
  | Catala_utils.Message.Lexing | Parsing | Typing | Generic ->
    DiagnosticSeverity.Error
  | Warning -> Warning

let pp_range fmt { Range.start; end_ } =
  let open Format in
  let pp_pos fmt { Position.line; character } =
    fprintf fmt "l:%d, c:%d" line character
  in
  fprintf fmt "start:(%a), end:(%a)" pp_pos start pp_pos end_

let create ?prog uri =
  {
    uri;
    locale = Catala_utils.Cli.file_lang uri;
    errors = UriMap.empty;
    scopelang_prg = prog;
    jump_table = None;
  }

let add_suggestions file uri range err =
  let errors =
    UriMap.update uri
      (function
        | None -> Some (RangeMap.singleton range (range, err))
        | Some rmap -> Some (RangeMap.add range (range, err) rmap))
      file.errors
  in
  { file with errors }

let lookup_suggestions file range =
  Option.bind (UriMap.find_opt file.uri file.errors)
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
  match file.jump_table with
  | None -> []
  | Some { variables; lookup_table } ->
    (* Displays the full position map in logs *)
    (* Log.info (fun m -> m "%a@." Jump.PMap.pp variables); *)
    (* Generates warning diagnostic for each symbol *)
    [
      ( file.uri,
        Jump.LTable.bindings lookup_table
        |> List.map snd
        |> List.concat_map
             (fun { Jump.declaration; definitions; usages; types } ->
               let build r = diag_r Warning (range_of_pos r) (`String "abc") in
               let declaration = Option.map (fun x -> [x]) declaration in
               [declaration; definitions; usages; types]
               |> List.filter_map (function
                    | None -> None
                    | Some r -> (
                      List.filter
                        (fun r -> Catala_utils.Pos.get_file r = file.uri)
                        r
                      |> function [] -> None | r -> Some r))
               |> List.concat
               |> List.map build) );
    ]
    @ [
        ( file.uri,
          Jump.PMap.fold_on_file file.uri
            (fun r v acc ->
              let msg =
                Format.asprintf "%a : @[<h>%a@]"
                  Format.(pp_print_list ~pp_sep:pp_print_space Jump.pp_var)
                  (Jump.PMap.DS.elements v) Catala_utils.Pos.format_loc_text r
              in
              diag_r Warning (range_of_pos r) (`String msg) :: acc)
            variables [] );
      ]

let all_diagnostics file =
  let open Catala_utils.Message in
  let errs = UriMap.bindings file.errors in
  List.map
    (fun (uri, rmap) ->
      ( uri,
        List.map
          (fun (range, (_range, err)) ->
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
          (RangeMap.bindings rmap) ))
    errs

let of_position pos = Catala_utils.Pos.get_file pos, Utils.range_of_pos pos

let generic_lookup ?uri { uri = file_uri; jump_table; _ } (p : Position.t) f =
  let open Option in
  let uri = Option.value uri ~default:file_uri in
  let p = Utils.(lsp_range p p |> pos_of_range uri) in
  let open Jump in
  let ( let* ) = Option.bind in
  let* jump_table = jump_table in
  let l = lookup jump_table p in
  List.filter_map f l |> List.concat |> function [] -> None | l -> Some l

let lookup_declaration ?uri f p =
  generic_lookup ?uri f p (fun { declaration; _ } ->
      Option.map (fun x -> [x]) declaration)
  |> Option.map (List.map of_position)

let lookup_def ?uri f p =
  generic_lookup ?uri f p (fun { definitions; _ } -> definitions)
  |> Option.map (List.map of_position)
  |> function
  | Some l -> Some l
  | None ->
    (* If no definition is found, we default to referencing the declaration. In
       most case, it is relevant hence a better UX. *)
    lookup_declaration ?uri f p

let lookup_usages ?uri f p =
  generic_lookup ?uri f p (fun { usages; _ } -> usages)
  |> Option.map (List.map of_position)

let lookup_type f p =
  let p = Utils.(lsp_range p p |> pos_of_range f.uri) in
  let ( let* ) = Option.bind in
  let* jt = f.jump_table in
  let prg = f.scopelang_prg in
  let* r, lookup_s = Jump.lookup_type jt p in
  let kind = try Jump.Ord_lookup.max_elt lookup_s with _ -> assert false in
  let md = Type_printing.typ_to_markdown ?prg f.locale kind in
  Some (r, md)

let lookup_type_declaration f p =
  let p = Utils.(lsp_range p p |> pos_of_range f.uri) in
  let ( let* ) = Option.bind in
  let* jt = f.jump_table in
  let* _r, lookup_s = Jump.lookup_type jt p in
  let open Shared_ast in
  let elt = try Jump.Ord_lookup.max_elt lookup_s with _ -> assert false in
  match (elt : Jump.type_lookup) with
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
  let variables =
    Option.bind file.jump_table @@ fun tbl -> Some tbl.variables
  in
  match variables with
  | None -> []
  | Some variables ->
    Jump.PMap.fold_on_file file.uri
      (fun p vl acc ->
        Jump.PMap.DS.fold
          (fun v acc ->
            match Jump.var_to_symbol p v with None -> acc | Some v -> v :: acc)
          vl acc)
      variables []

let load_module_interfaces config_dir includes program =
  (* Recurse into program modules, looking up files in [using] and loading
     them *)
  let open Catala_utils in
  let open Shared_ast in
  let err_req_pos chain =
    List.map (fun mpos -> "Module required from", mpos) chain
  in
  let includes =
    List.map File.Tree.build includes
    |> List.fold_left File.Tree.union File.Tree.empty
  in
  let find_module req_chain (mname, mpos) =
    let required_from_file = Pos.get_file mpos in
    let includes =
      File.Tree.union includes
        (File.Tree.build (File.dirname required_from_file))
    in
    let extensions =
      [".catala_fr", "fr"; ".catala_en", "en"; ".catala_pl", "pl"]
    in
    match
      List.filter_map
        (fun (ext, _) -> File.Tree.lookup includes (mname ^ ext))
        extensions
    with
    | [] ->
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
  let rec aux req_chain seen uses :
      (ModuleName.t * Surface.Ast.module_content * ModuleName.t Ident.Map.t)
      option
      File.Map.t
      * ModuleName.t Ident.Map.t =
    List.fold_left
      (fun (seen, use_map) use ->
        let f = find_module req_chain use.Surface.Ast.mod_use_name in
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
          (* Some file paths are absolute, we normalize them wrt to the config
             directory *)
          let f_path = Utils.join_paths config_dir f in
          let module_content =
            Surface.Parser_driver.load_interface (Global.FileName f_path)
          in
          let modname =
            ModuleName.fresh module_content.module_modname.module_name
          in
          let seen = File.Map.add f None seen in
          let seen, sub_use_map =
            aux
              (Mark.get use.Surface.Ast.mod_use_name :: req_chain)
              seen module_content.Surface.Ast.module_submodules
          in
          ( File.Map.add f (Some (modname, module_content, sub_use_map)) seen,
            Ident.Map.add
              (Mark.remove use.Surface.Ast.mod_use_alias)
              modname use_map ))
      (seen, Ident.Map.empty) uses
  in
  let seen =
    match program.Surface.Ast.program_module with
    | Some m ->
      let file = Pos.get_file (Mark.get m.module_name) in
      File.Map.singleton file None
    | None -> File.Map.empty
  in
  let file_module_map, root_uses =
    aux [] seen program.Surface.Ast.program_used_modules
  in
  let modules =
    File.Map.fold
      (fun _ info acc ->
        match info with
        | None -> acc
        | Some (mname, intf, use_map) ->
          ModuleName.Map.add mname (intf, use_map) acc)
      file_module_map ModuleName.Map.empty
  in
  root_uses, modules

let process_document
    ?previous_file
    ?contents
    (project_state : Discovery.project)
    (uri : string) : t =
  let open Catala_utils in
  Log.info (fun m -> m "processing document '%s'" uri);
  let uri = Uri.pct_decode uri in
  let input_src =
    match contents with
    | None -> Global.FileName uri
    | Some c -> Contents (c, uri)
  in
  let input_src, resolve_included_file =
    let project_file_opt =
      Discovery.Scan_map.find_opt uri project_state.project_files
    in
    match project_file_opt with
    | None -> (* should not happen *) input_src, None
    | Some { Discovery.file = _; including_files; _ } ->
      let module S = Discovery.Project_files in
      if S.is_empty including_files then input_src, None
      else begin
        Log.info (fun m ->
            m "found document included in files: %a" Utils.pp_string_list
              (S.elements including_files
              |> List.map (fun { Clerk_scan.file_name; _ } -> file_name)));
        let including_file = S.choose including_files in
        if S.cardinal including_files > 1 then
          (* TODO: also handle multiple file processing *)
          Log.info (fun m -> m "found multiple document inclusion");
        Log.info (fun m ->
            m "found document inclusion in %s - processing this one instead"
              including_file.file_name);
        let resolve_included_file path =
          if File.equal path uri then input_src else Global.FileName path
        in
        FileName including_file.file_name, Some resolve_included_file
      end
  in
  let _ = Catala_utils.Global.enforce_options ~input_src () in
  let l = ref [] in
  let on_error e = l := e :: !l in
  let () = Catala_utils.Message.register_lsp_error_notifier on_error in
  let errors, prog, jump_table =
    try
      (* Resets the lexing context to a fresh one *)
      Surface.Lexer_common.context := Law;
      let prg =
        Surface.Parser_driver.parse_top_level_file ?resolve_included_file
          input_src
      in
      let (prg as surface) = prg in
      let open Catala_utils in
      let ctx, modules_contents =
        let mod_uses, modules =
          load_module_interfaces project_state.clerk_root_dir
            project_state.clerk_config.global.include_dirs prg
        in
        let ctx =
          Desugared.Name_resolution.form_context (prg, mod_uses) modules
        in
        let modules_content : Surface.Ast.module_content Uid.Module.Map.t =
          Uid.Module.Map.map (fun elt -> fst elt) modules
        in
        ctx, modules_content
      in
      let prg =
        Desugared.From_surface.translate_program ctx modules_contents prg
      in
      let prg = Desugared.Disambiguate.program prg in
      let () = Desugared.Linting.lint_program prg in
      let exceptions_graphs =
        Scopelang.From_desugared.build_exceptions_graph prg
      in
      match surface.Surface.Ast.program_module with
      | Some { module_external = true; _ } ->
        (* If the module is external, we skip it as the translation from
           desugared would trigger an error *)
        Log.debug (fun m -> m "skipping external module interface");
        [], None, None
      | _ ->
        let prg =
          Scopelang.From_desugared.translate_program prg exceptions_graphs
          |> Scopelang.Ast.type_program
        in
        let _type_ordering =
          Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_structs
            prg.program_ctx.ctx_enums
        in
        let _ =
          let type_ordering =
            Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_structs
              prg.program_ctx.ctx_enums
          in
          let prg = Scopelang.Ast.type_program prg in
          let prg = Dcalc.From_scopelang.translate_program prg in
          let prg = Shared_ast.Typing.program ~internal_check:true prg in
          prg, type_ordering
        in
        let jump_table =
          Jump.populate input_src ctx modules_contents surface prg
        in
        !l, Some prg, Some jump_table
    with e ->
      let errors =
        match e with
        | Catala_utils.Message.CompilerError er -> [er]
        | Catala_utils.Message.CompilerErrors er_l -> er_l
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
      List.rev !l, None, None
  in
  let file = create ?prog uri in
  let file =
    match previous_file, jump_table with
    | Some { jump_table = Some jump_table; scopelang_prg; _ }, None ->
      { file with jump_table = Some jump_table; scopelang_prg }
    | _, Some jump_table -> { file with jump_table = Some jump_table }
    | (None | Some _), None -> file
  in
  List.fold_left
    (fun f (err : Catala_utils.Message.lsp_error) ->
      let dummy_range =
        Range.create
          ~start:{ line = 0; character = 0 }
          ~end_:{ line = 0; character = 0 }
      in
      let uri, range =
        match err.pos, err.kind with
        | None, _ -> uri, dummy_range
        | Some pos, Lexing ->
          Catala_utils.Pos.get_file pos, unclosed_range_of_pos pos
        | Some pos, _ -> Catala_utils.Pos.get_file pos, range_of_pos pos
      in
      add_suggestions f uri range err)
    file errors
