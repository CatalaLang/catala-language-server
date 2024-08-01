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

module RangeMap = Map.Make (struct
  type t = Range.t

  let compare
      ({ Range.start; end_ } as x)
      ({ Range.start = start'; end_ = end_' } as y) =
    if (start' >= start && end_' <= end_) || (start >= start' && end_ <= end_')
    then 0
    else compare x y
end)

type file = {
  uri : string;
  scopelang_prg : Shared_ast.typed Scopelang.Ast.program option;
  jump_table : Jump.t option;
  errors : (Range.t * Catala_utils.Message.lsp_error) RangeMap.t;
}

type t = file

let err_severity = function
  | Catala_utils.Message.Lexing | Parsing | Typing | Generic ->
    DiagnosticSeverity.Error

let pp_range fmt { Range.start; end_ } =
  let open Format in
  let pp_pos fmt { Position.line; character } =
    fprintf fmt "l:%d, c:%d" line character
  in
  fprintf fmt "start:(%a), end:(%a)" pp_pos start pp_pos end_

let create ?prog uri =
  { uri; errors = RangeMap.empty; scopelang_prg = prog; jump_table = None }

let add_suggestions file range err =
  let errors = RangeMap.add range (range, err) file.errors in
  { file with errors }

let lookup_suggestions file range =
  RangeMap.find_opt range file.errors
  |> function
  | None -> None
  | Some (range, err) -> (
    match err.suggestion with
    | None | Some [] -> None
    | Some suggs -> Some (range, suggs))

let lookup_suggestions_by_pos file pos =
  let range = { Range.start = pos; end_ = pos } in
  lookup_suggestions file range

let all_diagnostics file =
  let open Catala_utils.Message in
  let errs = RangeMap.bindings file.errors in
  List.map
    (fun (range, (_range, err)) ->
      let severity = err_severity err.kind in
      let message = Format.asprintf "%t" err.message in
      diag_r severity range message)
    errs

let to_position pos = Catala_utils.Pos.get_file pos, Utils.range_of_pos pos

let generic_lookup ?uri { uri = file_uri; jump_table; _ } (p : Position.t) f =
  let open Option in
  let uri = Option.value uri ~default:file_uri in
  let p = Utils.(lsp_range p p |> pos_of_range uri) in
  let open Jump in
  let ( let* ) = Option.bind in
  let* jump_table = jump_table in
  let* v = map f (lookup jump_table p) |> join in
  Some v

let lookup_def ?uri f p =
  generic_lookup ?uri f p (fun { definitions; _ } -> definitions)
  |> Option.map (List.map to_position)

let lookup_declaration ?uri f p =
  generic_lookup ?uri f p (fun { declaration; _ } -> declaration)
  |> Option.map to_position

let lookup_usages ?uri f p =
  generic_lookup ?uri f p (fun { usages; _ } -> usages)
  |> Option.map (List.map to_position)

let lookup_type f p =
  let p = Utils.(lsp_range p p |> pos_of_range f.uri) in
  let ( let* ) = Option.bind in
  let* jt = f.jump_table in
  let* typ = Jump.lookup_type jt p in
  let* prg = f.scopelang_prg in
  let typ_s = Format.asprintf "%a" (Shared_ast.Print.typ prg.program_ctx) typ in
  Some typ_s

let lookup_document_symbols file =
  let variables =
    Option.bind file.jump_table @@ fun tbl -> Some tbl.variables
  in
  match variables with
  | None -> []
  | Some variables ->
    List.filter_map
      (fun (p, v) ->
        if file.uri = Catala_utils.Pos.get_file p then
          Some (Jump.var_to_symbol p v)
        else None)
      (Jump.PMap.bindings variables)

let lookup_project_symbols all_files =
  let all_variables =
    let all_tbls =
      List.filter_map
        (fun f -> Option.bind f.jump_table @@ fun tbl -> Some tbl.variables)
        all_files
    in
    List.fold_left
      (Jump.PMap.union (fun _ l _ -> Some l))
      Jump.PMap.empty all_tbls
  in
  List.map
    (fun (p, v) -> Jump.var_to_symbol p v)
    (Jump.PMap.bindings all_variables)

let lookup_clerk_toml (path : string) =
  let from_dir = Filename.dirname path in
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
      | Some dir ->
        Log.debug (fun m ->
            m "found config file at: '%s'" (Filename.concat dir "clerk.toml"));
        Some (Clerk_config.read File.(dir / "clerk.toml"))
    end
  with exn ->
    Log.err (fun m ->
        m "failed to lookup config file: %s" (Printexc.to_string exn));
    None

let load_module_interfaces includes program =
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
      (ModuleName.t * Surface.Ast.interface * ModuleName.t Ident.Map.t) option
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
          let intf = Surface.Parser_driver.load_interface (Global.FileName f) in
          let modname = ModuleName.fresh intf.intf_modname.module_name in
          let seen = File.Map.add f None seen in
          let seen, sub_use_map =
            aux
              (Mark.get use.Surface.Ast.mod_use_name :: req_chain)
              seen intf.Surface.Ast.intf_submodules
          in
          ( File.Map.add f (Some (modname, intf, sub_use_map)) seen,
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

let process_document ?contents (uri : string) : t =
  Log.debug (fun m -> m "Processing document '%s'" uri);
  let uri = Uri.path (Uri.of_string uri) in
  let input_src =
    let open Catala_utils.Global in
    match contents with None -> FileName uri | Some c -> Contents (c, uri)
  in
  let _ = Catala_utils.Global.enforce_options ~input_src () in
  let config_opt = lookup_clerk_toml uri in
  let l = ref [] in
  let on_error e = l := e :: !l in
  let () = Catala_utils.Message.register_lsp_error_notifier on_error in
  let parsing_errs, prg_opt =
    try
      (* Resets the lexing context to a fresh one *)
      Surface.Lexer_common.context := Law;
      let prg = Surface.Parser_driver.parse_top_level_file input_src in
      let prg = Surface.Fill_positions.fill_pos_with_legislative_info prg in
      let open Catala_utils in
      let ctx =
        let mod_uses, modules =
          match config_opt with
          | None -> String.Map.empty, Uid.Module.Map.empty
          | Some config -> load_module_interfaces config.include_dirs prg
        in
        Desugared.Name_resolution.form_context (prg, mod_uses) modules
      in
      let prg = Desugared.From_surface.translate_program ctx prg in
      let prg = Desugared.Disambiguate.program prg in
      let exceptions_graphs =
        Scopelang.From_desugared.build_exceptions_graph prg
      in
      let prg =
        Scopelang.From_desugared.translate_program prg exceptions_graphs
      in
      let _type_ordering =
        Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_structs
          prg.program_ctx.ctx_enums
      in
      let prg = Scopelang.Ast.type_program prg in
      [], Some prg
    with e ->
      (match e with
      | Catala_utils.Message.CompilerError er ->
        Log.debug (fun m ->
            m "caught (CompilerError %t)" (fun ppf ->
                Catala_utils.Message.Content.emit ~ppf er Error))
      | _ -> ());
      Log.debug (fun m ->
          m "caught generic exception: %s (%d diagnostics to send)"
            (Printexc.to_string e) (List.length !l));
      List.rev !l, None
  in
  let file = create ?prog:prg_opt uri in
  let jump_table = Option.map Jump.populate prg_opt in
  let file = { file with jump_table } in
  List.fold_left
    (fun f (err : Catala_utils.Message.lsp_error) ->
      let dummy_range =
        Range.create
          ~start:{ line = 0; character = 0 }
          ~end_:{ line = 0; character = 0 }
      in
      let range =
        match err.pos, err.kind with
        | None, _ -> dummy_range
        | Some pos, Lexing -> unclosed_range_of_pos pos
        | Some pos, _ -> range_of_pos pos
      in
      add_suggestions f range err)
    file parsing_errs
