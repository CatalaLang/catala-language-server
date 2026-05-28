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
(****************************************************************)

open Server_types
open Server_state
open Catala_utils
open Utils
open Shared_ast

let lookup_completions (doc : document_state) ~doc_content pos diagnostics =
  Doc_completion.lookup_completions
    (doc : document_state)
    ~doc_content pos diagnostics

(* This is use for debugging: call this function in all_diagnostics to underline
   all processed symbols in LSP. Useful to determine which expression wasn't
   properly handled. *)
let all_symbols_as_warning (doc_id : Doc_id.t) processing_result =
  let diags : (Doc_id.doc_id * Diagnostic.t list) list =
    match processing_result with
    | Skipped | Faulty _ -> []
    | Partial (_, { jump_table = (lazy { variables; lookup_table }); _ })
    | Valid { jump_table = (lazy { variables; lookup_table }); _ } ->
      (* Displays the full position map in logs *)
      (* Log.info (fun m -> m "%a@." Jump_table.PMap.pp variables); *)
      (* Generates warning diagnostic for each symbol *)
      [
        ( doc_id,
          Jump_table.LTable.bindings lookup_table
          |> List.map snd
          |> List.concat_map
               (fun { Jump_table.declaration; definitions; usages; types } ->
                 let build r =
                   Diagnostic.diag_r Warning (range_of_pos r) (`String "abc")
                 in
                 let declaration = Option.map (fun x -> [x]) declaration in
                 [declaration; definitions; usages; types]
                 |> List.filter_map (function
                   | None -> None
                   | Some r -> (
                     List.filter
                       (fun r ->
                         Catala_utils.Pos.get_file r = (doc_id :> File.t))
                       r
                     |> function [] -> None | r -> Some r))
                 |> List.concat
                 |> List.map build) );
      ]
      @ [
          ( doc_id,
            Jump_table.PMap.fold_on_file doc_id
              (fun r v acc ->
                let msg =
                  Format.asprintf "%a : @[<h>%a@]"
                    Format.(
                      pp_print_list ~pp_sep:pp_print_space Jump_table.pp_var)
                    (Jump_table.PMap.DS.elements v)
                    (Catala_utils.Pos.format_loc_text ())
                    r
                in
                Diagnostic.diag_r Warning (range_of_pos r) (`String msg) :: acc)
              variables [] );
        ]
  in
  let m : diagnostic Range.Map.t Doc_id.Map.t =
    List.fold_left
      (fun m (doc_id, diags) ->
        let diags =
          List.map
            (fun (diag : Diagnostic.t) ->
              ( diag.range,
                ({ range = diag.range; lsp_error = None; diag } : diagnostic) ))
            diags
        in
        Doc_id.Map.update doc_id
          (function
            | None -> Some (Range.Map.of_list diags)
            | Some s -> Some (Range.Map.add_seq (List.to_seq diags) s))
          m)
      Doc_id.Map.empty diags
  in
  m

let of_position pos = Catala_utils.Pos.get_file pos, Utils.range_of_pos pos

let generic_lookup
    ?doc_id
    (document : document_state)
    (p : Linol_lwt.Position.t)
    (f : Jump_table.lookup_entry -> 'a option) =
  let*? { jump_table = (lazy jt); _ } = document.last_valid_result in
  let open Option in
  let uri = Option.value doc_id ~default:document.document_id in
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

let is_output_or_context_var = function
  | { Scopelang.Ast.svar_io = { io_input = (NoInput | Reentrant), _; _ }; _ } ->
    true
  | _ -> false

(* Returns (scope_name, clerk_var_name, decl_pos) for a scope variable that is
   eligible for an exception codelens/args, or None if it should be skipped. *)
let scope_var_exception_info
    desugared
    (scope_decl : typed Scopelang.Ast.scope_decl)
    scope_var =
  let var_ty = ScopeVar.Map.find scope_var scope_decl.scope_sig in
  let*? is_sub_scope_var =
    ScopeName.Map.find_opt scope_decl.scope_decl_name
      desugared.Desugared.Ast.program_root.module_scopes
    |> function
    | None -> None
    | Some scope -> Some (ScopeVar.Map.mem scope_var scope.scope_sub_scopes)
  in
  if is_sub_scope_var then None
  else if not (is_output_or_context_var var_ty) then None
  else
    let raw_name = ScopeVar.to_string scope_var in
    let clerk_name =
      (* Scopelang encodes stateful variables as "var#state"; clerk expects
         "var.state" *)
      match String.split_on_char '#' raw_name with
      | [v; s] -> v ^ "." ^ s
      | _ -> raw_name
    in
    let decl_pos = Mark.get (ScopeVar.get_info scope_var) in
    if Pos.get_file decl_pos = "" then None
    else Some (scope_decl.scope_decl_name, clerk_name, decl_pos)

let exception_args_json
    (doc_id : Doc_id.t)
    (scope_decl_name, clerk_name, decl_pos) =
  `Assoc
    [
      "uri", `String (doc_id :> string);
      "scope", `String (ScopeName.base scope_decl_name);
      "variable", `String clerk_name;
      "declFile", `String (Pos.get_file decl_pos :> string);
      "declLine", `Int (Pos.get_start_line decl_pos);
      "declCol", `Int (Pos.get_start_column decl_pos);
      "declEndLine", `Int (Pos.get_end_line decl_pos);
      "declEndCol", `Int (Pos.get_end_column decl_pos);
    ]

let get_hover_type ?(markdown = false) f p =
  let p = Utils.(lsp_range p p |> pos_of_range (f.document_id :> File.t)) in
  let*? { jump_table = (lazy jt); prg; desugared; _ } = f.last_valid_result in
  let*? range, lookup_s = Jump_table.lookup_type jt p in
  let kind =
    try Jump_table.Ord_lookup.max_elt lookup_s with _ -> assert false
  in
  if markdown then
    let md = Type_printing.typ_to_markdown prg f.locale kind in
    let definition_tree_link =
      let open Jump_table in
      let*? l = PMap.lookup p jt.variables in
      let id =
        PMap.DS.elements l
        |> List.find_map (function
          | Declaration { id = Some id; _ } -> Some id
          | _ -> None)
      in
      let*? id = id in
      let*? scope_decl, scope_var =
        ScopeName.Map.bindings prg.program_scopes
        |> List.find_map (fun (_, (scope_decl, _)) ->
            ScopeVar.Map.bindings scope_decl.Scopelang.Ast.scope_sig
            |> List.find_map (fun (scope_var, _) ->
                if ScopeVar.id scope_var = id then Some (scope_decl, scope_var)
                else None))
      in
      let*? exceptions_args =
        scope_var_exception_info desugared scope_decl scope_var
      in
      let main_doc_id =
        let doc_id = f.document_id in
        if Projects.is_an_included_file doc_id f.project then
          List.hd (Projects.including_files doc_id f.project)
        else doc_id
      in
      let args_json =
        `List [exception_args_json main_doc_id exceptions_args]
        |> Yojson.Safe.to_string
        |> Uri.pct_encode ~component:`Query_key
      in
      let show_def_tree_s = function
        | `En -> "Show definition tree"
        | `Fr -> "Afficher l'arbre de définitions"
        | `Pl -> assert false
      in
      Some
        (Printf.sprintf "\n\n[%s](command:catala.showExceptions?%s)"
           (show_def_tree_s f.locale) args_json)
    in
    let value =
      md.Linol_lwt.MarkupContent.value
      ^ Option.value ~default:"" definition_tree_link
    in
    Some
      (Linol_lwt.Hover.create ~range
         ~contents:
           (`MarkupContent
              (Linol_lwt.MarkupContent.create
                 ~kind:Linol_lwt.MarkupKind.Markdown ~value))
         ())
  else
    let md = Type_printing.typ_to_raw_string prg f.locale kind in
    Some (Linol_lwt.Hover.create ~range ~contents:(`MarkedString md) ())

let lookup_type_declaration f p =
  let p = Utils.(lsp_range p p |> pos_of_range (f.document_id :> File.t)) in
  let*? { jump_table = (lazy jt); _ } = f.last_valid_result in
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
  | Scope { scope_decl_name; _ } ->
    Some (of_position @@ Mark.get (ScopeName.get_info scope_decl_name))
  | Expr _ | Type _ -> None

let lookup_occurences f p : Range.Set.t Doc_id.Map.t option =
  let p = Utils.(lsp_range p p |> pos_of_range (f.document_id :> File.t)) in
  let*? { jump_table = (lazy jt); _ } = f.last_valid_result in
  let occurences = Jump_table.lookup jt p in
  let m =
    List.fold_left
      (fun m { Jump_table.declaration; definitions; usages; types } ->
        let all_ranges =
          Option.to_list declaration
          @ (Option.to_list definitions |> List.flatten)
          @ (Option.to_list usages |> List.flatten)
          @ (Option.to_list types |> List.flatten)
          |> List.map (fun p -> Doc_id.of_catala_pos p, Utils.range_of_pos p)
        in
        List.fold_left
          (fun m (doc_id, r) ->
            Doc_id.Map.update doc_id
              (function
                | None -> Some (Range.Set.singleton r)
                | Some s -> Some (Range.Set.add r s))
              m)
          m all_ranges)
      Doc_id.Map.empty occurences
  in
  if Doc_id.Map.is_empty m then None else Some m

let lookup_document_symbols file =
  let*?! { jump_table = (lazy jt); _ } = file.last_valid_result, [] in
  Jump_table.PMap.fold_on_file file.document_id
    (fun p vl acc ->
      Jump_table.PMap.DS.fold
        (fun v acc ->
          match Jump_table.var_to_symbol p v with
          | None -> acc
          | Some v -> v :: acc)
        vl acc)
    jt.variables []

let lookup_lenses file =
  let*? { jump_table = (lazy jt); _ } = file.last_valid_result in
  (* we consider only the including file's document id if the document is
     included. *)
  let main_doc_id =
    let doc_id = file.document_id in
    if Projects.is_an_included_file doc_id file.project then
      List.hd (Projects.including_files doc_id file.project)
    else doc_id
  in
  let mk_no_input_lens scope range =
    let arguments =
      [
        `Assoc
          [
            "uri", `String (main_doc_id :> string);
            "scope", `String (ScopeName.base scope);
          ];
      ]
    in
    let open Linol_lwt in
    let run_command =
      Command.create ~arguments ~command:"catala.runScope" ~title:"▶ Run" ()
    in
    let debug_command =
      Command.create ~arguments ~command:"catala.debugScope" ~title:"🛠 Debug" ()
    in
    [
      CodeLens.create ~command:run_command ~range ();
      CodeLens.create ~command:debug_command ~range ();
    ]
  in
  let mk_input_lens scope range =
    let arguments =
      [
        `Assoc
          [
            "uri", `String (main_doc_id :> string);
            "scope", `String (ScopeName.base scope);
          ];
      ]
    in
    let open Linol_lwt in
    let run_command =
      Command.create ~arguments ~command:"catala.openInputEditor"
        ~title:"▶ Run with..." ()
    in
    [CodeLens.create ~command:run_command ~range ()]
  in
  let scope_lenses =
    Jump_table.PMap.fold_on_file file.document_id
      (fun p vl acc ->
        Jump_table.PMap.DS.fold
          (fun var acc ->
            match var with
            | Scope_decl { scope_decl; _ } ->
              let { Scopelang.Ast.scope_decl_name; scope_sig; _ } =
                scope_decl
              in
              if
                ScopeVar.Map.for_all
                  (fun _scope_var v -> is_output_or_context_var v)
                  scope_sig
              then mk_no_input_lens scope_decl_name (range_of_pos p) @ acc
              else mk_input_lens scope_decl_name (range_of_pos p) @ acc
            | _ -> acc)
          vl acc)
      jt.variables []
  in
  Some scope_lenses

let exceptions_at (file : document_state) (p : Linol_lwt.Position.t) :
    Yojson.Safe.t option =
  let*? { jump_table = (lazy jt); desugared; prg; _ } =
    file.last_valid_result
  in
  let doc_id = file.document_id in
  let p = Utils.(lsp_range p p |> pos_of_range (file.document_id :> File.t)) in
  let open Jump_table in
  let*? l : PMap.DS.t = PMap.lookup p jt.variables in
  let x =
    PMap.DS.elements l
    |> List.filter_map (function
      | Usage ({ id = Some _; _ } as jump)
      | Declaration ({ id = Some _; _ } as jump)
      | Definition ({ id = Some _; _ } as jump) ->
        Some jump
      | _ -> None)
  in
  match x with
  | [] | _ :: _ :: _ -> None
  | [{ hash = _; name = _; typ = _; id }] ->
    let*? id = id in
    let prg_scopes :
        (ScopeName.t * typed Scopelang.Ast.scope_decl Mark.pos) list =
      prg.program_scopes |> ScopeName.Map.bindings
    in
    let*? scope_decl, scope_var =
      List.find_map
        (fun (_scope_name, (({ Scopelang.Ast.scope_sig; _ } as scope_decl), _))
           ->
          let all_vars = ScopeVar.Map.bindings scope_sig in
          List.find_map
            (fun (scope_var, _) ->
              if ScopeVar.id scope_var = id then Some (scope_decl, scope_var)
              else None)
            all_vars)
        prg_scopes
    in
    let*? exceptions_args =
      scope_var_exception_info desugared scope_decl scope_var
    in
    let main_doc_id =
      if Projects.is_an_included_file doc_id file.project then
        List.hd (Projects.including_files doc_id file.project)
      else doc_id
    in
    Some (exception_args_json main_doc_id exceptions_args)
