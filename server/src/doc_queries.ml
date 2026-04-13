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
                    (Catala_utils.Pos.format_loc_text ()) r
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

let get_hover_type ?(markdown = false) f p =
  let p = Utils.(lsp_range p p |> pos_of_range (f.document_id :> File.t)) in
  let*? { jump_table = (lazy jt); prg; _ } = f.last_valid_result in
  let*? range, lookup_s = Jump_table.lookup_type jt p in
  let kind =
    try Jump_table.Ord_lookup.max_elt lookup_s with _ -> assert false
  in
  if markdown then
    let md = Type_printing.typ_to_markdown prg f.locale kind in
    Some (Linol_lwt.Hover.create ~range ~contents:(`MarkupContent md) ())
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
  | Scope (scope_decl_name, _) ->
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
  let main_doc_id =
    match Projects.including_files file.document_id file.project with
    | [] -> file.document_id
    | h :: _ ->
      (* We only consider the first one *)
      h
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
  let mk_exception_lens scope var_name range decl_pos =
    let arguments =
      [
        `Assoc
          [
            "uri", `String (main_doc_id :> string);
            "scope", `String (ScopeName.base scope);
            "variable", `String var_name;
            "declFile", `String (Pos.get_file decl_pos :> string);
            "declLine", `Int (Pos.get_start_line decl_pos);
            "declCol", `Int (Pos.get_start_column decl_pos);
            "declEndLine", `Int (Pos.get_end_line decl_pos);
            "declEndCol", `Int (Pos.get_end_column decl_pos);
          ];
      ]
    in
    let open Linol_lwt in
    let command =
      Command.create ~arguments ~command:"catala.showExceptions"
        ~title:"Definitions and exceptions" ()
    in
    [CodeLens.create ~command ~range ()]
  in
  let scope_lenses =
    let is_output_or_context_var = function
      | {
          Scopelang.Ast.svar_io = { io_input = (NoInput | Reentrant), _; _ };
          _;
        } ->
        true
      | _ -> false
    in
    Jump_table.PMap.fold_on_file file.document_id
      (fun p vl acc ->
        Jump_table.PMap.DS.fold
          (fun var acc ->
            match var with
            | Scope_decl { scope_decl_name; scope_sig; scope_sub_scopes; _ } ->
              let acc =
                if
                  ScopeVar.Map.for_all
                    (fun _scope_var v -> is_output_or_context_var v)
                    scope_sig
                then mk_no_input_lens scope_decl_name (range_of_pos p) @ acc
                else mk_input_lens scope_decl_name (range_of_pos p) @ acc
              in
              ScopeVar.Map.fold
                (fun scope_var var_ty acc ->
                  if ScopeVar.Set.mem scope_var scope_sub_scopes then
                    acc (* subscope call variables: skip *)
                  else if not (is_output_or_context_var var_ty) then
                    acc (* input-only variables: skip *)
                  else
                    let raw_name = ScopeVar.to_string scope_var in
                    let clerk_name =
                      (* Scopelang encodes stateful variables as "var#state";
                         clerk expects "var.state" *)
                      match String.split_on_char '#' raw_name with
                      | [var; state] -> var ^ "." ^ state
                      | _ -> raw_name
                    in
                    let pos = Mark.get (ScopeVar.get_info scope_var) in
                    if Pos.get_file pos = "" then
                      acc (* skip vars with no source position *)
                    else
                      mk_exception_lens scope_decl_name clerk_name
                        (range_of_pos pos) pos
                      @ acc)
                scope_sig acc
            | _ -> acc)
          vl acc)
      jt.variables []
  in
  Some scope_lenses
