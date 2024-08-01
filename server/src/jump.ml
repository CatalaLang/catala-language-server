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

open Catala_utils
open Shared_ast
open Utils
open Scopelang.Ast

let hash (type a) (module M : Uid.Id with type t = a) (v : a) : int =
  Hashtbl.hash (M.get_info v)

module PMap = Map.Make (struct
  type t = Pos.t

  let compare p p' =
    (* Lattice trick for sub-range lookups *)
    if is_included p p' || is_included p' p then 0
    else
      let open Pos in
      let ( let* ) x f = if x <> 0 then x else f () in
      let* () = String.compare (get_file p) (get_file p') in
      let* () = Int.compare (get_start_line p) (get_start_line p') in
      let* () = Int.compare (get_end_line p) (get_end_line p') in
      let* () = Int.compare (get_start_column p) (get_start_column p') in
      let* () = Int.compare (get_end_column p) (get_end_column p') in
      0

  let format ppf p = Format.pp_print_string ppf (Pos.to_string_short p)
end)

type jump = { hash : int; name : string; typ : typ }

module LTable = Stdlib.Map.Make (Int)

type lookup_entry = {
  declaration : Pos.t option;
  definitions : Pos.t list option;
  usages : Pos.t list option;
}

let empty_lookup = { declaration = None; definitions = None; usages = None }

type var =
  | Topdef of jump
  | Definition of jump
  | Declaration of jump
  | Usage of jump

type t = { variables : var PMap.t; lookup_table : lookup_entry LTable.t }

let pp_var ppf =
  let open Format in
  function
  | Topdef { name; hash; _ } -> fprintf ppf "topdef: %s#%d" name hash
  | Definition { name; hash; _ } -> fprintf ppf "definition: %s#%d" name hash
  | Declaration { name; hash; _ } -> fprintf ppf "declaration: %s#%d" name hash
  | Usage { name; hash; _ } -> fprintf ppf "usage: %s#%d" name hash

let pp ppf variables =
  let open Format in
  fprintf ppf "@[<v>@[<v 2>variables:@ %a@]@]"
    (PMap.format_bindings ~pp_sep:pp_print_cut (fun ppf f v ->
         fprintf ppf "%a: %t" pp_var v f))
    variables

let pp_table ppf { declaration; definitions; usages } =
  let open Format in
  fprintf ppf "decl: %a, def: %a, usage: %a"
    (pp_print_option
       ~none:(fun fmt () -> fprintf fmt "none")
       (fun fmt _ -> fprintf fmt "some"))
    declaration
    (pp_print_option
       ~none:(fun fmt () -> fprintf fmt "none")
       (fun fmt _ -> fprintf fmt "some"))
    definitions
    (pp_print_option
       ~none:(fun fmt () -> fprintf fmt "none")
       (fun fmt _ -> fprintf fmt "some"))
    usages

let traverse_expr e m =
  let open Shared_ast in
  let open Catala_utils in
  let rec f e acc =
    match Mark.remove e with
    | ELocation (ScopelangScopeVar { name; _ }) ->
      let (Typed { pos = _; ty = typ }) = Mark.get e in
      let (scope_var : ScopeVar.t), pos = name in
      let name = ScopeVar.to_string scope_var in
      let hash = hash (module ScopeVar) scope_var in
      let var = Usage { name; hash; typ } in
      PMap.add pos var acc
    | ELocation (ToplevelVar { name; _ }) ->
      let (Typed { pos; ty = typ }) = Mark.get e in
      let (topdef_var : TopdefName.t), _ = name in
      let name = TopdefName.to_string topdef_var in
      let hash = Hashtbl.hash (TopdefName.get_info topdef_var) in
      let var = Usage { name; hash; typ } in
      PMap.add pos var acc
    | EStructAccess { name = _; e = sub_expr; field } ->
      let name = StructField.to_string field in
      let (Typed { pos = expr_pos; ty = typ }) = Mark.get e in
      let (Typed { pos = sub_expr_pos; ty = _ }) = Mark.get sub_expr in
      let hash = hash (module StructField) field in
      let var = Usage { name; hash; typ } in
      let pos =
        let open Pos in
        (* Hack to extract the field's position as StructField's mark points to
           the declaration, i.e., compute the disjoint position of expr_pos (the
           full expression) deprived of sub_expr_pos (structure's name) *)
        from_info (get_file expr_pos) (get_start_line expr_pos)
          (get_end_column sub_expr_pos + 1)
          (get_end_line expr_pos) (get_end_column expr_pos)
      in
      let acc = PMap.add pos var acc in
      f sub_expr acc
    | _ -> Expr.shallow_fold f e acc
  in
  Expr.shallow_fold f e m

let traverse_scope_decl (rule : typed rule) m : var PMap.t =
  match rule with
  | ScopeVarDefinition { var; typ; io = _; e }
  | SubScopeVarDefinition { var; typ; var_within_origin_scope = _; e } ->
    let var, pos_l = var in
    let _pos = snd (ScopeVar.get_info var) in
    let name = ScopeVar.to_string var in
    let hash = hash (module ScopeVar) var in
    let var = Definition { name; hash; typ } in
    let m = List.fold_right (fun p -> PMap.add p var) pos_l m in
    traverse_expr e m
  | Assertion e -> traverse_expr e m

let traverse_scope_sig (type a) (scope : a scope_decl) m : var PMap.t =
  ScopeVar.Map.fold
    (fun scope_var var_ty m ->
      let name = ScopeVar.to_string scope_var in
      let pos = snd (ScopeVar.get_info scope_var) in
      let hash = hash (module ScopeVar) scope_var in
      let var = Declaration { name; hash; typ = var_ty.svar_out_ty } in
      let var_typ_name = "typ:" ^ name in
      let typ =
        Declaration
          {
            name = var_typ_name;
            hash = Hashtbl.hash (var_typ_name, Mark.get var_ty.svar_out_ty);
            typ = var_ty.svar_out_ty;
          }
      in
      let m = PMap.add pos var m in
      PMap.add (Mark.get var_ty.svar_out_ty) typ m)
    scope.scope_sig m

let traverse_scope (scope : typed scope_decl) m : var PMap.t =
  let m = traverse_scope_sig scope m in
  List.fold_right traverse_scope_decl scope.scope_decl_rules m

let traverse_topdef (topdef : TopdefName.t) ((e, typ) : typed expr * typ) m :
    var PMap.t =
  let name = TopdefName.to_string topdef in
  let topdef_pos = snd (TopdefName.get_info topdef) in
  let hash = Hashtbl.hash (TopdefName.get_info topdef) in
  let topdef = Topdef { name; hash; typ } in
  let m = PMap.add topdef_pos topdef m in
  traverse_expr e m

let traverse (prog : Shared_ast.typed Scopelang.Ast.program) : var PMap.t =
  let m =
    ModuleName.Map.fold
      (fun _m_name decl_map acc ->
        ScopeName.Map.fold
          (fun _sname scope acc -> traverse_scope_sig (Mark.remove scope) acc)
          decl_map acc)
      prog.program_modules PMap.empty
  in
  let m = TopdefName.Map.fold traverse_topdef prog.program_topdefs m in
  let all_scopes =
    ScopeName.Map.values prog.program_scopes |> List.map Mark.remove
  in
  List.fold_right traverse_scope all_scopes m

let populate (prog : Shared_ast.typed Scopelang.Ast.program) : t =
  let variables = traverse prog in
  let add f = function None -> Some (f empty_lookup) | Some v -> Some (f v) in
  let add_def p =
    add (fun v ->
        {
          v with
          definitions =
            (match v.definitions with
            | None -> Some [p]
            | Some l -> Some (p :: l));
        })
  in
  let add_decl p = add (fun v -> { v with declaration = Some p }) in
  let add_usage p =
    add (fun v ->
        {
          v with
          usages =
            (match v.usages with None -> Some [p] | Some l -> Some (p :: l));
        })
  in
  let lookup_table =
    PMap.fold
      (fun p x tbl ->
        let add, hash =
          match x with
          | Topdef jump -> (fun m -> add_def p m |> add_decl p), jump.hash
          | Definition jump -> add_def p, jump.hash
          | Declaration jump -> add_decl p, jump.hash
          | Usage jump -> add_usage p, jump.hash
        in
        LTable.update hash add tbl)
      variables LTable.empty
  in
  { variables; lookup_table }

let lookup (tables : t) (p : Pos.t) : lookup_entry option =
  PMap.find_opt p tables.variables
  |> function
  | Some (Topdef j | Definition j | Declaration j | Usage j) ->
    LTable.find_opt j.hash tables.lookup_table
  | None -> None

let lookup_type (tables : t) (p : Pos.t) : typ option =
  PMap.find_opt p tables.variables
  |> function
  | Some (Topdef j | Definition j | Declaration j | Usage j) -> Some j.typ
  | None -> None

let var_to_symbol (p : Pos.t) (var : var) : Linol_lwt.SymbolInformation.t =
  let open Linol_lwt in
  let name, (kind : SymbolKind.t) =
    match var with
    | Topdef v -> v.name, Constant
    | Definition v -> v.name, Function
    | Declaration v -> v.name, Interface
    | Usage v -> v.name, Variable
  in
  let location =
    Location.create ~range:(range_of_pos p)
      ~uri:(DocumentUri.of_path (Pos.get_file p))
  in
  Linol_lwt.SymbolInformation.create ~kind ~name ~location ()
