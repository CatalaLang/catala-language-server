open Catala_utils
open Shared_ast
open Utils
open Scopelang.Ast

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

  let format ppf p = Format.fprintf ppf "%s" (Pos.to_string_short p)
end)

type jump = { name : string; var_id : int; typ : typ }

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

let pp ppf variables =
  let open Format in
  let pp_var ppf = function
    | Topdef { name; _ } -> fprintf ppf "topdef: %s" name
    | Definition { name; _ } -> fprintf ppf "definition: %s" name
    | Declaration { name; _ } -> fprintf ppf "declaration: %s" name
    | Usage { name; _ } -> fprintf ppf "usage: %s" name
  in
  Format.fprintf ppf "@[<v>@[<v 2>variables:@ %a@]@]"
    (PMap.format_bindings ~pp_sep:Format.pp_print_cut (fun ppf f v ->
         Format.fprintf ppf "%a: %t" pp_var v f))
    variables

let traverse_expr e m =
  let open Shared_ast in
  let open Catala_utils in
  let rec f e acc =
    match Mark.remove e with
    | ELocation (ScopelangScopeVar { name; _ }) ->
      let (Typed { pos = _; ty = typ }) = Mark.get e in
      let (scope_var : ScopeVar.t), pos = name in
      let name =
        Format.asprintf "ScopeVar(%a#%d)" ScopeVar.format scope_var
          (ScopeVar.id scope_var)
      in
      let var = Usage { name; var_id = ScopeVar.id scope_var; typ } in
      PMap.add pos var acc
    | ELocation (ToplevelVar { name; _ }) ->
      let (Typed { pos; ty = typ }) = Mark.get e in
      let (topdef_var : TopdefName.t), _ = name in
      let name =
        Printf.sprintf "%s#%d"
          (TopdefName.to_string topdef_var)
          (TopdefName.id topdef_var)
      in
      let var = Usage { name; var_id = TopdefName.id topdef_var; typ } in
      PMap.add pos var acc
    | _ -> Expr.shallow_fold f e acc
  in
  Expr.shallow_fold f e m

let traverse_scope_decl (rule : typed rule) m : var PMap.t =
  match rule with
  | ScopeVarDefinition { var; typ; io = _; e }
  | SubScopeVarDefinition { var; typ; var_within_origin_scope = _; e } ->
    let var, pos_l = var in
    (* FIXME buggy locations *)
    let _pos = snd (ScopeVar.get_info var) in
    let name =
      Printf.sprintf "%s#%d" (ScopeVar.to_string var) (ScopeVar.id var)
    in
    let var = Definition { name; var_id = ScopeVar.id var; typ } in
    let m = List.fold_right (fun p -> PMap.add p var) pos_l m in
    traverse_expr e m
  | Assertion e -> traverse_expr e m

let traverse_scope_sig (type a) (scope : a scope_decl) m : var PMap.t =
  Format.eprintf "%s@." __LOC__;
  ScopeVar.Map.fold
    (fun var var_ty m ->
      let name =
        Printf.sprintf "%s#%d" (ScopeVar.to_string var) (ScopeVar.id var)
      in
      let pos = snd (ScopeVar.get_info var) in
      let var =
        Declaration { name; var_id = ScopeVar.id var; typ = var_ty.svar_out_ty }
      in
      Format.eprintf "%s: %s@." name __LOC__;
      PMap.add pos var m)
    scope.scope_sig m

let traverse_scope (scope : typed scope_decl) m : var PMap.t =
  let m = traverse_scope_sig scope m in
  List.fold_right traverse_scope_decl scope.scope_decl_rules m

let traverse_topdef (topdef : TopdefName.t) ((e, typ) : typed expr * typ) m :
    var PMap.t =
  let name =
    Printf.sprintf "topdef(%s#%d)"
      (TopdefName.to_string topdef)
      (TopdefName.id topdef)
  in
  let topdef_pos = snd (TopdefName.get_info topdef) in
  let topdef = Topdef { name; var_id = TopdefName.id topdef; typ } in
  let m = PMap.add topdef_pos topdef m in
  traverse_expr e m

let traverse (prog : Shared_ast.typed Scopelang.Ast.program) : var PMap.t =
  Log.debug (fun m -> m "%s" __LOC__);
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
  let pp_o ppf { declaration; definitions; usages } =
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
  in
  let add f = function
    | None -> Some (f empty_lookup)
    | Some v ->
      let af = f v in
      Log.debug (fun m -> m "BEFORE:(%a) vs. AFTER:(%a)" pp_o v pp_o af);
      Some (f v)
  in
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
        let add, var_id =
          match x with
          | Topdef jump -> (fun m -> add_def p m |> add_decl p), jump.var_id
          | Definition jump -> add_def p, jump.var_id
          | Declaration jump -> add_decl p, jump.var_id
          | Usage jump -> add_usage p, jump.var_id
        in
        LTable.update var_id add tbl)
      variables LTable.empty
  in
  Log.debug (fun m -> m "Jump populate done:@\n%a" pp variables);
  { variables; lookup_table }

let lookup (tables : t) (p : Pos.t) : lookup_entry option =
  PMap.find_opt p tables.variables
  |> function
  | Some (Topdef j | Definition j | Declaration j | Usage j) ->
    LTable.find_opt j.var_id tables.lookup_table
  | None -> None

let lookup_type (tables : t) (p : Pos.t) : typ option =
  PMap.find_opt p tables.variables
  |> function
  | Some (Topdef j | Definition j | Declaration j | Usage j) -> Some j.typ
  | None -> None
