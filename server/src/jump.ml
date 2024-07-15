open Catala_utils
open Shared_ast
open Utils

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

type jump = { name : string; var_id : int }

module LTable = Stdlib.Map.Make (Int)

type lookup_entry = {
  (* TODO: lists? *)
  declaration : Pos.t option;
  definition : Pos.t option;
  usage : Pos.t option;
}

let empty_lookup = { declaration = None; definition = None; usage = None }

type t = {
  declarations : jump PMap.t;
  definitions : jump PMap.t;
  usage : jump PMap.t;
  lookup_table : lookup_entry LTable.t;
}

let pp ppf { declarations; definitions; usage; lookup_table = _ } =
  Format.fprintf ppf
    "@[<v>@[<v 2>variable declarations:@ %a@]@ @[<v 2>variable definitions:@ \
     %a@]@ @[<v 2>variable usage:@ %a@]@]"
    (PMap.format_bindings ~pp_sep:Format.pp_print_cut (fun ppf f { name; _ } ->
         Format.fprintf ppf "%s: %t" name f))
    declarations
    (PMap.format_bindings ~pp_sep:Format.pp_print_cut (fun ppf f { name; _ } ->
         Format.fprintf ppf "%s: %t" name f))
    definitions
    (PMap.format_bindings ~pp_sep:Format.pp_print_cut (fun ppf f { name; _ } ->
         Format.fprintf ppf "%s: %t" name f))
    usage

let traverse_exprs prog =
  let open Shared_ast in
  let open Catala_utils in
  let rec f e acc =
    match Mark.remove e with
    | ELocation (DesugaredScopeVar { name; _ }) ->
      let (Untyped { pos }) = Mark.get e in
      let (scope_var : ScopeVar.t), _ = name in
      let name =
        Format.asprintf "ScopeVar(%a#%d)" ScopeVar.format scope_var
          (ScopeVar.id scope_var)
      in
      let jump = { name; var_id = ScopeVar.id scope_var } in
      PMap.add pos jump acc
    | ELocation (ToplevelVar { name; _ }) ->
      let (Untyped { pos }) = Mark.get e in
      let (topdef_var : TopdefName.t), _ = name in
      let name =
        Format.asprintf "TopVar(%a#%d)" TopdefName.format topdef_var
          (TopdefName.id topdef_var)
      in
      let jump = { name; var_id = TopdefName.id topdef_var } in
      PMap.add pos jump acc
    | _ -> Expr.shallow_fold f e acc
  in
  Desugared.Ast.fold_exprs
    ~f:(fun acc (e : _ gexpr) -> f e acc)
    ~init:PMap.empty prog

let traverse_declarations (prog : Desugared.Ast.program) =
  let open Desugared.Ast in
  let join = PMap.union (fun _k v _ -> Some v) in
  let map_join f l m = List.fold_left join m (List.map f l) in
  let traverse_scope scope =
    ScopeVar.Map.fold
      (fun var _or_state m ->
        (* TODO: states *)
        let name =
          Format.asprintf "ScopeVarDecl(%s#%d)" (ScopeVar.to_string var)
            (ScopeVar.id var)
        in
        let pos =
          let _s, p = ScopeVar.get_info var in
          (* _s => ??? *)
          p
        in
        let jump = { name; var_id = ScopeVar.id var } in
        PMap.add pos jump m)
      scope.scope_vars PMap.empty
  in
  let traverse_topdef ((topdef_name : TopdefName.t), _topdef_expr) =
    (* TODO: handle topdef_expr *)
    let name =
      Format.asprintf "TopdefDecl(%s#%d)"
        (TopdefName.to_string topdef_name)
        (TopdefName.id topdef_name)
    in
    let pos =
      let _s, p = TopdefName.get_info topdef_name in
      (* _s => ??? *)
      p
    in
    let jump = { name; var_id = TopdefName.id topdef_name } in
    PMap.singleton pos jump
  in
  let traverse_module (modul : modul) =
    let { module_scopes; module_topdefs } = modul in
    map_join traverse_scope (ScopeName.Map.values module_scopes) PMap.empty
    |> map_join traverse_topdef (TopdefName.Map.bindings module_topdefs)
  in
  let all_modules =
    prog.program_root :: ModuleName.Map.values prog.program_modules
  in
  map_join traverse_module all_modules PMap.empty

let traverse_definitions (prog : Desugared.Ast.program) =
  let open Desugared.Ast in
  let join = PMap.union (fun _k v _ -> Some v) in
  let map_join f l m = List.fold_left join m (List.map f l) in
  let traverse_scope scope =
    ScopeDef.Map.fold
      (fun (scopedef : ScopeDef.t) _ m ->
        let marked_var, _kind = scopedef in
        let pos = Mark.get marked_var in
        let var = Mark.remove marked_var in
        let name =
          Format.asprintf "ScopeDef(%s#%d)" (ScopeVar.to_string var)
            (ScopeVar.id var)
        in
        let jump = { name; var_id = ScopeVar.id var } in
        PMap.add pos jump m)
      scope.scope_defs PMap.empty
  in
  let traverse_topdef ((topdef_name : TopdefName.t), _topdef_expr) =
    (* TODO: handle topdef_expr *)
    let name =
      Format.asprintf "TopdefDef(%s#%d)"
        (TopdefName.to_string topdef_name)
        (TopdefName.id topdef_name)
    in
    let pos =
      let _s, p = TopdefName.get_info topdef_name in
      (* _s => ??? *)
      p
    in
    let jump = { name; var_id = TopdefName.id topdef_name } in
    PMap.singleton pos jump
  in
  let traverse_module (modul : modul) =
    let { module_scopes; module_topdefs } = modul in
    map_join traverse_scope (ScopeName.Map.values module_scopes) PMap.empty
    |> map_join traverse_topdef (TopdefName.Map.bindings module_topdefs)
  in
  let all_modules =
    prog.program_root :: ModuleName.Map.values prog.program_modules
  in
  map_join traverse_module all_modules PMap.empty

let populate_lookup_table ~definitions ~declarations ~usage =
  let lookup_table = LTable.empty in
  let add f = function None -> Some (f empty_lookup) | Some v -> Some (f v) in
  let lookup_table =
    PMap.fold
      (fun p { var_id; _ } tbl ->
        LTable.update var_id (add (fun v -> { v with definition = Some p })) tbl)
      definitions lookup_table
  in
  let lookup_table =
    PMap.fold
      (fun p { var_id; _ } tbl ->
        LTable.update var_id
          (add (fun v -> { v with declaration = Some p }))
          tbl)
      declarations lookup_table
  in
  let lookup_table =
    PMap.fold
      (fun p { var_id; _ } tbl ->
        LTable.update var_id (add (fun v -> { v with usage = Some p })) tbl)
      usage lookup_table
  in
  lookup_table

let traverse (prog : Desugared.Ast.program) : t =
  Log.debug (fun m -> m "%s" __LOC__);
  let usage = traverse_exprs prog in
  let declarations = traverse_declarations prog in
  let definitions = traverse_definitions prog in
  let lookup_table = populate_lookup_table ~definitions ~declarations ~usage in
  let res = { definitions; declarations; usage; lookup_table } in
  Log.debug (fun m -> m "JUMP DONE:@\n%a@." pp res);
  res

let lookup (tables : t) (p : Pos.t) : lookup_entry option =
  let find m k =
    PMap.find_opt p m
    |> function
    | Some { var_id; name = _ } -> LTable.find_opt var_id tables.lookup_table
    | None -> k ()
  in
  find tables.definitions
  @@ fun () ->
  find tables.declarations @@ fun () -> find tables.usage @@ fun () -> None
