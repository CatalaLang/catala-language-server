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

open Server_types

module type Data = sig
  type t

  val compare : t -> t -> int
  val format : Format.formatter -> t -> unit
end

module MakeTree (D : Data) = struct
  module DS = Set.Make (D)

  type itv = (int (* line *) * int (* col *)) * (int (* line *) * int (* col *))

  type node = Node of { itv : itv; data : DS.t; children : tree }
  and tree = node list

  type t = tree

  let format_itv ppf ((li, i), (lj, j)) =
    Format.fprintf ppf "(%d:%d)⟷(%d:%d)" li i lj j

  let itv_to_range ((li, i), (lj, j)) =
    let pos = Catala_utils.Pos.from_info "" li i lj j in
    Utils.range_of_pos pos

  let rec format_node ppf (Node { itv; data; children }) =
    let open Format in
    match children with
    | [] ->
      fprintf ppf "@[<h>%a: [ @[<h>%a@] ]@]" format_itv itv
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ; ") D.format)
        (DS.elements data)
    | _ ->
      fprintf ppf "@[<v 2>%a: [ @[<h>%a@] ]@ %a@]" format_itv itv
        (pp_print_list ~pp_sep:pp_print_space D.format)
        (DS.elements data) format_tree children

  and format_tree ppf trees =
    let open Format in
    fprintf ppf "@[<v>%a@]"
      (pp_print_list ~pp_sep:pp_print_cut format_node)
      trees

  let is_in ((li, i), (lj, j)) (ln, n) =
    (ln > li && ln < lj)
    || (ln = li && ln = lj && i <= n && n <= j)
    || (ln > li && ln = lj && n <= j)
    || (ln = li && ln < lj && i <= n)

  let is_in_line ((li, _), (lj, _)) ln = ln >= li && ln <= lj
  let is_included itv (left, right) = is_in itv left && is_in itv right

  let rec lookup i tree =
    List.find_opt (fun (Node { itv; _ }) -> is_in itv i) tree
    |> function
    | None -> None
    | Some (Node { children = []; data; _ }) -> Some data
    | Some (Node { children; data; _ }) -> (
      match lookup i children with None -> Some data | Some v -> Some v)

  let rec lookup_with_range i tree =
    List.find_opt (fun (Node { itv; _ }) -> is_in itv i) tree
    |> function
    | None -> None
    | Some (Node { itv; children = []; data }) -> Some (itv_to_range itv, data)
    | Some (Node { itv; children; data }) -> (
      match lookup_with_range i children with
      | None -> Some (itv_to_range itv, data)
      | Some v -> Some v)

  let all_nodes tree =
    let rev_nodes = ref [] in
    let rec loop (Node { children; _ } as x) =
      rev_nodes := x :: !rev_nodes;
      List.iter loop children
    in
    List.iter loop tree;
    List.rev !rev_nodes

  let lookup_on_line l tree =
    List.find_all (fun (Node { itv; _ }) -> is_in_line itv l) tree
    |> all_nodes
    |> List.filter (fun (Node { itv = (li, _), (lj, _); _ }) ->
        not (li <> l || lj <> l))
    |> List.map (fun (Node { data; _ }) -> data)
    |> function [] -> None | l -> Some l

  let lookup_best_on_line (l : int) tree : DS.t option =
    match List.find_all (fun (Node { itv; _ }) -> is_in_line itv l) tree with
    | [] -> None
    | candidates -> (
      let compare_candidate
          (Node { itv = (li, i), _; _ })
          (Node { itv = (li', i'), _; _ }) =
        (* hypothesis: contains [l] *)
        let dist x =
          let abs x = if x < 0 then -x else x in
          abs (x - l)
        in
        if li = li' then compare i i' else compare (dist li) (dist li')
      in
      let sorted_candidates =
        List.sort compare_candidate (all_nodes candidates)
      in
      match sorted_candidates with
      | [] -> assert false
      | Node { data; _ } :: _ -> Some data)

  let compare_itv (((li, i), (lj, j)) as itv) (((li', i'), (lj', j')) as itv') =
    if itv = itv' then `Equal
    else if is_included itv itv' then `Subset
    else if is_included itv' itv then `Supset
    else if lj' < li || (lj' = li && j' < i) then `Disjoint_left
    else if li' > lj || (li' = lj && i' > j) then `Disjoint_right
    else if is_in itv (li', i') then
      `Left_inclusion
        ( ((li', i'), (lj, j)) (* included part *),
          ((lj, j + 1), (lj', j')) (* disjoint part *) )
    else (
      assert (is_in itv (lj', j'));
      `Right_inclusion
        ( ((li', i'), (li, i - 1)) (* disjoint part *),
          ((li, i), (lj', j')) (* included part *) ))

  let rec insert_all itv (data : DS.t) tree =
    let rec find_included_nodes acc = function
      | [] -> List.rev acc, `Disjoint []
      | (Node n as h) :: t as l -> (
        match compare_itv itv n.itv with
        | `Equal -> assert false
        | `Subset -> find_included_nodes (h :: acc) t
        | `Disjoint_right -> List.rev acc, `Disjoint l
        | `Disjoint_left | `Supset | `Right_inclusion _ ->
          (* by construction *) assert false
        | `Left_inclusion (included_itv, _disjoint_part) ->
          acc, `Joint (included_itv, l))
    in
    let rec loop acc itv : tree -> tree = function
      | [] -> Node { itv; data; children = [] } :: acc |> List.rev
      | (Node ({ itv = itv_p; children; _ } as node_r) as node) :: t as l -> (
        match compare_itv itv_p itv with
        | `Equal ->
          let node = Node { node_r with data = DS.union data node_r.data } in
          List.rev_append (node :: acc) t
        | `Disjoint_left ->
          List.rev_append (Node { itv; data; children = [] } :: acc) l
        | `Disjoint_right -> loop (node :: acc) itv t
        | `Subset ->
          let new_children = insert_all itv data children in
          List.rev_append
            (Node { node_r with children = new_children } :: acc)
            t
        | `Supset -> (
          let all_included_nodes, r = find_included_nodes [] l in
          match r with
          | `Disjoint l ->
            let node = Node { itv; data; children = all_included_nodes } in
            List.rev_append (node :: acc) l
          | `Joint (included_itv, r) ->
            let (_, i'), _ = included_itv in
            let (li, i), (lj, _) = itv in
            let node =
              Node
                {
                  itv = (li, i), (lj, i' - 1);
                  data;
                  children = all_included_nodes;
                }
            in
            List.rev_append (node :: acc) (loop [] included_itv r))
        | `Left_inclusion (included_itv, disjoint_part) ->
          let new_children = insert_all included_itv data children in
          loop
            (Node { node_r with children = new_children } :: acc)
            disjoint_part t
        | `Right_inclusion (disjoint_part, included_itv) ->
          let acc = Node { itv = disjoint_part; data; children = [] } :: acc in
          let new_children = insert_all included_itv data children in
          List.rev_append
            (Node { node_r with children = new_children } :: acc)
            t)
    in
    loop [] itv tree
end

open Catala_utils

module Make (D : Data) = struct
  module Tree = MakeTree (D)
  module DS = Tree.DS

  type pmap = Tree.t Doc_id.Map.t
  type acc = (Pos.t * D.t) list

  let empty_acc = []

  let format ppf pmap =
    let open Format in
    fprintf ppf "@[<v 2>variables:@ %a@]"
      (Doc_id.Map.format ~pp_sep:pp_print_cut Tree.format_tree)
      pmap

  let pos_to_itv pos =
    let li = Pos.get_start_line pos in
    let i = Pos.get_start_column pos in
    let lj = Pos.get_end_line pos in
    let j = Pos.get_end_column pos in
    (li, i), (lj, j)

  let format_entry ppf (p, d) =
    let open Format in
    fprintf ppf "@[<h>%a: %a@]" Tree.format_itv (pos_to_itv p) D.format d

  let add_all pos data variables =
    if pos = Pos.void then variables
    else
      let itv = pos_to_itv pos in
      let data = DS.of_list data in
      Doc_id.Map.update (Doc_id.of_catala_pos pos)
        (function
          | None -> Some [Tree.Node { itv; data; children = [] }]
          | Some tree -> Some (Tree.insert_all itv data tree))
        variables

  let add pos data variables = add_all pos [data] variables

  let finalize acc =
    List.fold_left (fun m (pos, d) -> add pos d m) Doc_id.Map.empty acc

  let add pos data acc = (pos, data) :: acc

  let add_all pos datas acc =
    List.fold_left (fun acc d -> (pos, d) :: acc) acc datas

  let lookup pos (pmap : pmap) =
    let ( let* ) = Option.bind in
    (* we assume that pos's start/end lines, start/end column are equal *)
    let* tree = Doc_id.Map.find_opt (Doc_id.of_catala_pos pos) pmap in
    Tree.lookup (Pos.get_start_line pos, Pos.get_start_column pos) tree

  let lookup_with_range pos pmap =
    let ( let* ) = Option.bind in
    (* we assume that pos's start/end lines, start/end column are equal *)
    let* tree = Doc_id.Map.find_opt (Doc_id.of_catala_pos pos) pmap in
    Tree.lookup_with_range
      (Pos.get_start_line pos, Pos.get_start_column pos)
      tree

  let lookup_best_on_line doc_id l pmap =
    let ( let* ) = Option.bind in
    (* we assume that pos's start/end lines, start/end column are equal *)
    let* tree = Doc_id.Map.find_opt doc_id pmap in
    Tree.lookup_best_on_line l tree |> Option.map DS.choose

  let lookup_on_line doc_id l (pmap : pmap) =
    let ( let* ) = Option.bind in
    (* we assume that pos's start/end lines, start/end column are equal *)
    let* tree = Doc_id.Map.find_opt doc_id pmap in
    Tree.lookup_on_line l tree
    |> Option.map (fun ld -> List.fold_left DS.union DS.empty ld)

  let files pmap = Doc_id.Map.keys pmap

  let fold f pmap acc =
    let rec fold (doc_id : Doc_id.t) tree acc =
      List.fold_left
        (fun acc (Tree.Node { itv = (li, i), (lj, j); children; data }) ->
          let acc = f (Pos.from_info (doc_id :> File.t) li i lj j) data acc in
          fold doc_id children acc)
        acc tree
    in
    Doc_id.Map.fold fold pmap acc

  let fold_on_file file f pmap acc =
    Doc_id.Map.find_opt file pmap
    |> function
    | None -> acc
    | Some pmap -> fold f (Doc_id.Map.singleton file pmap) acc

  let iter f pmap = fold (fun k v () -> f k v) pmap ()
end
