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

module type Data = sig
  type t

  val format : Format.formatter -> t -> unit
end

module Make_trie (D : Data) = struct
  type itv = (int (* line *) * int (* col *)) * (int (* line *) * int (* col *))

  type node = Node of { itv : itv; data : D.t; children : trie }
  and trie = node list

  type t = trie

  let pp_itv ppf ((li, i), (lj, j)) =
    Format.fprintf ppf "(%d:%d)⟷(%d:%d)" li i lj j

  let itv_to_range ((li, i), (lj, j)) =
    let pos = Catala_utils.Pos.from_info "" li i lj j in
    Utils.range_of_pos pos

  let rec pp_node ppf (Node { itv; data; children }) =
    let open Format in
    match children with
    | [] -> fprintf ppf "@[<h>%a: %a@]" pp_itv itv D.format data
    | _ ->
      fprintf ppf "@[<v 2>%a: %a@ %a@]" pp_itv itv D.format data pp_trie
        children

  and pp_trie ppf tries =
    let open Format in
    fprintf ppf "@[<v>%a@]" (pp_print_list ~pp_sep:pp_print_cut pp_node) tries

  let is_in ((li, i), (lj, j)) (ln, n) =
    (ln > li && ln < lj)
    || (ln = li && ln = lj && i <= n && n <= j)
    || (ln > li && ln = lj && n <= j)
    || (ln = li && ln < lj && i <= n)

  let is_included itv (left, right) = is_in itv left && is_in itv right

  let rec lookup i trie =
    List.find_opt (fun (Node { itv; _ }) -> is_in itv i) trie
    |> function
    | None -> None
    | Some (Node { children = []; data; _ }) -> Some data
    | Some (Node { children; data; _ }) -> (
      match lookup i children with None -> Some data | Some v -> Some v)

  let rec lookup_with_range i trie =
    List.find_opt (fun (Node { itv; _ }) -> is_in itv i) trie
    |> function
    | None -> None
    | Some (Node { itv; children = []; data }) -> Some (itv_to_range itv, data)
    | Some (Node { itv; children; data }) -> (
      match lookup_with_range i children with
      | None -> Some (itv_to_range itv, data)
      | Some v -> Some v)

  let compare_itv (((li, i), (lj, j)) as itv) (((li', i'), (lj', j')) as itv') =
    if itv = itv' then `Equal
    else if is_included itv itv' then `Subset
    else if lj' < li || (lj' = li && j' < i) then `Disjoint_left
    else if li' > lj || (li' = lj && i' > j) then `Disjoint_right
    else if (not (is_in itv (li', i'))) && not (is_in itv (lj', j')) then
      `Supset
    else if is_in itv (li', i') then
      `Left_inclusion
        ( ((li', i'), (lj, j)) (* included part *),
          ((lj, j + 1), (lj', j')) (* disjoint part *) )
    else (
      assert (is_in itv (lj', j'));
      `Right_inclusion
        ( ((li', i'), (li, i - 1)) (* disjoint part *),
          ((li, i), (lj', j')) (* included part *) ))

  let rec gather_children (Node { children; _ }) =
    let fresh_children =
      List.map (fun (Node node) -> Node { node with children = [] }) children
    in
    fresh_children @ List.concat_map gather_children children

  let rec insert itv data trie =
    let rec find_included_nodes acc = function
      | [] -> List.rev acc, []
      | (Node n as h) :: t -> (
        match compare_itv itv n.itv with
        | `Equal -> assert false
        | `Subset -> find_included_nodes (h :: acc) t
        | `Disjoint_right -> List.rev (h :: acc), t
        | `Disjoint_left | `Supset | `Right_inclusion _ ->
          (* by construction *) assert false
        | `Left_inclusion (included_itv, disjoint_part) -> (
          let included_node =
            Node { n with itv = included_itv; children = [] }
          in
          let disjoint_node =
            Node { n with itv = disjoint_part; children = [] }
          in
          let all_children = gather_children h in
          (* Introduce quadratric behavior but shouldn't be impactful for our
             use-case *)
          let sub_trie =
            List.fold_left
              (fun trie (Node children) ->
                insert children.itv children.data trie)
              [included_node; disjoint_node]
              all_children
          in
          match sub_trie with
          | [included_node; disjoint_node] ->
            List.rev (included_node :: acc), disjoint_node :: t
          | [a; b; c] -> List.rev (a :: acc), b :: c :: t
          | _ -> assert false))
    in
    let rec loop acc itv : trie -> trie = function
      | [] -> Node { itv; data; children = [] } :: acc |> List.rev
      | (Node ({ itv = itv_p; children; _ } as node_r) as node) :: t as l -> (
        match compare_itv itv_p itv with
        | `Equal ->
          if data <> node_r.data then
            Log.debug (fun m ->
                m
                  "Warning: different data present for new node (%a) vs. \
                   previous node (%a)"
                  pp_node
                  (Node { itv; data; children = [] })
                  pp_node node);
          (* Do not overwrite nodes *)
          List.rev_append (node :: acc) t
        | `Disjoint_left ->
          List.rev_append (Node { itv; data; children = [] } :: acc) l
        | `Disjoint_right -> loop (node :: acc) itv t
        | `Subset ->
          let new_children = insert itv data children in
          List.rev_append
            (Node { node_r with children = new_children } :: acc)
            t
        | `Supset ->
          let all_included_nodes, disjoint_nodes = find_included_nodes [] l in
          let node = Node { itv; data; children = all_included_nodes } in
          List.rev_append (node :: acc) disjoint_nodes
        | `Left_inclusion (included_itv, disjoint_part) ->
          let new_children = insert included_itv data children in
          loop
            (Node { node_r with children = new_children } :: acc)
            disjoint_part t
        | `Right_inclusion (disjoint_part, included_itv) ->
          let acc = Node { itv = disjoint_part; data; children = [] } :: acc in
          let new_children = insert included_itv data children in
          List.rev_append
            (Node { node_r with children = new_children } :: acc)
            t)
    in
    loop [] itv trie
end

open Catala_utils
module FileMap = Map.Make (String)

module Make (D : Data) = struct
  module Trie = Make_trie (D)

  type pmap = Trie.t FileMap.t
  type t = pmap

  let pp ppf pmap =
    let open Format in
    fprintf ppf "@[<v 2>variables:@ %a@]"
      (FileMap.format ~pp_sep:pp_print_cut Trie.pp_trie)
      pmap

  let ( -- ) i j = List.init (j - i + 1) (fun x -> i + x)

  let merge_tries _i trie trie' : Trie.t option =
    match trie, trie' with
    | None, None -> None
    | None, Some (itv, data) -> Some [Node { itv; data; children = [] }]
    | Some trie, None -> Some trie
    | Some trie, Some (itv, data) -> Some (Trie.insert itv data trie)

  let pos_to_itv pos =
    let li = Pos.get_start_line pos in
    let i = Pos.get_start_column pos in
    let lj = Pos.get_end_line pos in
    let j = Pos.get_end_column pos in
    (li, i), (lj, j)

  let pp_raw ppf (p, d) =
    let open Format in
    fprintf ppf "@[<h>%a: %a@]" Trie.pp_itv (pos_to_itv p) D.format d

  let add pos data variables =
    if pos = Pos.no_pos then variables
    else
      let itv = pos_to_itv pos in
      FileMap.update (Pos.get_file pos)
        (function
          | None -> Some [Trie.Node { itv; data; children = [] }]
          | Some trie -> Some (Trie.insert itv data trie))
        variables

  let lookup pos pmap =
    let ( let* ) = Option.bind in
    (* we assume that pos's start/end lines, start/end column are equal *)
    let* trie = FileMap.find_opt (Pos.get_file pos) pmap in
    Trie.lookup (Pos.get_start_line pos, Pos.get_start_column pos) trie

  let lookup_with_range pos pmap =
    let ( let* ) = Option.bind in
    (* we assume that pos's start/end lines, start/end column are equal *)
    let* trie = FileMap.find_opt (Pos.get_file pos) pmap in
    Trie.lookup_with_range
      (Pos.get_start_line pos, Pos.get_start_column pos)
      trie

  let fold f pmap acc =
    let rec fold file trie acc =
      List.fold_left
        (fun acc (Trie.Node { itv = (li, i), (lj, j); children; data }) ->
          let acc = f (Pos.from_info file li i lj j) data acc in
          fold file children acc)
        acc trie
    in
    FileMap.fold fold pmap acc

  let fold_on_file file f pmap acc =
    FileMap.find_opt file pmap
    |> function
    | None -> acc | Some pmap -> fold f (FileMap.singleton file pmap) acc

  let iter f pmap = fold (fun k v () -> f k v) pmap ()
  let empty = FileMap.empty
end
