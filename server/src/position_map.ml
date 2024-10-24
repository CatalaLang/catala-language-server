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
  type itv_node = Node of { itv : int * int; data : D.t; children : trie }
  and trie = itv_node list

  type t = trie

  let rec pp_node ppf (Node { itv = i, j; data; children }) =
    let open Format in
    let pp_d fmt i =
      if i = max_int then fprintf fmt "EOL" else pp_print_int fmt i
    in
    match children with
    | [] -> fprintf ppf "@[<h>(%a⟷%a): %a@]" pp_d i pp_d j D.format data
    | _ ->
      fprintf ppf "@[<v 2>(%a⟷%a): %a@ %a@]" pp_d i pp_d j D.format data pp_trie
        children

  and pp_trie ppf tries =
    let open Format in
    fprintf ppf "@[<v>%a@]" (pp_print_list ~pp_sep:pp_print_cut pp_node) tries

  let is_in (i, j) n = i <= n && n <= j
  let ( let*? ) opt f = match opt with None -> None | Some v -> f v

  let rec lookup i trie =
    List.find_opt (fun (Node { itv; _ }) -> is_in itv i) trie
    |> function
    | None -> None
    | Some (Node { children = []; data; _ }) -> Some data
    | Some (Node { children; data; _ }) -> (
      match lookup i children with None -> Some data | Some v -> Some v)

  let compare_itv ((i, j) as itv) (i', j') =
    if i = i' && j = j' then `Equal
    else if j' < i then `Disjoint_left
    else if i' > j then `Disjoint_right
    else if i <= i' && j' <= j then `Subset
    else if i' < i && j' > j then `Supset
    else if is_in itv i' && j' > j then
      `Left_inclusion
        ((i', j) (* included part *), (j + 1, j') (* disjoint part *))
    else (
      assert (i > i' && is_in itv j');
      `Right_inclusion
        ((i', i - 1) (* disjoint part *), (i, j') (* included part *)))

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
        | `Disjoint_right -> List.rev acc, t
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
          (* Introduce quadratric behavior but shouldn't be impactful with our
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
          | _ -> assert false))
    in
    let rec loop acc itv : trie -> trie = function
      | [] -> Node { itv; data; children = [] } :: acc |> List.rev
      | (Node ({ itv = itv_p; children; _ } as node_r) as node) :: t as l -> (
        match compare_itv itv_p itv with
        | `Equal ->
          if data <> node_r.data then
            Log.warn (fun m ->
                m
                  "different data present for new node (%a) vs. previous node \
                   (%a)"
                  pp_node
                  (Node { itv; data; children = [] })
                  pp_node node);
          (* We do not create equivalent nodes *)
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

module LineMap = Map.Make (struct
  include Int

  let format = Format.pp_print_int
end)

module Make (D : Data) = struct
  module Trie = Make_trie (D)

  type pmap = Trie.t LineMap.t FileMap.t
  type t = pmap

  let pp ppf pmap =
    let open Format in
    let pp_lines ppf lmap =
      fprintf ppf "@[<v 2>lines:@ %a@]"
        (LineMap.format ~pp_sep:pp_print_cut Trie.pp_trie)
        lmap
    in
    fprintf ppf "@[<v 2>variables:@ @[<v 2>%a@]@]"
      (FileMap.format ~pp_sep:pp_print_cut pp_lines)
      pmap

  let ( -- ) i j = List.init (j - i + 1) (fun x -> i + x)

  let lines pos v =
    let s = Pos.get_start_line pos in
    let e = Pos.get_end_line pos in
    let s_col = Pos.get_start_column pos in
    let e_col = Pos.get_end_column pos in
    if s = e then LineMap.singleton s ((s_col, e_col), v)
    else
      let s_itv = (s_col, max_int), v in
      let e_itv = (0, e_col), v in
      LineMap.of_list
        ((s, s_itv)
        :: (e, e_itv)
        :: List.map (fun i -> i, ((0, max_int), v)) (s + 1 -- (e - 1)))

  let merge_tries _i trie trie' : Trie.t option =
    match trie, trie' with
    | None, None -> None
    | None, Some (itv, data) -> Some [Node { itv; data; children = [] }]
    | Some trie, None -> Some trie
    | Some trie, Some (itv, data) -> Some (Trie.insert itv data trie)

  let add pos data variables =
    FileMap.update (Pos.get_file pos)
      (function
        | None ->
          Some
            (lines pos data
            |> LineMap.map (fun (itv, data) ->
                   [Trie.Node { itv; data; children = [] }]))
        | Some im -> Some (LineMap.merge merge_tries im (lines pos data)))
      variables

  let lookup pos pmap =
    let ( let* ) = Option.bind in
    (* we assume that pos's start/end lines, start/end column are equal *)
    let* lmap = FileMap.find_opt (Pos.get_file pos) pmap in
    let* trie = LineMap.find_opt (Pos.get_start_line pos) lmap in
    Trie.lookup (Pos.get_start_column pos) trie

  let fold f pmap acc =
    FileMap.fold
      (fun file lmap acc ->
        LineMap.fold
          (fun line trie acc ->
            let l = List.concat_map Trie.gather_children trie in
            List.fold_left
              (fun acc (Trie.Node { itv = i, j; data; _ }) ->
                f (Pos.from_info file line line i j) data acc)
              acc l)
          lmap acc)
      pmap acc

  let elements pmap = fold (fun pos var acc -> (pos, var) :: acc) pmap []
  let empty = FileMap.empty
end
