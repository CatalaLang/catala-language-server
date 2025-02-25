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

open QCheck
open Catala_utils

module PMap = Position_map.Make (struct
  include String

  let format = Format.pp_print_string
end)

let column_gen =
  let open Gen in
  let* l = 1 -- 80 in
  let* r = l -- (l + 50) in
  return (l, r)

let line_gen =
  let open Gen in
  let* l = 1 -- 800 in
  let* r = l -- (l + 2) in
  return (l, r)

let pos_gen =
  let open Gen in
  let open Pos in
  let* sl, el = line_gen in
  let* sc, ec = column_gen in
  return (from_info "dummy" sl sc el ec)

let data_gen =
  let open Gen in
  (* string_size (1 -- 10) ~gen:(oneof [char_range 'a' 'z'; char_range 'A'
     'Z']) *)
  string_small_of (oneof [char_range 'a' 'z'; char_range 'A' 'Z'])

let insert_all = List.fold_right (fun (p, v) -> PMap.add p v)

let map_gen =
  let open Gen in
  let* l = list_size (10 -- 15) (pair pos_gen data_gen) in
  return (insert_all l PMap.empty)

let gen_pair = Gen.(pair map_gen (pair pos_gen data_gen))

module S = Set.Make (struct
  type t = string

  let compare = compare
end)

let elements t =
  PMap.fold (fun _ d acc -> S.add_seq (PMap.DS.to_seq d) acc) t S.empty

let gen_arb =
  make
    ~print:(fun (map, (p, d)) ->
      let add = PMap.add p d map in
      Format.asprintf
        "data: %a@\n\
         map:@\n\
         @[<v 2>Before:@ %a@]@\n\
         @[<v 2>After:@ %a@]@\n\
         elements diff:@\n\
         @[<v>%a@]"
        PMap.pp_raw (p, d) PMap.pp map PMap.pp add
        Format.(
          pp_print_list ~pp_sep:pp_print_cut (fun fmt d ->
              Format.fprintf fmt "data:%s" d))
        (let l = S.add d (elements map) in
         let r = elements add in
         S.diff l r |> S.elements))
    gen_pair

let pbt_test =
  let insertion_prop (t, (p, d)) =
    S.equal (S.add d (elements t)) (elements (PMap.add p d t))
    || Option.is_some (PMap.lookup p t)
  in
  QCheck.Test.make ~name:"insertion" ~long_factor:1000 ~count:100_000
    ~max_fail:0 gen_arb insertion_prop

let mk_pos ?lines sc ec =
  let el, ol = match lines with Some (a, b) -> a, b | None -> 1, 1 in
  Pos.from_info "dummy" el sc ol ec

let test_commute () =
  let map = PMap.empty in
  let inner = PMap.add (mk_pos 1 2) "inner" in
  let outter = PMap.add (mk_pos 1 3) "outter" in
  let before = outter (inner map) in
  let after = inner (outter map) in
  assert (before = after)

let () =
  let open Tezt.Test in
  register ~__FILE__ ~title:"commutativity" ~tags:["unit"; "ordering"]
    (fun () -> Lwt.return @@ test_commute ());
  register ~__FILE__ ~title:"PBT w/ qcheck" ~tags:["pbt"; "qcheck"] (fun () ->
      let (Test cell) = pbt_test in
      Lwt.return @@ QCheck.Test.check_cell_exn cell)

let () = Tezt.Test.run ()
