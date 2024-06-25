exception Finished of (State.t, string) result

let handle_exn _ = function
  (* Fallback *)
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace exn bt

let finally st e =
  match e with
  | None -> st
  | Some (bt, exn) ->
    (* Print the backtrace if requested *)
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stdout bt;
    let res = handle_exn st exn in
    raise (Finished res)

let process _preludes path _opt_contents =
  let _dir = Filename.dirname path in
  let _file = Filename.basename path in
  let _preludes =
    (* let rec aux l = *)
    (*   match l with *)
    (*   | [] -> [] *)
    (*   | State.{ dir = d; source = `File f; _ } :: _ *)
    (*     when String.equal f file && String.equal dir d -> *)
    (*     [] *)
    (*   (\* When the opened file is one of the prelude files, ignore it and the *)
    (*      following prelude files. *\) *)
    (*   | h :: t -> h :: aux t *)
    (* in *)
    (* aux preludes *)
    []
  in
  let st = () in
  try
    (* let g = Parser.parse_logic ~preludes l_file in *)
    (* let open Pipeline in *)
    let st =
      ()
      (* run ~finally g st *)
      (*   (fix *)
      (*      (op ~name:"expand" Parser.expand) *)
      (*      (op ~name:"headers" Header.inspect *)
      (*      @>>> op ~name:"typecheck" Typer_Pipe.typecheck *)
      (*      @>|> op (fun st _ -> st, ()) *)
      (*      @>>> _end)) *)
    in
    Ok st
  with
  | Finished res -> assert false
  | exn -> handle_exn st exn
