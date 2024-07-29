let () =
  let combine r1 r2 =
    let report src level ~over k msgf =
      let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
      r2.Logs.report src level ~over (fun () -> v) msgf
    in
    { Logs.report }
  in
  let logfile =
    let oc = open_out "/tmp/log" in
    let fmt = Format.formatter_of_out_channel oc in
    Logs.format_reporter ~app:fmt ~dst:fmt ()
  in
  let err_std =
    Logs.format_reporter ~app:Format.err_formatter ~dst:Format.err_formatter ()
  in
  Logs.set_reporter (combine logfile err_std);
  Logs.set_level (Some Logs.Debug)

let run () =
  Log.app (fun m ->
      m "cmd: %a"
        Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
        (Array.to_list Sys.argv));
  let s = new Server.catala_lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in

  let task = Linol_lwt.Jsonrpc2.run (server ~env:()) in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    Log.err (fun m -> m "uncaught exception: %s" (Printexc.to_string e));
    exit 1

let () = run ()
