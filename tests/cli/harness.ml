(* The plugin driven through its command line, as the editor drives it, in a
   scratch copy of a fixture project. *)

include Tezt
include Tezt.Base
module O = Catala_types_atd.Catala_types_t
module J = Catala_types_atd.Catala_types_j

(* Resolved before any test changes directory. *)
let fixtures = Sys.getcwd () // "fixtures"
let plugins = Sys.getcwd () // "plugins"

let register ~__FILE__ ~title ~tags f =
  Test.register ~__FILE__ ~title ~tags:("cli" :: tags) f

(* ---- projects ------------------------------------------------------------ *)

(* A copy of [fixtures/name] ([only] its listed files), at [into] under the
   test's temporary directory. *)
let project ?only ?into name =
  let dir = Temp.dir (Option.value into ~default:name) in
  let files =
    match only with
    | Some files -> files
    | None ->
      Sys.readdir (fixtures // name)
      |> Array.to_list
      |> List.filter (fun f -> f.[0] <> '_' && f.[0] <> '.')
  in
  List.iter
    (fun f ->
      write_file (dir // f) ~contents:(read_file (fixtures // name // f)))
    files;
  dir

let edit dir file f =
  write_file (dir // file) ~contents:(f (read_file (dir // file)))

(* Every occurrence of the literal [s] replaced by [by]. *)
let subst s ~by text = Re.replace_string (Re.compile (Re.str s)) ~by text

(* ...and of the whole word [w]. *)
let subst_word w ~by text =
  Re.replace_string (Re.compile Re.(seq [bow; str w; eow])) ~by text

let contains text s = Re.execp (Re.compile (Re.str s)) text
let count text s = List.length (Re.all (Re.compile (Re.str s)) text)

(* ---- processes ----------------------------------------------------------- *)

type result = { cmd : string; ok : bool; out : string; err : string }

(* Tezt spawns in the current directory; spawning is synchronous and a worker
   runs one test at a time, so changing it around the spawn is enough. *)
let in_dir dir f =
  let here = Sys.getcwd () in
  Sys.chdir dir;
  Fun.protect ~finally:(fun () -> Sys.chdir here) f

let exec ?(env = []) ?(stdin = "") dir prog args =
  let env = String_map.of_list (("CATALA_PLUGINS", plugins) :: env) in
  let p, input =
    in_dir dir (fun () -> Process.spawn_with_stdin ~env prog args)
  in
  let* () = Lwt_io.write input stdin in
  let* () = Lwt_io.close input in
  let* out = Lwt_io.read (Process.stdout p)
  and* err = Lwt_io.read (Process.stderr p) in
  let* status = Process.wait p in
  return
    {
      cmd = String.concat " " (prog :: args);
      ok = status = Unix.WEXITED 0;
      out;
      err;
    }

let testcase ?env ?stdin dir args =
  exec ?env ?stdin dir "catala" ("testcase" :: args)

let succeed r =
  if not r.ok then Test.fail "%s failed:\n%s%s" r.cmd r.out r.err;
  r.out

let start dir =
  let* r = exec dir "clerk" ["start"] in
  ignore (succeed r);
  unit

let typecheck dir file =
  let* r = exec dir "clerk" ["typecheck"; file] in
  ignore (succeed r);
  unit

(* ---- answers, not crashes
   ------------------------------------------------- *)

let crash = rex "Unexpected error|Assert_failure|Not_found|Failure"

(* [r] explains itself with [says], whatever its exit status. *)
let says says r =
  let text = r.out ^ r.err in
  if not (contains text says) then
    Test.fail "%s should say %S:\n%s" r.cmd says text;
  if text =~ crash then Test.fail "%s crashed:\n%s" r.cmd text

let refuses msg r =
  if r.ok then Test.fail "%s accepted it:\n%s" r.cmd r.out;
  says msg r

(* ---- the wire
   ------------------------------------------------------------- *)

let read ?(reader = "read") dir file =
  let* r = testcase dir [reader; file] in
  return (J.test_list_of_string (succeed r))

let partial_read = read ~reader:"partial-read"

let rebuild ?scope ?stdin ?buffer dir file =
  let args =
    (match scope with Some s -> ["--scope"; s] | None -> [])
    @ (match buffer with Some b -> ["--buffer-path"; b] | None -> [])
    @ [file]
  in
  let* r = testcase ?stdin dir ("rebuild" :: args) in
  return (J.recovery_of_string (succeed r))

let run ?env ?stdin ?buffer ~scope dir file =
  let args =
    ["run"; "-l"; "en"; "--scope"; scope]
    @ (match buffer with Some b -> ["--buffer-path"; b] | None -> [])
    @ [file]
  in
  let* r = testcase ?env ?stdin dir args in
  return (J.test_run_of_string (succeed r))

let write ?(lang = "en") dir tests =
  let* r =
    testcase ~stdin:(J.string_of_test_list tests) dir ["write"; "-l"; lang]
  in
  return (succeed r)

let rebuilt (r : O.recovery) =
  List.filter_map (fun (t : O.recovered_test) -> t.rebuilt) r.tests

let path_name path =
  String.concat "."
    (List.map
       (function
         | `StructField s | `EnumPayload s -> s
         | `ListIndex i | `TupleIndex i -> string_of_int i)
       path)

let outcome_name : O.carry_outcome -> string = function
  | Fits -> "Fits"
  | Wrap -> "Wrap"
  | Unwrap -> "Unwrap"
  | WasUnset -> "WasUnset"
  | WasAbsentNowRequired -> "WasAbsentNowRequired"
  | TypeChanged _ -> "TypeChanged"
  | Dropped -> "Dropped"
  | Partial -> "Partial"

(* Each carry record of the recovery as ["path:side:outcome"]. *)
let marks (r : O.recovery) =
  List.concat_map
    (fun (t : O.recovered_test) ->
      List.map
        (fun (c : O.carry_record) ->
          sf "%s:%s:%s" (path_name c.path)
            (match c.side with In -> "In" | Out -> "Out")
            (outcome_name c.outcome))
        t.outcomes)
    r.tests

let raw (io : O.test_io) =
  Option.map (fun (d : O.value_def) -> d.value.value) io.value

(* [io] with its value replaced by [raw]. *)
let with_raw raw (io : O.test_io) =
  let value : O.value_def =
    match io.value with
    | Some d -> { d with value = { d.value with value = raw } }
    | None -> { value = { value = raw; attrs = [] }; pos = None }
  in
  { io with value = Some value }

let update name f l =
  List.map (fun (n, x) -> if n = name then n, f x else n, x) l

let names_of tests = List.map (fun (t : O.test) -> t.testing_scope) tests
