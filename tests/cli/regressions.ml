(* Past bugs of [generate]. *)

open Harness

let rec enum_names : O.typ -> string list = function
  | TEnum e ->
    e.enum_name
    :: List.concat_map
         (fun (_, t) -> Option.fold ~none:[] ~some:enum_names t)
         e.constructors
  | TStruct s -> List.concat_map (fun (_, t) -> enum_names t) s.fields
  | TOption t | TArray t -> enum_names t
  | TTuple ts -> List.concat_map enum_names ts
  | TArrow (args, ret) -> List.concat_map enum_names (ret :: args)
  | TBool | TInt | TRat | TMoney | TDate | TDuration | TUnit | TUnset -> []

let generate dir file scope =
  let* r = testcase dir ["generate"; file; "--scope"; scope] in
  return (J.test_list_of_string (succeed r))

(* An enum reached through nested module aliases (MMS.ME.E) is named from its
   own module. *)
let enum_path () =
  register ~__FILE__ ~title:"an enum through module aliases keeps its own path"
    ~tags:["generate"]
  @@ fun () ->
  let dir = project "enum_path" in
  let* () = start dir in
  let* tests = generate dir "my_scope.catala_en" "S" in
  let names =
    List.concat_map
      (fun (t : O.test) ->
        List.concat_map
          (fun (_, (io : O.test_io)) -> enum_names io.typ)
          (t.test_inputs @ t.test_outputs))
      tests
  in
  Check.list_mem Check.string "My_enum.E" names
    ~error_msg:"no %L among the enum names";
  Check.list_not_mem Check.string "My_struct.My_enum.E" names
    ~error_msg:"found %L";
  unit

(* A module only an output's type comes from is still imported. *)
let output_module_deps () =
  register ~__FILE__ ~title:"a module only an output uses is imported"
    ~tags:["generate"]
  @@ fun () ->
  let dir = project "output_module_deps" in
  let* () = start dir in
  let* tests = generate dir "my_scope.catala_en" "S" in
  let* text = write dir tests in
  List.iter
    (fun m ->
      if not (contains text ("> Using " ^ m)) then
        Test.fail "%s is not imported" m)
    ["Output_types"; "Input_types"];
  unit

let register () =
  enum_path ();
  output_module_deps ()
