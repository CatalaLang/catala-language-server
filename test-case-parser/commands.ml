(* This file is part of the Catala project. Copyright (C) 2024 Inria.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Shared_ast
module I = Desugared.Ast
module O = Catala_types_t
module J = Catala_types_j
open Project
open Model

let get_scope_test
    (prg : I.program)
    (testing_scope : string)
    (tested_scope : ScopeName.t)
    ~tested_module : O.test =
  let tested_module =
    match tested_module with
    | None ->
      Format.ksprintf failwith "Tested scope %s is not part of a Catala module"
        (ScopeName.to_string tested_scope)
    | Some x -> x
  in
  let tested_scope =
    let modul =
      List.fold_left
        (fun _ m -> ModuleName.Map.find m prg.program_modules)
        prg.program_root
        (ScopeName.path tested_scope)
    in
    get_scope_def prg
      (ScopeName.Map.find tested_scope modul.module_scopes)
      ~tested_module
  in
  let test_inputs =
    List.map
      (fun (v, (si : O.scope_input)) ->
        let default =
          if si.is_context then context_var_default
          else unset_default_value si.typ
        in
        v, { O.typ = si.typ; value = Some default })
      tested_scope.inputs
  in
  let test_outputs =
    List.map (fun (v, typ) -> v, { O.typ; value = None }) tested_scope.outputs
  in
  let description = "" in
  let title = tested_scope.name in
  {
    O.testing_scope;
    tested_scope;
    test_outputs;
    test_inputs;
    description;
    title;
    variables = [];
  }

(* --- *)

let write_stdout f arg =
  let buf = Buffer.create 4096 in
  f buf arg;
  Buffer.output_buffer stdout buf

let print_test test = write_stdout J.write_test test
let print_tests test = write_stdout J.write_test_list test

let read_program includes path_to_build options =
  let stdlib =
    Some (Global.raw_file File.(path_to_build / "_build" / "libcatala"))
  in
  let prg, ctx = Driver.Passes.desugared options ~stdlib ~includes in
  let prg = Desugared.Disambiguate.program prg in
  prg, ctx

let rec generate_default_value lang (typ : O.typ) : O.runtime_value =
  let value =
    match typ with
    | TBool -> O.Bool false
    | TInt -> O.Integer 0
    | TRat -> O.Decimal 0.
    | TMoney -> O.Money 0
    | TDate -> O.Date { year = 2000; month = 1; day = 1 }
    | TDuration -> O.Duration { years = 0; months = 0; days = 0 }
    | TTuple l ->
      O.Array (List.map (generate_default_value lang) l |> Array.of_list)
    | TStruct decl ->
      O.Struct
        ( decl,
          List.map
            (fun (s, t) -> s, (generate_default_value lang) t)
            decl.fields )
    | TEnum decl ->
      let elt =
        let cn, ty =
          List.find_opt
            (function _, None -> true | _ -> false)
            decl.constructors
          |> function Some s -> s | None -> List.hd decl.constructors
        in
        cn, Option.map (generate_default_value lang) ty
      in
      O.Enum (decl, elt)
    | TOption typ -> mk_absent typ
    | TArray _ -> O.Array [||]
    | TUnset -> O.Unset
    | TUnit -> raise (Unsupported "unit type")
    | TArrow _ -> raise (Unsupported "arrow type")
  in
  { value; attrs = [] }

let patch_paths
    (modl : ModuleName.t)
    ({ tested_scope; test_inputs; test_outputs; _ } as test : O.test) =
  let open O in
  let patch_name s =
    if String.contains s '.' || s = EnumName.to_string ConstantNames.option_enum
    then s
    else Format.sprintf "%s.%s" (ModuleName.to_string modl) s
  in
  let rec patch_enum_decl = function
    | { O.enum_name; constructors; ctor_attrs } ->
      {
        enum_name = patch_name enum_name;
        constructors =
          List.map (fun (c, t) -> c, Option.map patch_typ t) constructors;
        ctor_attrs;
      }
  and patch_struct_decl = function
    | { struct_name; fields } ->
      {
        struct_name = patch_name struct_name;
        fields = List.map (fun (fl, t) -> fl, patch_typ t) fields;
      }
  and patch_typ : O.typ -> O.typ =
   fun t ->
    match t with
    | TBool | TInt | TRat | TMoney | TDate | TDuration | TUnit | TUnset -> t
    | TTuple l -> TTuple (List.map patch_typ l)
    | TStruct sdecl -> TStruct (patch_struct_decl sdecl)
    | TEnum edecl -> TEnum (patch_enum_decl edecl)
    | TOption t -> TOption (patch_typ t)
    | TArray t -> TArray (patch_typ t)
    | TArrow (tl, t) -> TArrow (List.map patch_typ tl, patch_typ t)
  in
  let rec patch_value : O.runtime_value -> O.runtime_value =
   fun ({ value; attrs } as v) ->
    match value with
    | O.Bool _ | O.Money _ | O.Integer _ | O.Decimal _ | O.Date _ | O.Duration _
    | O.Empty | O.Unset | O.NotOverridden ->
      v
    | O.Enum (enum_decl, (cstr, rv_opt)) ->
      {
        value =
          O.Enum
            (patch_enum_decl enum_decl, (cstr, Option.map patch_value rv_opt));
        attrs;
      }
    | O.Struct (struct_decl, fields) ->
      {
        value =
          O.Struct
            ( patch_struct_decl struct_decl,
              List.map (fun (fl, v) -> fl, patch_value v) fields );
        attrs;
      }
    | O.Array t -> { value = O.Array (Array.map patch_value t); attrs }
  in
  let patch_value_def (x : O.value_def) =
    { x with value = patch_value x.value }
  in
  let patch_test_io : O.test_io -> O.test_io =
   fun { typ; value } ->
    { typ = patch_typ typ; value = Option.map patch_value_def value }
  in
  let tested_scope =
    {
      tested_scope with
      inputs =
        List.map
          (fun (x, (si : O.scope_input)) ->
            x, { si with typ = patch_typ si.typ })
          tested_scope.inputs;
      outputs = List.map (fun (x, t) -> x, patch_typ t) tested_scope.outputs;
    }
  in
  let test_inputs = List.map (fun (x, io) -> x, patch_test_io io) test_inputs in
  let test_outputs =
    List.map (fun (x, io) -> x, patch_test_io io) test_outputs
  in
  { test with tested_scope; test_inputs; test_outputs }

let generate_test
    tested_scope
    ?(enforce_module = true)
    ?(testing_scope = tested_scope ^ "_test")
    ?(with_default_values = false)
    ?program
    include_dirs
    options =
  (* [program]: a caller that already read the module hands it over. *)
  let prg, _ =
    match program with
    | Some p -> p
    | None ->
      let path_to_build, include_dirs =
        if include_dirs = [] then lookup_include_dirs options
        else ".", include_dirs
      in
      read_program include_dirs path_to_build options
  in
  let tested_scope =
    Ident.Map.find tested_scope prg.I.program_ctx.ctx_scope_index
  in
  let tested_module =
    if enforce_module then Option.map fst prg.I.program_module_name
    else
      Option.map fst prg.I.program_module_name
      |> function
      | None -> Some (ModuleName.fresh ("no_module", Pos.void))
      | Some m -> Some m
  in
  let test = get_scope_test prg testing_scope tested_scope ~tested_module in
  let test =
    (* As our root module is not the test file but the scope's file (which is
       not the case for read), qualified name do not have the expected module
       set. We patch types to retroactively add it so that the test structure is
       fully operational. *)
    Option.map (fun modl -> patch_paths modl test) tested_module
    |> Option.value ~default:test
  in
  if with_default_values then
    let test_inputs =
      List.map
        (fun (s, (io : O.test_io)) ->
          let is_context =
            List.assoc_opt s test.tested_scope.inputs
            |> Option.map (fun (si : O.scope_input) -> si.is_context)
            |> Option.value ~default:false
          in
          ( s,
            O.
              {
                io with
                value =
                  Some
                    (if is_context then context_var_default
                     else
                       {
                         value = generate_default_value prg.program_lang io.typ;
                         pos = None;
                       });
              } ))
        test.test_inputs
    in
    { test with test_inputs }
  else test

let generate_cmd
    tested_scope
    ?testing_scope
    include_dirs
    options
    with_default_values
    enforce_module =
  print_tests
    [
      generate_test ~with_default_values tested_scope ?testing_scope
        include_dirs options ~enforce_module;
    ]

exception InvalidTestingScope of string

let invalid_testing_scope fmt =
  Format.kasprintf (fun msg -> raise (InvalidTestingScope msg)) fmt

(* note: filters for both 'test' and 'testUI' attrs *)
let get_test_scopes prg =
  prg.I.program_root.module_scopes
  |> ScopeName.Map.filter (fun scope_name _scope ->
      Pos.has_attr (Mark.get (ScopeName.get_info scope_name)) Test
      && Pos.has_attr (Mark.get (ScopeName.get_info scope_name)) TestUi)
  |> ScopeName.Map.keys

(* `#[test]` without `#[testcase.testui]`: hand-written, may hold anything. The
   editor can only express literals and equalities, so writing such a file back
   would delete it. Must agree with [surface_scopes_by_ownership]. *)
let unowned_test_scopes prg =
  prg.I.program_root.module_scopes
  |> ScopeName.Map.filter (fun scope_name _scope ->
      Pos.has_attr (Mark.get (ScopeName.get_info scope_name)) Test
      && not (Pos.has_attr (Mark.get (ScopeName.get_info scope_name)) TestUi))
  |> ScopeName.Map.keys

(* Raw newlines in a pre-joined string escape the message box. *)
let error_mixed_ownership (unowned : string list) =
  Message.error
    "this file mixes tests the editor owns with tests written by hand:@\n\
     @[<v 2>  %a@]@\n\
     The editor can express only literal values and equalities, so reading it \
     would drop whatever else those tests contain, and writing it back would \
     delete them. Mark them `#[testcase.testui]` to hand them over, or keep \
     them in a file of their own."
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut Format.pp_print_string)
    unowned

let parse_expected_variable (s : string) :
    (string * O.runtime_value option) option =
  match String.index_opt s ':' with
  | None -> Some (String.trim s, None)
  | Some i ->
    let name = String.trim (String.sub s 0 i) in
    let value = String.trim (String.sub s (i + 1) (String.length s - i - 1)) in
    if value = "" then Some (name, None)
    else Some (name, Some (Model.runtime_value_of_string value))

let get_catala_test (prg, naming_ctx) testing_scope_name =
  let testing_scope =
    ScopeName.Map.find testing_scope_name prg.I.program_root.module_scopes
  in
  let info = Mark.get (ScopeName.get_info testing_scope_name) in
  let get_single_attr ~default pos f =
    match Pos.get_attrs pos f with [] -> default | x :: _ -> x
  in
  let description =
    get_single_attr ~default:"" info (function
      | TestDescription s -> Some s
      | _ -> None)
  in
  let title =
    get_single_attr ~default:"" info (function
      | TestTitle s -> Some s
      | _ -> None)
  in
  let subscope_var, tested_scope =
    let count = ScopeVar.Map.cardinal testing_scope.I.scope_sub_scopes in
    if count <> 1 then
      invalid_testing_scope
        "@{<b>%a@}: testing scopes are expected to have one, and only one \
         subscope, this has %d"
        ScopeName.format testing_scope_name count
    else ScopeVar.Map.choose testing_scope.scope_sub_scopes
  in
  let tested_id_var_map =
    Ident.Map.filter_map
      (fun _ -> function ScopeVar v -> Some v | SubScope _ -> None)
      (ScopeName.Map.find tested_scope
         naming_ctx.Desugared.Name_resolution.scopes)
        .var_idmap
  in
  let tested_module =
    match ScopeName.path tested_scope with
    | [] ->
      Message.error
        "%a tests %a, which names no module: qualify it as Module.%a"
        ScopeName.format testing_scope_name ScopeName.format tested_scope
        ScopeName.format tested_scope
    | m :: _ -> Some m
  in
  let base_test =
    get_scope_test ~tested_module prg
      (ScopeName.to_string testing_scope_name)
      tested_scope
  in
  let test_inputs =
    List.map
      (fun (var_str, (test_in : O.test_io)) ->
        let var_within_origin_scope =
          Ident.Map.find var_str tested_id_var_map
        in
        let value =
          let rules =
            try
              let def_key =
                ( (subscope_var, Pos.void),
                  I.ScopeDef.SubScopeInput
                    { name = tested_scope; var_within_origin_scope } )
              in
              let def = I.ScopeDef.Map.find def_key testing_scope.scope_defs in
              RuleName.Map.bindings def.scope_def_rules
            with Ident.Map.Not_found _ | I.ScopeDef.Map.Not_found _ -> []
          in
          let is_context =
            List.assoc_opt var_str base_test.tested_scope.inputs
            |> Option.map (fun (si : O.scope_input) -> si.is_context)
            |> Option.value ~default:false
          in
          match rules with
          | [] ->
            Some
              (if is_context then context_var_default
               else unset_default_value test_in.O.typ)
          | [(_, rule)] ->
            let e = Expr.unbox_closed rule.rule_cons in
            let value = get_value prg.program_lang prg.program_ctx e in
            Some { O.value; pos = Some (get_source_position (Expr.pos e)) }
          | rules ->
            let extra_pos =
              List.map (fun (r, _) -> "", Mark.get (RuleName.get_info r)) rules
            in
            Message.error ~extra_pos
              "Multiple definitions of test input value in test scope %a.%a!;@ \
               %d rule(s) found: [%a]"
              ScopeName.format testing_scope_name ScopeVar.format
              var_within_origin_scope (List.length rules)
              (Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
                 (fun ppf (r, _) -> RuleName.format ppf r))
              rules
        in
        var_str, { test_in with O.value })
      base_test.test_inputs
  in
  let test_outputs =
    let scope_info =
      ScopeName.Map.find tested_scope prg.program_ctx.ctx_scopes
    in
    let scope_field_map =
      ScopeVar.Map.fold
        (fun var field acc -> StructField.Map.add field var acc)
        scope_info.out_struct_fields StructField.Map.empty
    in
    let assertion_values =
      I.AssertionName.Map.fold
        (fun _ e acc ->
          match Expr.unbox_closed e with
          | ( EAppOp
                {
                  op = Op.Eq, _;
                  args =
                    [
                      ( EStructAccess
                          {
                            field;
                            e =
                              ( ELocation
                                  (DesugaredScopeVar { name = svar, pos; _ }),
                                _ );
                            _;
                          },
                        _ );
                      value;
                    ];
                  _;
                },
              _ )
            when svar = subscope_var ->
            let scope_var = StructField.Map.find field scope_field_map in
            (* A rich-assertion editor (foo > 30 and foo < 50) would revisit
               this. *)
            if ScopeVar.Map.mem scope_var acc then
              Message.error ~pos
                "%a is asserted twice; keep one assertion per output."
                ScopeVar.format scope_var;
            ScopeVar.Map.add scope_var
              {
                O.value = get_value prg.program_lang prg.program_ctx value;
                pos = Some (get_source_position pos);
              }
              acc
          | ( EAppOp
                {
                  op = Op.Eq, _;
                  args = [((EStructAccess _, _) as e); _value];
                  _;
                },
              m ) ->
            Message.error ~pos:(Expr.mark_pos m)
              "Could not read test assertion: %a" Expr.format e
          | (_, m) as e ->
            Message.error ~pos:(Expr.mark_pos m)
              "Could not read test assertion: %a" Expr.format e)
        testing_scope.scope_assertions ScopeVar.Map.empty
    in
    List.map
      (fun (var_str, test_out) ->
        let var = Ident.Map.find var_str tested_id_var_map in
        let value = ScopeVar.Map.find_opt var assertion_values in
        var_str, { test_out with O.value })
      base_test.test_outputs
  in
  let variables =
    Pos.get_attrs info (function
      | ExpectedVariable s -> parse_expected_variable s
      | _ -> None)
  in
  { base_test with O.test_inputs; test_outputs; description; title; variables }

let import_catala_tests (prg, naming_ctx) =
  List.map (get_catala_test (prg, naming_ctx)) (get_test_scopes prg)

let read_test include_dirs (options : Global.options) buffer_path scope_filter =
  let path_to_build, include_dirs =
    if include_dirs = [] then lookup_include_dirs ?buffer_path options
    else ".", include_dirs
  in
  let prg = read_program include_dirs path_to_build options in
  (match unowned_test_scopes (fst prg), get_test_scopes (fst prg) with
  | (_ :: _ as unowned), _ :: _ ->
    error_mixed_ownership (List.map ScopeName.to_string unowned)
  | _ -> ());
  let tests = import_catala_tests prg in
  let tests =
    match scope_filter with
    | None -> tests
    | Some scope ->
      List.filter (fun (t : O.test) -> t.O.testing_scope = scope) tests
  in
  match check_tests_fit tests with
  | Ok () -> write_stdout J.write_test_list tests
  | Error problems ->
    Message.error
      "this test no longer fits the scope it targets:@\n\
       @[<v 2>  %a@]@\n\
       Its values were written against a different signature. Use@ `catala \
       testcase partial-read` to read them as authored."
      (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf s ->
           Format.pp_print_string ppf s))
      problems

(* [working_copy_ext] hides the language, hence [~lang]. *)
let read_tests_of_file ~(lang : Global.backend_lang) (file : string) :
    (O.test list option, string) result =
  if not (Sys.file_exists file) then Ok None
  else
    match
      let options =
        Global.enforce_options ~input_src:(Global.FileName file)
          ~language:(Some lang) ()
      in
      let path_to_build, include_dirs = lookup_include_dirs options in
      import_catala_tests (read_program include_dirs path_to_build options)
    with
    | tests -> Ok (Some tests)
    | exception e -> Error (error_text e)

(* The tested scope's assertions only: another test's expectations on the same
   field names are not this test's. *)
let retrieve_assertions_values
    (dcalc_prg : typed Dcalc.Ast.program)
    (scope : ScopeName.t) : (StructField.t * (dcalc, typed) gexpr) list =
  let get_expected_value (assert_e : (dcalc, typed) gexpr) =
    match Mark.remove assert_e with
    | EAssert (EAppOp { args = [(EStructAccess { field; _ }, _); v]; _ }, _) ->
      field, v
    | _ -> assert false
  in
  let code_items = dcalc_prg.code_items |> BoundList.to_seq |> List.of_seq in
  List.fold_left
    (fun acc -> function
      | _, Topdef _ -> acc
      | _, ScopeDef (name, _) when not (ScopeName.equal name scope) -> acc
      | _, ScopeDef (_, body) ->
        let _, body_list = Bindlib.unbind body.scope_body_expr in
        let scope_lets : (dcalc, typed) gexpr scope_let list =
          body_list |> BoundList.to_seq |> List.of_seq |> List.map snd
        in
        List.filter_map
          (function
            | { scope_let_kind = Assertion; scope_let_expr; _ } ->
              Some (get_expected_value scope_let_expr)
            | _ -> None)
          scope_lets)
    [] code_items

let retrieve_program include_dirs options scope_name =
  let path_to_build, include_dirs =
    if include_dirs = [] then
      let _path_to_build, include_dirs = lookup_include_dirs options in
      let path_to_build, build_include_dirs =
        lookup_include_dirs ~prefix_build:true options
      in
      path_to_build, build_include_dirs @ include_dirs
    else ".", []
  in
  let desugared_prg, naming_ctx =
    read_program include_dirs path_to_build options
  in
  let testing_scope_name =
    match
      Ident.Map.find_opt scope_name
        Desugared.Name_resolution.(naming_ctx.local.typedefs)
    with
    | Some (TScope (sname, _)) -> sname
    | _ -> Message.error "No scope %S was found in the program" scope_name
  in
  let dcalc_prg : ((dcalc, dcalc, typed) base_gexpr * typed mark) program =
    let prg =
      Scopelang.From_desugared.(
        translate_program desugared_prg (build_exceptions_graph desugared_prg))
    in
    let prg = Scopelang.Ast.type_program prg in
    Dcalc.From_scopelang.translate_program prg
  in
  desugared_prg, naming_ctx, testing_scope_name, dcalc_prg

let rec convert_atd_to_runtime_value : O.runtime_value -> Catala_runtime.Value.t
    =
 fun v ->
  let open Catala_runtime in
  let open Value in
  match v.value with
  | O.Bool b -> V (Bool, b)
  | Money m -> V (Money, Z.of_int m)
  | Integer i -> V (Integer, Z.of_int i)
  | Decimal d -> V (Decimal, Q.of_float d)
  | Date { year; month; day } -> V (Date, Dates_calc.make_date ~year ~month ~day)
  | Duration { years; months; days } ->
    V (Duration, Dates_calc.make_period ~years ~months ~days)
  | Enum (decl, (cstr_s, v_opt)) ->
    let v = Option.map convert_atd_to_runtime_value v_opt in
    let index =
      List.mapi (fun i (name, _) -> name, i) decl.constructors
      |> List.assoc cstr_s
    in
    V
      ( Enum { name = decl.enum_name; constr = (fun _ -> index, cstr_s, v) },
        (decl.enum_name, (cstr_s, v)) )
  | Struct (decl, fvl) ->
    let l = List.map (fun (s, v) -> s, convert_atd_to_runtime_value v) fvl in
    let ty = Struct { name = decl.struct_name; fields = (fun _ -> l) } in
    V (ty, (decl.struct_name, l))
  | Array l ->
    let l = Array.map convert_atd_to_runtime_value l in
    V (Array Fun.id, l)
  | Unset -> failwith "Cannot convert 'Unset' atd value to Catala runtime value"
  | NotOverridden ->
    failwith "Cannot convert 'NotOverridden' atd value to Catala runtime value"
  | Empty -> failwith "Cannot convert 'Empty' atd value to Catala runtime value"

let interpret_program dcalc_prg scope_name build_term_to_interp =
  Interpreter.load_runtime_modules
    ~hashf:Hash.(finalise ~monomorphize_types:false)
    (dcalc_prg : typed Dcalc.Ast.program);
  Message.report_delayed_errors_if_any ();
  let failed_asserts = ref [] in
  let on_assert_failures e =
    match e with
    | { Message.kind = AssertFailure; _ } ->
      failed_asserts := e :: !failed_asserts;
      false (* absorb error *)
    | _ -> true (* propagate error and crash *)
  in
  let () =
    Catala_utils.Message.register_lsp_error_absorber on_assert_failures
  in
  let program_fun =
    Expr.unbox (Program.to_expr dcalc_prg scope_name)
    |> Interpreter.evaluate_expr dcalc_prg.decl_ctx dcalc_prg.lang
  in
  let to_interp = build_term_to_interp program_fun in
  let results =
    Interpreter.evaluate_expr dcalc_prg.decl_ctx dcalc_prg.lang to_interp
  in
  Message.report_delayed_errors_if_any ();
  results, !failed_asserts

(* A run's results, as the test's outputs typed by the output struct. *)
let test_outputs_of dcalc_prg out_struct actual_results =
  List.map
    (fun (field, value_expr) ->
      let pos = Some (get_source_position (Expr.pos value_expr)) in
      ( StructField.to_string field,
        {
          O.value =
            Some
              {
                value = get_value dcalc_prg.lang dcalc_prg.decl_ctx value_expr;
                pos;
              };
          typ =
            get_typ dcalc_prg.lang dcalc_prg.decl_ctx
              (StructField.Map.find field out_struct);
        } ))
    actual_results

let rec convert_to_json_input ({ value; _ } : O.runtime_value) : Yojson.Safe.t =
  let open O in
  let convert_runtime_raw = function
    | Bool b -> `Bool b
    | Money i -> `String (string_of_float (float i /. 100.))
    | Integer i -> `String (string_of_int i)
    | Decimal f -> `String (string_of_float f)
    | Date { year; month; day } ->
      `String (Format.sprintf "%04d-%02d-%02d" year month day)
    | Duration { years; months; days } ->
      `Assoc ["years", `Int years; "months", `Int months; "days", `Int days]
    | Enum (_decl, (c, None)) when c = option_absent -> `Null
    | Enum (_decl, (c, Some x)) when c = option_present ->
      convert_to_json_input x
    | Enum (_decl, (constr, None)) -> `String constr
    | Enum (_decl, (constr, Some v)) -> `Assoc [constr, convert_to_json_input v]
    | Struct (_decl, fl) ->
      `Assoc
        (List.filter_map
           (function
             | _, ({ value = Enum (_decl, (c, None)); _ } : O.runtime_value)
               when c = option_absent ->
               None
             | fname, v -> Some (fname, convert_to_json_input v))
           fl)
    | Array l -> `List (Array.to_list l |> List.map convert_to_json_input)
    | Unset -> failwith "convert_to_json_input: cannot convert 'unset' values"
    | NotOverridden ->
      failwith "convert_to_json_input: cannot convert 'NotOverridden' values"
    | Empty -> failwith "convert_to_json_input: cannot convert 'empty' values"
  in
  convert_runtime_raw value

let run_with_inputs
    include_dirs
    options
    tested_scope_name
    (scope_input : Yojson.Safe.t) =
  let desugared_prg, _naming_ctx, scope_name, dcalc_prg =
    retrieve_program include_dirs options tested_scope_name
  in
  let test =
    get_scope_test desugared_prg "<abstract>" scope_name
      ~tested_module:(Some (ModuleName.fresh ("abstract", Pos.void)))
  in
  let input_expr =
    let in_struct =
      (ScopeName.Map.find scope_name dcalc_prg.decl_ctx.ctx_scopes)
        .in_struct_name
    in
    let ty = TStruct in_struct, Pos.void in
    let atd_test_inputs : O.runtime_value =
      Lexing.from_string (Yojson.Safe.to_string scope_input)
      |> J.read_test_inputs (Yojson.init_lexer ())
      |> fun fields ->
      {
        O.attrs = [];
        value =
          O.Struct
            ( (* Dummy declaration *)
              { O.struct_name = StructName.to_string in_struct; fields = [] },
              List.filter_map
                (fun (field_name, { O.value; typ = _ }) ->
                  let rv = (Option.get value).value in
                  match rv.O.value with
                  | O.NotOverridden -> None
                  | O.Unset ->
                    failwith
                      (Printf.sprintf
                         "run_with_inputs: input '%s' has Unset value"
                         field_name)
                  | _ -> Some (field_name, rv))
                fields );
      }
    in
    let encoding = Encoding.make_encoding dcalc_prg.decl_ctx ty in
    let module JsonE = Json_encoding.Make (Json_repr.Yojson) in
    let rval =
      JsonE.destruct encoding (convert_to_json_input atd_test_inputs)
    in
    Encoding.convert_to_dcalc dcalc_prg.decl_ctx
      (Typed { pos = Pos.void; ty })
      ty rval
    |> Expr.unbox
    |> Interpreter.addcustom
    |> Expr.box
  in
  let build_term program_fun =
    Expr.make_app (Expr.box program_fun) [input_expr]
      [Expr.ty input_expr]
      (Expr.pos program_fun)
    |> Expr.unbox
  in
  let result_struct, failed_asserts =
    interpret_program dcalc_prg scope_name build_term
  in
  let (actual_results : (StructField.t * (dcalc, typed) gexpr) list), out_struct
      =
    match result_struct with
    | EStruct { fields; name }, _ ->
      let b = StructField.Map.bindings fields in
      ( List.map (fun (f, e) -> f, Interpreter.delcustom e) b,
        StructName.Map.find name dcalc_prg.decl_ctx.ctx_structs )
    | _ -> assert false
  in
  let test_outputs = test_outputs_of dcalc_prg out_struct actual_results in
  let assert_failures = not (failed_asserts = []) in
  let test = O.{ test with test_outputs } in
  write_stdout J.write_test_run O.{ test; assert_failures; diffs = [] }

let run_test include_dirs options testing_scope =
  let desugared_prg, naming_ctx, testing_scope_name, dcalc_prg =
    retrieve_program include_dirs options testing_scope
  in
  let test = get_catala_test (desugared_prg, naming_ctx) testing_scope_name in
  let build_term program_fun =
    let _args, program_expr =
      match program_fun with
      | EAbs { binder; _ }, _ -> Bindlib.unmbind binder
      | _ -> assert false
    in
    program_expr
  in
  let result_struct, failed_asserts =
    interpret_program dcalc_prg testing_scope_name build_term
  in
  let (actual_results : (StructField.t * (dcalc, typed) gexpr) list), out_struct
      =
    match result_struct with
    | EStruct { fields; _ }, _ -> (
      match StructField.Map.choose fields with
      | _, (EStruct { fields; name }, _) ->
        let b = StructField.Map.bindings fields in
        ( List.map (fun (f, e) -> f, Interpreter.delcustom e) b,
          StructName.Map.find name dcalc_prg.decl_ctx.ctx_structs )
      | _ -> assert false)
    | _ -> assert false
  in
  let test_outputs = test_outputs_of dcalc_prg out_struct actual_results in
  let test = O.{ test with test_outputs } in
  let expected_results =
    retrieve_assertions_values dcalc_prg testing_scope_name
  in
  let diffs =
    compute_diff expected_results actual_results
    |> List.map (proj_diff (get_value dcalc_prg.lang dcalc_prg.decl_ctx))
  in
  let assert_failures = not (failed_asserts = []) in
  let test_run = { O.test; O.assert_failures; O.diffs } in
  write_stdout J.write_test_run test_run

let clerk_exe () =
  match Sys.getenv_opt "CATALA_CLERK_PATH" with
  | Some p when p <> "" -> p
  | _ ->
    let beside =
      Filename.concat
        (Filename.dirname Sys.executable_name)
        (if Sys.win32 then "clerk.exe" else "clerk")
    in
    if Sys.file_exists beside then beside else Filename.basename beside

(* No shell: the child inherits the cwd, so it is set around the spawn. Both
   streams go through one pipe, in order. *)
let run_process ~cwd exe args =
  let r, w = Unix.pipe ~cloexec:true () in
  let ic = Unix.in_channel_of_descr r in
  let here = Sys.getcwd () in
  let closed = ref false in
  let close_w () =
    if not !closed then (
      closed := true;
      Unix.close w)
  in
  Fun.protect
    ~finally:(fun () ->
      close_w ();
      close_in_noerr ic)
    (fun () ->
      Sys.chdir cwd;
      let pid =
        Fun.protect
          ~finally:(fun () -> Sys.chdir here)
          (fun () ->
            Unix.create_process exe (Array.of_list (exe :: args)) Unix.stdin w w)
      in
      close_w ();
      let output = In_channel.input_all ic in
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED n -> n, output
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
        1, output ^ "\n(killed by a signal)")

(* Fatal: stale objects fail the interpreter on any interface change. Clerk
   prints compiler errors only in debug mode, hence the hint. *)
let run_clerk ~root args =
  let exe = clerk_exe () in
  let status, output =
    try run_process ~cwd:root exe args
    with Unix.Unix_error (e, _, _) -> 127, exe ^ ": " ^ Unix.error_message e
  in
  if status <> 0 then
    Message.error
      "Could not prepare the modules: clerk %s exited with %d in %s@\n\
       Run 'clerk build -d' there to see the compiler errors.@\n\
       %s"
      (String.concat " " args) status root output

(* One clerk run for all of them: each spawn re-reads the whole project. *)
let prepare_runtime_plugins_of (files : string list) =
  match List.map File.make_absolute files with
  | [] -> ()
  | first :: _ as files -> (
    match find_project_root (Filename.dirname first) with
    | None -> ()
    | Some root -> run_clerk ~root ("run" :: "--prepare-only" :: files))

let prepare_runtime_plugins (file : string) = prepare_runtime_plugins_of [file]

let build_runtime_plugins ?buffer_path (options : Global.options) =
  let file =
    let f = Global.input_src_file options.Global.input_src in
    if Sys.file_exists f then Some f
    else
      match buffer_path with
      | Some b when Sys.file_exists b -> Some b
      | _ -> None
  in
  match file with
  | Some f -> prepare_runtime_plugins f
  | None ->
    (* Nothing to point at: the declared targets are the best guess left. *)
    if Sys.file_exists "clerk.toml" then run_clerk ~root:"." ["build"]

(* [Contents] at [buffer_path] anchors module resolution to the file. *)
let run_test_cmd
    include_dirs
    options
    test_scope_name
    scope_input_opt
    buffer_path =
  let options =
    match options.Global.input_src, buffer_path with
    | Global.Stdin _, Some b ->
      let text = In_channel.input_all stdin in
      let options =
        Global.enforce_options ~input_src:(Global.Contents (text, b)) ()
      in
      (match Driver.Passes.surface options with
      | prg ->
        prepare_runtime_plugins_of
          (List.filter_map
             (fun (u : Surface.Ast.module_use) ->
               find_module_file
                 (Mark.remove u.mod_use_name)
                 (Filename.dirname b))
             prg.Surface.Ast.program_used_modules)
      | exception _ -> ());
      options
    | _ ->
      build_runtime_plugins ?buffer_path options;
      options
  in
  match scope_input_opt with
  | None -> run_test include_dirs options test_scope_name
  | Some json -> run_with_inputs include_dirs options test_scope_name json

let print_scopes scopes = write_stdout J.write_scope_def_list scopes

let list_scopes include_dirs options =
  let path_to_build, include_dirs =
    if include_dirs = [] then lookup_include_dirs options else ".", include_dirs
  in
  let prg, _ = read_program include_dirs path_to_build options in
  let module_name =
    match prg.program_module_name with
    | None -> failwith "Expected a Catala module"
    | Some (mn, _) -> mn
  in
  let modul = prg.program_root in
  let filtered_scopes =
    ScopeName.Map.filter_map
      (fun _sn -> function
        | { I.scope_visibility = Private; _ } -> None
        | sc -> (
          if scope_inputs prg.program_lang prg.program_ctx sc = [] then
            (* We do not consider no-input scopes *)
            None
          else
            try Some (get_scope_def prg sc ~tested_module:module_name)
            with _ -> None))
      modul.module_scopes
    |> ScopeName.Map.bindings
    |> List.map snd
  in
  print_scopes filtered_scopes

let serialize_inputs (scope_input : Yojson.Safe.t option) =
  let scope_input =
    match scope_input with
    | None -> failwith "serialize-inputs requires --input"
    | Some i -> i
  in
  Lexing.from_string (Yojson.Safe.to_string scope_input)
  |> J.read_test_inputs (Yojson.init_lexer ())
  |> function
  | fields ->
    let dummy_decl = { O.struct_name = "dummy"; fields = [] } in
    let value =
      O.Struct
        ( dummy_decl,
          List.filter_map
            (fun (field_name, { J.value; typ = _ }) ->
              let rv = (Option.get value).value in
              match rv.O.value with
              | O.NotOverridden -> None
              | O.Unset ->
                failwith
                  (Printf.sprintf "serialize_inputs: input '%s' has Unset value"
                     field_name)
              | _ -> Some (field_name, rv))
            fields )
    in
    let json = convert_to_json_input { value; attrs = [] } in
    Format.(
      fprintf std_formatter "%a@." (Yojson.Safe.pretty_print ~std:true) json)
