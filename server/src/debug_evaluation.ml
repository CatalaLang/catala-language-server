(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
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

open Catala_utils
open Shared_ast
open Lwt.Syntax
open Server_types
open Debug_interpret
open Debug_protocol

type dcalc_expr = ((yes, yes) interpr_kind, typed) gexpr
type dcalc_env = (yes, typed) env

module PMap = Position_map.Make (struct
  type t = dcalc_expr

  let compare = Expr.compare
  let format = Expr.format
end)

let get_pos (_, Typed { pos; _ }) = pos
let get_typ (_, Typed { ty; _ }) = ty

type paused_state = { e : dcalc_expr; env : (yes, typed) env }

type interp_exn =
  | Runtime of
      Catala_runtime.error * Catala_runtime.code_location list * string option
  | Internal of (Format.formatter -> unit)

type value = Expr of dcalc_expr | Exn of { pos : Pos.t; exn : interp_exn }
type step = { value : value; env : dcalc_env; mutable breakpoint : bool }

let get_step_pos = function
  | { value = Expr e; _ } -> get_pos e
  | { value = Exn { pos; _ }; _ } -> pos

type debugger_state = {
  rpc : Debug_rpc.t;
  logger : string -> unit Lwt.t;
  file : Doc_id.t;
  scope : ScopeName.t;
  program : typed Dcalc.Ast.program;
  pmap : PMap.t;
  all_steps : step array;
  mutable index : int;
  final_index : int;
  steps_table : step list Pos.Map.t;
  mutable breakpoints : (Breakpoint.t * step list) Pos.Map.t;
  result : (dcalc_expr, exn) Result.t;
}

let should_stop (e : dcalc_expr) =
  match Mark.remove e with
  | ELit LUnit -> false
  | EAbs _ -> false
  | EAssert (EAppOp _, _) -> false
  | ELit _ -> true
  | EApp _ -> true
  | EAppOp _ -> true
  | EArray _ -> true
  | EVar _ -> true
  | EIfThenElse _ -> true
  | EStruct _ -> true
  | EInj _ -> true
  | EMatch _ -> true
  | ETuple _ -> true
  | ETupleAccess _ -> true
  | EStructAccess _ -> true
  | EExternal _ -> true
  | EAssert _ -> true
  | EFatalError _ -> true
  | EPos _ -> true
  | EDefault _ -> true
  | EPureDefault _ -> true
  | EEmpty -> true
  | EErrorOnEmpty _ -> true
  | ECustom _ -> true
  | EBad -> true
  | ELocation _ -> .

let run_debugger
    ?inputs
    (rpc : Debug_rpc.t)
    (logger : string -> unit Lwt.t)
    (file : Doc_id.t)
    (scope : ScopeName.t)
    program
    pmap : debugger_state Lwt.t =
  let scope_pos = Mark.get (ScopeName.get_info scope) in
  let first_step : step =
    let expr =
      Mark.add
        (Typed { pos = scope_pos; ty = Mark.add Pos.void (TLit TUnit) })
        EEmpty
    in
    { value = Expr expr; env = Env Var.Map.empty; breakpoint = false }
  in
  let last_step = ref first_step in
  let is_equivalent step =
    let last_pos = get_step_pos !last_step in
    let pos = get_step_pos step in
    Pos.equal last_pos pos && step.env == step.env
  in
  let rev_steps = ref [first_step] in
  let steps_table_r = ref Pos.Map.empty in
  let on_expr expr env =
    let pos = get_pos expr in
    let step = { value = Expr expr; env; breakpoint = false } in
    if (not (should_stop expr)) || is_equivalent step then Lwt.return_unit
    else begin
      steps_table_r :=
        Pos.Map.update pos
          (function None -> Some [step] | Some l -> Some (step :: l))
          !steps_table_r;
      rev_steps := step :: !rev_steps;
      last_step := step;
      Lwt.return_unit
    end
  in
  let* result =
    Lwt.catch
      (fun () ->
        let* r =
          Debug_interpret.interpret_with_env ?inputs ~on_expr program scope
        in
        Catala_utils.Message.report_delayed_errors_if_any ();
        Lwt.return_ok r)
      (fun exn -> Lwt.return_error exn)
  in
  let* last_step_l =
    match result with
    | Error (Catala_utils.Message.CompilerError content) ->
      let pp ppf = Catala_utils.Message.Content.emit ~ppf content Error in
      let* () =
        Format.kasprintf logger "Error during program execution:@\n%t" pp
      in
      let pos = get_step_pos !last_step in
      Lwt.return
        [
          {
            value = Exn { pos; exn = Internal pp };
            breakpoint = false;
            env = !last_step.env;
          };
        ]
    | Error (Catala_utils.Message.CompilerErrors contents) ->
      let pp ppf = Catala_utils.Message.Content.emit_n ~ppf contents Error in
      let* () =
        Format.kasprintf logger "Errors during program execution:@\n%t" pp
      in
      let pos = get_step_pos !last_step in
      Lwt.return
        [
          {
            value = Exn { pos; exn = Internal pp };
            breakpoint = false;
            env = !last_step.env;
          };
        ]
    | Error (Catala_runtime.Error (a, b, msg_opt)) ->
      let pos = get_step_pos !last_step in
      Lwt.return
        [
          {
            value = Exn { pos; exn = Runtime (a, b, msg_opt) };
            breakpoint = false;
            env = !last_step.env;
          };
        ]
    | Error exn ->
      let* () =
        Format.kasprintf logger
          "Unhandled exception during program execution: %s"
          (Printexc.to_string exn)
      in
      Lwt.return_nil
    | Ok ((EStruct { name = _; fields }, _) : dcalc_expr) ->
      (* Scope result variables *)
      StructField.Map.bindings fields
      |> List.rev_map (fun (sf, v) ->
          let var =
            Bindlib.new_var (fun _ -> Mark.remove v) (StructField.to_string sf)
          in
          let expr =
            let field_pos = snd (StructField.get_info sf) in
            let (Typed { pos = _; ty }) = Mark.get v in
            Mark.add (Typed { pos = field_pos; ty }) (EVar var)
          in
          let env = Env (Var.Map.singleton var (v, Env Var.Map.empty)) in
          { value = Expr expr; env; breakpoint = false })
      |> Lwt.return
    | Ok result ->
      let result_var = Bindlib.new_var (fun _ -> Mark.remove result) "Result" in
      let env =
        Env (Var.Map.singleton result_var (result, Env Var.Map.empty))
      in
      Lwt.return [{ value = Expr result; env; breakpoint = false }]
  in
  let all_steps = Array.of_list (List.rev (last_step_l @ !rev_steps)) in
  Lwt.return
    {
      rpc;
      logger;
      file;
      scope;
      program;
      pmap;
      all_steps;
      index = 0;
      final_index = Array.length all_steps - 1;
      steps_table = !steps_table_r;
      breakpoints = Pos.Map.empty;
      result;
    }

let load_program options ((clerk_config : Clerk_config.t), root_dir) file scope
    =
  let find_scope (p : typed Dcalc.Ast.program) (scope : string) =
    let l : ScopeName.t list = ScopeName.Map.keys p.decl_ctx.ctx_scopes in
    List.find (fun s -> ScopeName.base s = scope) l
  in
  let input_src = Global.FileName (File.clean_path file) in
  let surface = Surface.Parser_driver.parse_top_level_file input_src in
  let stdlib_path = File.(root_dir / "_build" / "libcatala") in
  if not (File.exists stdlib_path) then
    failwith "Stdlib not found - Please compile your project first.";
  let mod_uses, modules =
    try
      Driver.load_modules options
        ~stdlib:(Some (Global.raw_file stdlib_path))
        (List.map Global.raw_file clerk_config.global.include_dirs)
        surface
    with e -> raise e
  in
  let ctx =
    Desugared.Name_resolution.form_context (surface, mod_uses) modules
  in
  let modules_contents : Surface.Ast.module_content Uid.Module.Map.t =
    Uid.Module.Map.map (fun elt -> fst elt) modules
  in
  let prg =
    Desugared.From_surface.translate_program ctx modules_contents surface
    |> Desugared.Disambiguate.program
  in
  let exceptions_graphs = Scopelang.From_desugared.build_exceptions_graph prg in
  let scopelang_prg =
    Scopelang.From_desugared.translate_program prg exceptions_graphs
    |> Scopelang.Ast.type_program
  in
  let prg =
    Dcalc.From_scopelang.translate_program scopelang_prg |> Typing.program
    (* Do not optimize, we lose some relevant locations *)
  in
  let scope = find_scope prg scope in
  let pmap =
    let e = Expr.unbox (Program.to_expr prg scope) |> addcustom in
    let rec process e acc =
      let acc =
        match Mark.remove e with
        (* | ELit _ -> acc *)
        | _be ->
          let pos = get_pos e in
          PMap.add pos e acc
      in
      Expr.shallow_fold process e acc
    in
    process e PMap.empty
  in
  prg, scope, pmap

let try_build_deps ~logger file =
  let* () = logger "Building scope dependencies..." in
  try
    let _ = File.process_out "clerk" ["run"; file; "--prepare-only"; "-d"] in
    Lwt.return_unit
  with Failure s ->
    let* () = Format.ksprintf logger "Failed to build dependencies: %s" s in
    Format.ksprintf logger
      "Trying to launch debugger anyway... If it fails, make sure that you \
       declared a `clerk.toml` file in your project."

let run_debugger ?inputs rpc ~file ~scope logger : debugger_state Lwt.t =
  let* () = logger "Initializing Catala debugger.." in
  let build_dir, config_and_root =
    match Utils.lookup_clerk_toml file with
    | None -> "_build", (Clerk_config.default_config, ".")
    | Some (config, root) -> File.(root / "_build"), (config, root)
  in
  let options =
    Catala_utils.Global.enforce_options ~input_src:(FileName file)
      ~whole_program:true ~bin_dir:build_dir
      ~path_rewrite:(fun i -> (i :> string))
      ~stop_on_error:true ()
  in
  let* () = try_build_deps ~logger file in
  let program, scope, pmap = load_program options config_and_root file scope in
  let* () =
    Format.kasprintf logger "Program loaded - main scope: %a" ScopeName.format
      scope
  in
  let file = Doc_id.of_file file in
  Interpreter.load_runtime_modules
    ~hashf:(Hash.finalise ~monomorphize_types:false)
    program;
  Log.debug (fun m ->
      m "Evaluating with debug info (scope %a from %a)" ScopeName.format scope
        Doc_id.format file);
  let* full_state = run_debugger ?inputs rpc logger file scope program pmap in
  Log.debug (fun m ->
      m "Program fully evaluated (%d steps)" full_state.final_index);
  let* () = logger "Catala debugger ready" in
  Lwt.return full_state

let pos_to_source : Pos.t -> Source.t =
 fun p -> Source.make ~path:(Some (Pos.get_file p)) ()

let expr_to_breakpoint ?l e : Breakpoint.t =
  let pos = get_pos e in
  {
    id = None;
    verified = true;
    message = None;
    source = Some (pos_to_source pos);
    line = Some (Pos.get_start_line pos);
    column = Some (Pos.get_start_column pos);
    end_line = Some (Pos.get_end_line pos);
    end_column = Some (Pos.get_end_column pos);
    instruction_reference = None;
    offset = Option.map (( - ) (Pos.get_start_line pos)) l;
  }

let possible_breakpoints runner doc_id line : Breakpoint_location.t list =
  let to_bp_loc e : Breakpoint_location.t =
    let pos = get_pos e in
    {
      line = Pos.get_start_line pos;
      column = Some (Pos.get_start_column pos);
      end_line = Some (Pos.get_end_line pos);
      end_column = Some (Pos.get_end_column pos);
    }
  in
  match PMap.lookup_on_line doc_id line runner.pmap with
  | None -> []
  | Some l ->
    Log.debug (fun m ->
        m "possible breakpoints for line %d on lines: [%a]" line
          Format.(
            pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_int)
          (List.map (fun e -> get_pos e |> Pos.get_start_line) l));
    List.filter should_stop l |> List.map to_bp_loc

let step state : [ `End | `Exn of step | `B of step | `Ok of step ] =
  if state.index < state.final_index then begin
    state.index <- succ state.index;
    match state.all_steps.(state.index) with
    | { breakpoint = true; _ } as s -> `B s
    | { value = Exn _; _ } as step -> `Exn step
    | { breakpoint = false; _ } as s -> `Ok s
  end
  else
    match state.all_steps.(state.index) with
    | { value = Exn _; _ } as step -> `Exn step
    | _ -> `End

let continue_until_breakpoint state : [ `End | `B of step | `Exn of step ] =
  let rec loop () =
    match step state with
    | `Ok _ -> loop ()
    | `B s -> `B s
    | `End -> `End
    | `Exn step -> `Exn step
  in
  loop ()

let step_back state : [ `Start | `B of step | `Ok of step ] =
  if state.index > 0 then begin
    state.index <- pred state.index;
    match state.all_steps.(state.index) with
    | { breakpoint = true; _ } as s -> `B s
    | { breakpoint = false; _ } as s -> `Ok s
  end
  else `Start

let continue_back_until_breakpoint state : [ `Start | `B of step ] =
  let rec loop () =
    match step_back state with
    | `Ok _ -> loop ()
    | `B s -> `B s
    | `Start -> `Start
  in
  loop ()

let find_breakpoint (pos_map : PMap.t) doc_id (bp : Source_breakpoint.t) :
    (Pos.t * Breakpoint.t) option =
  match bp.column with
  | None -> begin
    match PMap.lookup_best_on_line doc_id bp.line pos_map with
    | None ->
      Log.debug (fun m ->
          m "Did not find a satisfying candidate at line %d" bp.line);
      None
    | Some e when should_stop e ->
      let pos = get_pos e in
      Some (pos, expr_to_breakpoint ~l:bp.line e)
    | Some _ -> None
  end
  | Some c ->
    let pos = Pos.from_info (doc_id :> string) bp.line c bp.line c in
    PMap.lookup pos pos_map
    |> Option.map (PMap.DS.filter should_stop)
    |> Option.map PMap.DS.choose
    |> Option.map (fun e ->
        let pos = get_pos e in
        pos, expr_to_breakpoint e)

let set_breakpoints
    (state : debugger_state)
    (doc_id : Doc_id.t)
    (source_bps : Source_breakpoint.t list) : Breakpoint.t list =
  let preserved_breakpoints, removed_breakpoints =
    Pos.Map.partition
      (fun pos _ -> Pos.get_file pos <> (doc_id :> string))
      state.breakpoints
  in
  (* Remove outdated breakpoints *)
  let () =
    Pos.Map.bindings removed_breakpoints
    |> List.concat_map (fun (_, (_, steps)) -> steps)
    |> List.iter (fun step -> step.breakpoint <- false)
  in
  let new_breakpoints_with_pos =
    List.filter_map (find_breakpoint state.pmap doc_id) source_bps
  in
  let new_breakpoints =
    (* Update steps with the newly set breakpoints *)
    List.filter_map
      (fun (pos, bp) ->
        match Pos.Map.find_opt pos state.steps_table with
        | None ->
          (* Expression is not part of the execution *)
          None
        | Some steps ->
          List.iter (fun step -> step.breakpoint <- true) steps;
          Some (pos, (bp, steps)))
      new_breakpoints_with_pos
  in
  let () =
    (* Update global breakpoints *)
    state.breakpoints <-
      Pos.Map.disjoint_union preserved_breakpoints
        (Pos.Map.of_list new_breakpoints)
  in
  (* Return every breakpoint even though some are unreachable *)
  List.map snd new_breakpoints_with_pos

let current_step state =
  Log.debug (fun m -> m "current step: %d" state.index);
  state.all_steps.(state.index)
