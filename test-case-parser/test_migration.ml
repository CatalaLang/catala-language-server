(* Test-migration pipeline, split out of [test_case_parser_lib]: signature-drift
   detection (the structured [sig_diff]), the value rewrite (apply slice a:
   [migrate_value]/[migrate_record]), and the [migrate status|diff|init|apply]
   commands. Depends on the core module for the read/write/value machinery and
   the signature primitives (canonical projection, hash, pin, snapshot). *)

open Catala_utils
open Shared_ast
open Test_case_parser_lib

(* === Structured signature diff (for migration plan/apply) ===================

   A SCHEMA diff: two signatures (not two values), keyed by a location into the
   contract. Distinct from the value-level [compute_diff] below, which compares
   two values of ONE fixed type; here the type itself changes. The model is
   navigable so [apply] can later lower each node onto the recovered value tree.

   Lock invariant (unit-tested): [sig_diff a b = [] iff hash a = hash b]. The
   structural pass alone guarantees it; the rename pass only REGROUPS nodes
   (a detected rename always injects a [Renamed] node), so it can never turn a
   non-empty diff empty. Rename detection is therefore advisory — wrong guesses
   cost readability, never totality. *)

type sdiff_loc =
  | LScope (* scope identity (module.name) *)
  | LInput of string
  | LOutput of string
  | LStruct of string (* a struct def, by qualified name *)
  | LEnum of string
  | LField of string * string (* struct name, field name *)
  | LCtor of string * string (* enum name, ctor name *)

type sdiff_change =
  | Added
  | Removed
  | Retyped of cref * cref
  | CtxFlip of bool * bool (* old is_context, new is_context *)
  | PayloadChanged of cref option * cref option (* enum ctor payload *)
  | Renamed of string (* this def renamed to <new qualified name> *)

type sdiff_node = sdiff_loc * sdiff_change

(* Deterministic ordering: group ctors under their enum, fields under their
   struct; inputs, then outputs, then enums, then structs (mirrors the text). *)
let loc_sort_key = function
  | LScope -> "0"
  | LInput n -> "1 " ^ n
  | LOutput n -> "2 " ^ n
  | LEnum n -> "3 " ^ n ^ " "
  | LCtor (e, c) -> "3 " ^ e ^ "  " ^ c
  | LStruct n -> "4 " ^ n ^ " "
  | LField (s, f) -> "4 " ^ s ^ "  " ^ f

(* One comparable atom per canonical line: [key] is the equality oracle (equal
   key <=> equal canonical line), [cref]/[ctx] carry structured detail. *)
type atom = { key : string; cref : cref option; ctx : bool option }

let atoms_of (cs : csig) : (sdiff_loc * atom) list =
  let a ?cref ?ctx key = { key; cref; ctx } in
  let scope = [ LScope, a (cs.cs_module ^ "." ^ cs.cs_name) ] in
  let inputs =
    List.map
      (fun (n, (ctx, cr)) ->
        ( LInput n,
          a ~cref:cr ~ctx
            ((if ctx then "ctx " else "inp ") ^ string_of_cref cr) ))
      cs.cs_inputs
  in
  let outputs =
    List.map (fun (n, cr) -> LOutput n, a ~cref:cr (string_of_cref cr)) cs.cs_outputs
  in
  let enums =
    List.concat_map
      (fun (e, ctors) ->
        (LEnum e, a "enum")
        :: List.map
             (fun (c, p) ->
               ( LCtor (e, c),
                 {
                   key = (match p with None -> "-" | Some cr -> string_of_cref cr);
                   cref = p;
                   ctx = None;
                 } ))
             ctors)
      cs.cs_enums
  in
  let structs =
    List.concat_map
      (fun (s, fields) ->
        (LStruct s, a "struct")
        :: List.map
             (fun (f, cr) -> LField (s, f), a ~cref:cr (string_of_cref cr))
             fields)
      cs.cs_structs
  in
  scope @ inputs @ outputs @ enums @ structs

(* Exact, total structural diff over the canonical atoms. *)
let structural_diff (old_cs : csig) (new_cs : csig) : sdiff_node list =
  let oa = atoms_of old_cs and na = atoms_of new_cs in
  let locs =
    List.sort_uniq
      (fun x y -> String.compare (loc_sort_key x) (loc_sort_key y))
      (List.map fst oa @ List.map fst na)
  in
  List.concat_map
    (fun loc ->
      match List.assoc_opt loc oa, List.assoc_opt loc na with
      | Some _, None -> [ loc, Removed ]
      | None, Some _ -> [ loc, Added ]
      | None, None -> []
      | Some o, Some n when String.equal o.key n.key -> []
      | Some o, Some n -> (
        (* both present, content differs *)
        match loc with
        | LInput _ ->
          let ctx_node =
            match o.ctx, n.ctx with
            | Some a, Some b when a <> b -> [ loc, CtxFlip (a, b) ]
            | _ -> []
          in
          let ty_node =
            match o.cref, n.cref with
            | Some a, Some b when a <> b -> [ loc, Retyped (a, b) ]
            | _ -> []
          in
          ctx_node @ ty_node
        | LCtor _ -> [ loc, PayloadChanged (o.cref, n.cref) ]
        | _ -> (
          match o.cref, n.cref with
          | Some a, Some b -> [ loc, Retyped (a, b) ]
          | _ -> [ loc, Removed ] (* unreachable: def markers have a constant key *)
          )))
    locs

(* Rewrite a nominal name / a reference under a rename map. *)
let rename_name sigma n =
  match List.find_opt (fun (_, o, _) -> String.equal o n) sigma with
  | Some (_, _, nw) -> nw
  | None -> n

let rec rename_cref sigma = function
  | RNominal n -> RNominal (rename_name sigma n)
  | ROption c -> ROption (rename_cref sigma c)
  | RArray c -> RArray (rename_cref sigma c)
  | RTuple cs -> RTuple (List.map (rename_cref sigma) cs)
  | RArrow (cs, c) -> RArrow (List.map (rename_cref sigma) cs, rename_cref sigma c)
  | leaf -> leaf

let apply_rename_csig sigma (cs : csig) : csig =
  {
    cs with
    cs_inputs =
      List.map (fun (n, (ctx, cr)) -> n, (ctx, rename_cref sigma cr)) cs.cs_inputs;
    cs_outputs = List.map (fun (n, cr) -> n, rename_cref sigma cr) cs.cs_outputs;
    cs_structs =
      List.map
        (fun (n, fs) ->
          ( rename_name sigma n,
            List.map (fun (f, cr) -> f, rename_cref sigma cr) fs ))
        cs.cs_structs;
    cs_enums =
      List.map
        (fun (n, cts) ->
          ( rename_name sigma n,
            List.map (fun (c, p) -> c, Option.map (rename_cref sigma) p) cts ))
        cs.cs_enums;
  }

(* Advisory rename detection. A def present only in [old] and one present only
   in [new], of the same KIND, are a rename candidate when their bodies match
   after blanking every still-unmatched nominal name (so a rename whose body
   references another rename still pairs). Only UNIQUE matches are taken — an
   ambiguous shape stays an honest add+remove pair. *)
let detect_renames (old_cs : csig) (new_cs : csig) (base : sdiff_node list) :
    ([ `Struct | `Enum ] * string * string) list =
  let removed kind =
    List.filter_map
      (function
        | LStruct n, Removed when kind = `Struct -> Some n
        | LEnum n, Removed when kind = `Enum -> Some n
        | _ -> None)
      base
  in
  let added kind =
    List.filter_map
      (function
        | LStruct n, Added when kind = `Struct -> Some n
        | LEnum n, Added when kind = `Enum -> Some n
        | _ -> None)
      base
  in
  let pending =
    removed `Struct @ added `Struct @ removed `Enum @ added `Enum
  in
  let rec blank = function
    | RNominal n -> RNominal (if List.mem n pending then "?" else n)
    | ROption c -> ROption (blank c)
    | RArray c -> RArray (blank c)
    | RTuple cs -> RTuple (List.map blank cs)
    | RArrow (cs, c) -> RArrow (List.map blank cs, blank c)
    | leaf -> leaf
  in
  let struct_body cs n =
    List.assoc n cs.cs_structs
    |> List.map (fun (f, cr) -> f ^ ":" ^ string_of_cref (blank cr))
    |> List.sort String.compare |> String.concat ";"
  in
  let enum_body cs n =
    List.assoc n cs.cs_enums
    |> List.map (fun (c, p) ->
           c ^ ":"
           ^ (match p with None -> "-" | Some cr -> string_of_cref (blank cr)))
    |> List.sort String.compare |> String.concat ";"
  in
  let pair_unique kind body_old body_new =
    let rem = removed kind and add = added kind in
    List.filter_map
      (fun o ->
        let bo = body_old o in
        match List.filter (fun n -> String.equal (body_new n) bo) add with
        | [ n ] ->
          (* unique on the added side; also require uniqueness on removed side *)
          if List.length (List.filter (fun o' -> String.equal (body_old o') bo) rem) = 1
          then Some (kind, o, n)
          else None
        | _ -> None)
      rem
  in
  pair_unique `Struct (struct_body old_cs) (struct_body new_cs)
  @ pair_unique `Enum (enum_body old_cs) (enum_body new_cs)

let sig_diff (old_cs : csig) (new_cs : csig) : sdiff_node list =
  let base = structural_diff old_cs new_cs in
  match detect_renames old_cs new_cs base with
  | [] -> base
  | renames ->
    let sigma = renames in
    (* Apply the renames to OLD, then re-diff: nodes explained purely by the
       rename vanish, leaving the residual real changes. Prepend one Renamed
       node per detected rename. *)
    let residual = structural_diff (apply_rename_csig sigma old_cs) new_cs in
    let rename_nodes =
      List.map
        (fun (kind, o, n) ->
          (match kind with `Struct -> LStruct o | `Enum -> LEnum o), Renamed n)
        renames
    in
    rename_nodes @ residual

(* Render a structured diff to human lines (one per node). *)
let pp_sig_diff (old_cs : csig) (new_cs : csig) (nodes : sdiff_node list) :
    string list =
  let r = string_of_cref in
  let popt = function None -> "-" | Some cr -> r cr in
  let field_cref cs s f =
    match List.assoc_opt s cs.cs_structs with
    | Some fs -> Option.map r (List.assoc_opt f fs)
    | None -> None
  in
  let ctor_payload cs e c =
    match List.assoc_opt e cs.cs_enums with
    | Some cts -> ( match List.assoc_opt c cts with Some p -> Some p | None -> None)
    | None -> None
  in
  let input cs n =
    match List.assoc_opt n cs.cs_inputs with
    | Some (ctx, cr) -> r cr ^ if ctx then " (ctx)" else ""
    | None -> "?"
  in
  let output cs n = match List.assoc_opt n cs.cs_outputs with Some cr -> r cr | None -> "?" in
  List.map
    (fun (loc, ch) ->
      match loc, ch with
      | LStruct o, Renamed n -> Printf.sprintf "rename struct %s -> %s" o n
      | LEnum o, Renamed n -> Printf.sprintf "rename enum %s -> %s" o n
      | LScope, _ ->
        Printf.sprintf "scope: %s.%s -> %s.%s" old_cs.cs_module old_cs.cs_name
          new_cs.cs_module new_cs.cs_name
      | LInput n, Added -> Printf.sprintf "+ input %s: %s" n (input new_cs n)
      | LInput n, Removed -> Printf.sprintf "- input %s: %s" n (input old_cs n)
      | LInput n, Retyped (a, b) -> Printf.sprintf "~ input %s: %s -> %s" n (r a) (r b)
      | LInput n, CtxFlip (a, b) ->
        Printf.sprintf "~ input %s: %s -> %s" n
          (if a then "ctx" else "inp")
          (if b then "ctx" else "inp")
      | LOutput n, Added -> Printf.sprintf "+ output %s: %s" n (output new_cs n)
      | LOutput n, Removed -> Printf.sprintf "- output %s: %s" n (output old_cs n)
      | LOutput n, Retyped (a, b) -> Printf.sprintf "~ output %s: %s -> %s" n (r a) (r b)
      | LStruct n, Added -> Printf.sprintf "+ struct %s" n
      | LStruct n, Removed -> Printf.sprintf "- struct %s" n
      | LEnum n, Added -> Printf.sprintf "+ enum %s" n
      | LEnum n, Removed -> Printf.sprintf "- enum %s" n
      | LField (s, f), Added ->
        Printf.sprintf "+ field %s.%s: %s" s f
          (Option.value ~default:"?" (field_cref new_cs s f))
      | LField (s, f), Removed ->
        Printf.sprintf "- field %s.%s: %s" s f
          (Option.value ~default:"?" (field_cref old_cs s f))
      | LField (s, f), Retyped (a, b) ->
        Printf.sprintf "~ field %s.%s: %s -> %s" s f (r a) (r b)
      | LCtor (e, c), Added ->
        Printf.sprintf "+ ctor %s.%s%s" e c
          (match ctor_payload new_cs e c with
          | Some (Some cr) -> ": " ^ r cr
          | _ -> "")
      | LCtor (e, c), Removed ->
        Printf.sprintf "- ctor %s.%s%s" e c
          (match ctor_payload old_cs e c with
          | Some (Some cr) -> ": " ^ r cr
          | _ -> "")
      | LCtor (e, c), PayloadChanged (a, b) ->
        Printf.sprintf "~ ctor %s.%s: %s -> %s" e c (popt a) (popt b)
      | _ -> "?" (* unreachable: def markers don't retype, CtxFlip is input-only *))
    nodes

(* === apply (slice a): structured value rewrite ============================

   Pure. Fold a (renames + old-type/new-type) correspondence onto a recovered
   value tree (`runtime_value`), applying ONLY the auto tiers (the [Resolved]
   outcomes): nominal relabel, drop, option wrap/unwrap, recursing structurally.
   An ADDED field/input is left `Unset` and flagged `NeedsValue` (its value is the
   user's to provide, not ours to fabricate). Anything else — scalar coerce, a
   removed/renamed enum variant, tuple arity change, struct<->enum — is left a
   `NeedsDecision` note: loud, never a silent guess. The [NeedsResolving] outcomes
   are exactly the plan layer's worklist. `apply` (slice b: recover -> rewrite ->
   write -> verify) consumes this; here we only build and unit-test the rewrite.
   Renames are `(old,new)` nominal name pairs, from `signature_renames`. *)

(* What the rewrite did at one location. A [Resolved] outcome was handled
   automatically (informational); a [NeedsResolving] one is the plan's worklist —
   the human (via the plan layer) must finish it. A location kept verbatim emits
   no step (no news is good news). *)
type resolved =
  | Renamed of string * string (* nominal type relabel applied *)
  | Wrapped (* T -> option T *)
  | Unwrapped (* option T -> T *)
  | Dropped (* field/input removed *)

type needs_resolving =
  | NeedsValue (* a new field/input: left Unset, the user must supply a value *)
  | NeedsDecision of string
      (* couldn't auto-migrate: <reason> — needs a default, a choice, or a
         user-supplied transform *)

type outcome = Resolved of resolved | NeedsResolving of needs_resolving
type step = { path : string; outcome : outcome }

(* Does the rewrite leave anything for the human/plan to finish? *)
let needs_attention (steps : step list) =
  List.exists
    (fun s -> match s.outcome with NeedsResolving _ -> true | _ -> false)
    steps

let is_option = function O.TOption _ -> true | _ -> false

(* The value written into a NeedsValue hole: a readable `impossible` carrying the
   #[testcase.todo] marker, so it is distinguishable from a deliberate `impossible`
   and queryable (gate / editor). NeedsDecision holes are NOT written this way —
   `apply` leaves those tests stale (§ apply), preserving the old value. *)
let unset_hole : O.runtime_value = { O.value = O.Unset; attrs = [ O.Todo ] }

let rec tystr : O.typ -> string = function
  | TBool -> "bool"
  | TInt -> "int"
  | TRat -> "rat"
  | TMoney -> "money"
  | TDate -> "date"
  | TDuration -> "duration"
  | TUnit -> "unit"
  | TUnset -> "unset"
  | TOption t -> "option(" ^ tystr t ^ ")"
  | TArray t -> "array(" ^ tystr t ^ ")"
  | TTuple l -> "tuple(" ^ String.concat "," (List.map tystr l) ^ ")"
  | TArrow _ -> "arrow"
  | TStruct d -> "@" ^ d.struct_name
  | TEnum d -> "@" ^ d.enum_name

(* Rewrite [v] (of type [old_typ]) to [new_typ]. *)
let rec migrate_value
    lang
    (renames : (string * string) list)
    ~(old_typ : O.typ)
    ~(new_typ : O.typ)
    ~(path : string)
    (v : O.runtime_value) : O.runtime_value * step list =
  let blocked reason = { path; outcome = NeedsResolving (NeedsDecision reason) } in
  let did r = { path; outcome = Resolved r } in
  let added_at p = { path = p; outcome = NeedsResolving NeedsValue } in
  let dropped_at p = { path = p; outcome = Resolved Dropped } in
  let same_nominal o n =
    String.equal o n
    || List.exists (fun (a, b) -> String.equal a o && String.equal b n) renames
  in
  (* Bottom / type-agnostic values short-circuit the type walk and are preserved
     VERBATIM: [Unset] is `impossible` (⊥), which typechecks at any type, so it
     survives any type change as-is (NOT wrapped to `Present content impossible`)
     — a deliberate `impossible` is a complete value, not a hole, so it is not
     flagged. [NotOverridden] (a context var's "use the scope default") is
     likewise a valid type-agnostic state. (A hole the MIGRATION itself creates —
     an added field, or unwrapping an Absent — is flagged NeedsValue at that site;
     a pre-existing Unset is the runner's concern, not the migration's.) *)
  match v.value with
  | O.Unset | O.NotOverridden -> v, []
  | _ -> (
  match old_typ, new_typ with
  | TOption a, TOption b -> (
    (* option -> option: keep the present/absent shape, retype the payload. *)
    match v.value with
    | O.Enum (odecl, (ctor, None)) ->
      let retype (c, t) = c, Option.map (fun _ -> b) t in
      ( {
          value =
            O.Enum
              ( { odecl with constructors = List.map retype odecl.constructors },
                (ctor, None) );
          attrs = v.attrs;
        },
        [] )
    | O.Enum (odecl, (ctor, Some p)) ->
      let p', ns =
        migrate_value lang renames ~old_typ:a ~new_typ:b ~path:(path ^ "?") p
      in
      let retype (c, t) = c, Option.map (fun _ -> b) t in
      ( {
          value =
            O.Enum
              ( { odecl with constructors = List.map retype odecl.constructors },
                (ctor, Some p') );
          attrs = v.attrs;
        },
        ns )
    | _ -> v, [ blocked "expected an option value" ])
  | a, TOption b when not (is_option a) ->
    (* wrap: T -> option T *)
    let inner, ns =
      migrate_value lang renames ~old_typ:a ~new_typ:b ~path v
    in
    ( {
        value =
          O.Enum
            ( mk_optional_enum_decl lang b,
              ((get_lang_strings lang).present, Some inner) );
        attrs = v.attrs;
      },
      did Wrapped :: ns )
  | TOption a, b when not (is_option b) -> (
    (* unwrap: option T -> T *)
    match v.value with
    | O.Enum (_, (_, Some p)) ->
      let p', ns =
        migrate_value lang renames ~old_typ:a ~new_typ:b ~path p
      in
      p', did Unwrapped :: ns
    | O.Enum (_, (_, None)) ->
      (* unwrapping an Absent: the field is now mandatory but there is no value
         to carry over — leave a hole for the user, exactly like an added field. *)
      unset_hole, [ added_at path ]
    | _ -> v, [ blocked "expected an option value" ])
  | TArray a, TArray b -> (
    match v.value with
    | O.Array elems ->
      let elems', ns =
        Array.to_list elems
        |> List.mapi (fun i e ->
               migrate_value lang renames ~old_typ:a ~new_typ:b
                 ~path:(Printf.sprintf "%s[%d]" path i) e)
        |> List.split
      in
      ( { value = O.Array (Array.of_list elems'); attrs = v.attrs },
        List.concat ns )
    | _ -> v, [ blocked "expected an array value" ])
  | TTuple oas, TTuple nbs -> (
    if List.length oas <> List.length nbs then
      v, [ blocked "tuple arity changed" ]
    else
      match v.value with
      | O.Array elems when Array.length elems = List.length oas ->
        let elems', ns =
          List.combine (List.combine oas nbs) (Array.to_list elems)
          |> List.mapi (fun i ((oa, nb), e) ->
                 migrate_value lang renames ~old_typ:oa ~new_typ:nb
                   ~path:(Printf.sprintf "%s(%d)" path i) e)
          |> List.split
        in
        ( { value = O.Array (Array.of_list elems'); attrs = v.attrs },
          List.concat ns )
      | _ -> v, [ blocked "malformed tuple value" ])
  | TStruct od, TStruct nd -> (
    if not (same_nominal od.struct_name nd.struct_name) then
      ( v,
        [
          blocked
            (Printf.sprintf "type changed: %s -> %s" od.struct_name
               nd.struct_name);
        ] )
    else
      match v.value with
      | O.Struct (_, vfields) ->
        let relabel =
          if String.equal od.struct_name nd.struct_name then []
          else [ did (Renamed (od.struct_name, nd.struct_name)) ]
        in
        let new_fields, notes =
          List.fold_left
            (fun (acc, ns) (fname, nft) ->
              let fpath = path ^ "." ^ fname in
              match
                List.assoc_opt fname od.fields, List.assoc_opt fname vfields
              with
              | Some oft, Some fv ->
                let fv', fns =
                  migrate_value lang renames ~old_typ:oft ~new_typ:nft
                    ~path:fpath fv
                in
                acc @ [ fname, fv' ], ns @ fns
              | _ ->
                ( acc @ [ fname, unset_hole ],
                  ns @ [ added_at fpath ] ))
            ([], []) nd.fields
        in
        let dropped =
          List.filter_map
            (fun (fname, _) ->
              if List.mem_assoc fname nd.fields then None
              else Some (dropped_at (path ^ "." ^ fname)))
            vfields
        in
        { value = O.Struct (nd, new_fields); attrs = v.attrs },
        relabel @ notes @ dropped
      | _ -> v, [ blocked "expected a struct value" ])
  | TEnum od, TEnum nd -> (
    if not (same_nominal od.enum_name nd.enum_name) then
      ( v,
        [
          blocked
            (Printf.sprintf "type changed: %s -> %s" od.enum_name
               nd.enum_name);
        ] )
    else
      match v.value with
      | O.Enum (_, (ctor, payopt)) -> (
        match List.assoc_opt ctor nd.constructors with
        | None ->
          ( v,
            [
              blocked
                (Printf.sprintf "enum variant %s removed or renamed" ctor);
            ] )
        | Some new_payty -> (
          let old_payty = Option.join (List.assoc_opt ctor od.constructors) in
          match payopt, new_payty, old_payty with
          | None, None, _ ->
            { value = O.Enum (nd, (ctor, None)); attrs = v.attrs }, []
          | Some p, Some npt, Some opt ->
            let p', pns =
              migrate_value lang renames ~old_typ:opt ~new_typ:npt
                ~path:(path ^ ":" ^ ctor) p
            in
            { value = O.Enum (nd, (ctor, Some p')); attrs = v.attrs }, pns
          | _ ->
            ( v,
              [
                blocked
                  (Printf.sprintf "enum variant %s payload shape changed" ctor);
              ] )))
      | _ -> v, [ blocked "expected an enum value" ])
  | _ ->
    if old_typ = new_typ then v, []
    else
      ( v,
        [
          blocked
            (Printf.sprintf "needs explicit migration: %s -> %s"
               (tystr old_typ) (tystr new_typ));
        ] ))

(* Migrate a whole record (name -> value) given the old and new typed fields.
   A field present old+new with a value is rewritten; a dropped field is noted.
   A NEW field with no carried value depends on [added_is_hole]:
   - INPUTS ([added_is_hole=true], the default): a regular input must be provided,
     so it becomes a readable `Unset` flagged `NeedsValue` (the user's to fill).
   - OUTPUTS ([added_is_hole=false]): an output is an *assertion*, optional by
     nature — a new (or never-asserted) output is simply omitted, no hole, no
     step. Only a previously-asserted output whose type changed is flagged. *)
let migrate_record
    lang
    (renames : (string * string) list)
    ~(old_fields : (string * O.typ) list)
    ~(new_fields : (string * O.typ) list)
    ~(values : (string * O.runtime_value) list)
    ?(added_is_hole = true)
    () : (string * O.runtime_value) list * step list =
  let migrated =
    List.filter_map
      (fun (name, nt) ->
        match List.assoc_opt name old_fields, List.assoc_opt name values with
        | Some ot, Some v ->
          let v', ns =
            migrate_value lang renames ~old_typ:ot ~new_typ:nt ~path:name v
          in
          Some ((name, v'), ns)
        | _ ->
          if added_is_hole then
            Some
              ( (name, unset_hole),
                [ { path = name; outcome = NeedsResolving NeedsValue } ] )
          else None)
      new_fields
  in
  let dropped =
    List.filter_map
      (fun (name, _) ->
        if List.mem_assoc name new_fields then None
        else Some { path = name; outcome = Resolved Dropped })
      values
  in
  List.map fst migrated, List.concat_map snd migrated @ dropped

(* Nominal renames between two signatures, for [migrate_value]/[migrate_record].
   Derived from the same advisory detection that powers `migrate diff`. *)
let signature_renames (old_sd : O.scope_def) (new_sd : O.scope_def) :
    (string * string) list =
  let oc = canonical_model old_sd and nc = canonical_model new_sd in
  detect_renames oc nc (structural_diff oc nc)
  |> List.map (fun (_, o, n) -> o, n)
(* Index of the first occurrence of [sub] in [s], or None. *)
let find_sub ~sub s =
  let n = String.length sub and m = String.length s in
  if n = 0 then Some 0
  else
    let rec go i =
      if i + n > m then None
      else if String.equal (String.sub s i n) sub then Some i
      else go (i + 1)
    in
    go 0

let contains_sub ~sub s = find_sub ~sub s <> None

(* Content of the first double-quoted run in [s], or None. *)
let first_quoted s =
  match String.index_opt s '"' with
  | None -> None
  | Some i -> (
    match String.index_from_opt s (i + 1) '"' with
    | None -> None
    | Some j -> Some (String.sub s (i + 1) (j - i - 1)))

(* A "<Module>.<Scope>"-shaped token (two dot-separated capitalised idents):
   used to recover the tested scope of an unpinned test from its subscope line. *)
let qualified_scope_token s =
  let is_id_start c = (c >= 'A' && c <= 'Z') in
  let is_id c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
    || c = '_'
  in
  let m = String.length s in
  let rec scan i =
    if i >= m then None
    else if is_id_start s.[i] then
      let j = ref i in
      while !j < m && (is_id s.[!j] || s.[!j] = '.') do incr j done;
      let tok = String.sub s i (!j - i) in
      (* must look like Mod.Scope: exactly one dot, both parts capitalised *)
      (match String.split_on_char '.' tok with
       | [a; b]
         when String.length a > 0 && String.length b > 0
              && is_id_start a.[0] && is_id_start b.[0] ->
         Some tok
       | _ -> scan !j)
    else scan (i + 1)
  in
  scan 0

(* One testing scope as recovered textually from source. *)
type parsed_test = {
  pt_test_scope : string;        (* testing scope name, e.g. "Calc_rich" *)
  pt_target : string option;     (* "<Module>.<Scope>" under test *)
  pt_pin : string option;        (* full "<Module>.<Scope>@<hash>" pin *)
}

(* Recover the test scopes of a source file without typechecking it. Walks the
   attribute block that precedes each `declaration scope` (EN) /
   `déclaration champ d'application` (FR), keeping the #[testcase.sig] pin and the
   #[testcase.testui] marker, then reads the following subscope line for the
   tested scope. *)
let parse_tests_textually content =
  let lines = String.split_on_char '\n' content in
  let scope_kw = "declaration scope " in
  let scope_kw_fr = "champ d'application " in
  let is_scope_decl l =
    contains_sub ~sub:scope_kw l
    || (contains_sub ~sub:"claration" l && contains_sub ~sub:scope_kw_fr l)
  in
  let decl_name l =
    (* token right after the scope keyword, trailing ':' stripped *)
    let after =
      match find_sub ~sub:scope_kw l with
      | Some i -> String.sub l (i + String.length scope_kw) (String.length l - i - String.length scope_kw)
      | None -> (
        match find_sub ~sub:scope_kw_fr l with
        | Some i -> String.sub l (i + String.length scope_kw_fr) (String.length l - i - String.length scope_kw_fr)
        | None -> l)
    in
    let after = String.trim after in
    match String.index_opt after ':' with
    | Some k -> String.trim (String.sub after 0 k)
    | None -> (
      match String.index_opt after ' ' with
      | Some k -> String.sub after 0 k
      | None -> after)
  in
  let rec walk lines (pending : string list) (acc : parsed_test list)
      (cur : parsed_test option) =
    match lines with
    | [] -> List.rev (match cur with Some t -> t :: acc | None -> acc)
    | line :: rest ->
      let t = String.trim line in
      if String.length t >= 2 && t.[0] = '#' && t.[1] = '[' then
        (* attribute line: accumulate, flushing any open subscope search *)
        let acc = match cur with Some c -> c :: acc | None -> acc in
        walk rest (line :: pending) acc None
      else if is_scope_decl line then begin
        let acc = match cur with Some c -> c :: acc | None -> acc in
        let is_test =
          List.exists (fun a -> contains_sub ~sub:"testcase.testui" a) pending
        in
        let pin =
          List.find_map
            (fun a ->
              if contains_sub ~sub:"testcase.sig" a then first_quoted a else None)
            pending
        in
        if is_test then
          let target =
            match pin with
            | Some p -> (
              match String.index_opt p '@' with
              | Some k -> Some (String.sub p 0 k)
              | None -> Some p)
            | None -> None
          in
          walk rest []
            (match cur with Some c -> c :: acc | None -> acc)
            (Some { pt_test_scope = decl_name line; pt_target = target; pt_pin = pin })
        else walk rest [] acc None
      end
      else begin
        (* inside a test scope, before its target is known, look for the
           "Mod.Scope" subscope reference; otherwise reset pending on blanks *)
        match cur with
        | Some c when c.pt_target = None -> (
          match qualified_scope_token line with
          | Some tok -> walk rest [] (acc) (Some { c with pt_target = Some tok })
          | None -> walk rest (if t = "" then [] else pending) acc cur)
        | _ -> walk rest (if t = "" then [] else pending) acc cur
      end
  in
  walk lines [] [] None

(* Resolve the live signature hash of [module_name].[scope_base] as seen from
   [test_file], by loading only that file's import headers (so the live module
   typechecks regardless of test drift). Cached per module across a run. *)
let live_def_cache : (string, (O.scope_def, string) result) Hashtbl.t =
  Hashtbl.create 16

(* Does a header line import [module_name] (as a dotted/underscored identifier
   token)? Used to keep only the tested module's `> Using` line. *)
let line_mentions_module ~module_name line =
  let is_tok c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
    || c = '_' || c = '.'
  in
  let toks = ref [] and buf = Buffer.create 16 in
  String.iter
    (fun c ->
      if is_tok c then Buffer.add_char buf c
      else if Buffer.length buf > 0 then begin
        toks := Buffer.contents buf :: !toks;
        Buffer.clear buf
      end)
    line;
  if Buffer.length buf > 0 then toks := Buffer.contents buf :: !toks;
  List.exists (String.equal module_name) !toks

(* Resolve the live [scope_def] of [module_name].[scope_base] as seen from
   [test_file] (see §6b of the design doc). Cached per module across a run. *)
let live_scope_def ~test_file ~module_name ~scope_base =
  let key = module_name ^ "." ^ scope_base in
  match Hashtbl.find_opt live_def_cache key with
  | Some r -> r
  | None ->
    (* Load ONLY the tested module's import — not every `> Using` in the test —
       so a broken *sibling* import can't sink resolution of a healthy module
       (its transitive deps still come in through the module's own source). *)
    let all_headers =
      File.contents test_file
      |> String.split_on_char '\n'
      |> List.filter (fun l ->
             let l = String.trim l in
             String.length l > 0 && l.[0] = '>')
    in
    let headers =
      match List.filter (line_mentions_module ~module_name) all_headers with
      | [] -> all_headers (* defensive: no matching import (shouldn't happen) *)
      | xs -> xs
    in
    let content = String.concat "\n" headers ^ "\n" in
    let options =
      Global.enforce_options ~input_src:(Global.Contents (content, test_file)) ()
    in
    let path_to_build, include_dirs = lookup_include_dirs options in
    let build_ready = File.exists File.(path_to_build / "_build") in
    let r =
      try
        let prg, _ = read_program include_dirs path_to_build options in
        let found =
          ModuleName.Map.fold
            (fun mn modul acc ->
              match acc with
              | Some _ -> acc
              | None ->
                if String.equal (ModuleName.to_string mn) module_name then
                  Some (mn, modul)
                else None)
            prg.I.program_modules None
        in
        match found with
        | None ->
          Error
            (Printf.sprintf "module %s not found (renamed or removed?)"
               module_name)
        | Some (mn, modul) -> (
          let sc =
            ScopeName.Map.fold
              (fun sn sc acc ->
                match acc with
                | Some _ -> acc
                | None ->
                  if String.equal (ScopeName.base sn) scope_base then Some sc
                  else None)
              modul.I.module_scopes None
          in
          match sc with
          | None ->
            Error
              (Printf.sprintf "scope %s not found in module %s" scope_base
                 module_name)
          | Some sc -> Ok (get_scope_def prg sc ~tested_module:mn))
      with
      | Message.CompilerError _ | Message.CompilerErrors _ ->
        if not build_ready then
          Error "project not built — run `clerk start` first"
        else
          Error
            (Printf.sprintf "live module %s is missing or does not compile"
               module_name)
      | e -> Error (Printexc.to_string e)
    in
    Hashtbl.replace live_def_cache key r;
    r

let live_scope_hash ~test_file ~module_name ~scope_base =
  Result.map scope_signature_hash
    (live_scope_def ~test_file ~module_name ~scope_base)

(* Recursively collect Catala test files under [path] (file or directory). *)
let rec collect_catala_files path =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun name ->
           if String.length name > 0 && name.[0] = '.' then []
           else if String.equal name "_build" then []
           else collect_catala_files (Filename.concat path name))
  else
    let ok ext = Filename.check_suffix path ext in
    if ok ".catala_en" || ok ".catala_fr" || ok ".catala_pl" then [ path ]
    else []

(* Hash part of a "<Module>.<Scope>@<hash>" pin. *)
let pin_hash p =
  match String.index_opt p '@' with
  | Some k -> Some (String.sub p (k + 1) (String.length p - k - 1))
  | None -> None

(* Split "<Module>.<Scope>" into (module, scope). *)
let split_target target =
  match String.index_opt target '.' with
  | Some k ->
    ( String.sub target 0 k,
      String.sub target (k + 1) (String.length target - k - 1) )
  | None -> "", target

(* Pure classification decision (no IO), so the bucketing rules are unit-tested
   as a decision table. Inputs: the pin's hash (None = unpinned), the live-hash
   resolution (Ok hash | Error reason), whether the pinned snapshot is on disk,
   and the snapshot's basename (for the missing-snapshot reason). *)
let classify_decision ~(pin_hash : string option)
    ~(live : (string, string) result) ~(snapshot_present : bool)
    ~(snapshot_basename : string) : O.sig_state * string option =
  match pin_hash with
  | None -> `Unknown, None
  | Some _ -> (
    match live with
    | Error reason -> `Blocked, Some reason
    | Ok live_hash ->
      if pin_hash = Some live_hash then `Fresh, None
      else if snapshot_present then `Stale, None
      else `Blocked, Some (Printf.sprintf "missing snapshot %s" snapshot_basename))

let classify_test ~sig_dir ~test_file (pt : parsed_test) : O.sig_status_entry =
  let target = Option.value ~default:"?" pt.pt_target in
  let mk ?pin ?live ?reason state =
    {
      O.file = test_file;
      test_scope = pt.pt_test_scope;
      target_scope = target;
      state;
      pin;
      live;
      reason;
    }
  in
  match pt.pt_pin with
  | None -> mk `Unknown
  | Some pin ->
    let phash = pin_hash pin in
    let module_name, scope_base = split_target target in
    let live = live_scope_hash ~test_file ~module_name ~scope_base in
    let dir = Option.value ~default:(Filename.dirname test_file) sig_dir in
    let snap = File.(dir / (pin ^ ".sig.json")) in
    let state, reason =
      classify_decision ~pin_hash:phash ~live ~snapshot_present:(File.exists snap)
        ~snapshot_basename:(Filename.basename snap)
    in
    mk ?pin:phash ?live:(Result.to_option live) ?reason state

let state_label = function
  | `Fresh -> "fresh"
  | `Stale -> "stale"
  | `Unknown -> "unknown"
  | `Blocked -> "blocked"

(* Steady-state invariant: all tests of one scope share one pinned hash. More
   than one distinct hash for a scope means the cluster was migrated in part
   (some tests moved, some didn't) — a mid-migration state, never a committed
   one. Returns the "split" scopes (>1 distinct pin), scope-sorted, each with its
   distinct hashes (encounter order). Pure, so it is unit-tested directly. *)
let split_scopes (entries : O.sig_status_entry list) : (string * string list) list
    =
  List.fold_left
    (fun acc (e : O.sig_status_entry) ->
      match e.pin with
      | None -> acc
      | Some h ->
        let prev = try List.assoc e.target_scope acc with Not_found -> [] in
        if List.mem h prev then acc
        else
          (e.target_scope, h :: prev) :: List.remove_assoc e.target_scope acc)
    [] entries
  |> List.filter (fun (_, hs) -> List.length hs > 1)
  |> List.map (fun (s, hs) -> s, List.rev hs)
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let migrate_status check json sig_dir path _options =
  let files = collect_catala_files path in
  let entries =
    List.concat_map
      (fun test_file ->
        let content = File.contents test_file in
        parse_tests_textually content
        |> List.map (classify_test ~sig_dir ~test_file))
      files
  in
  let count st = List.length (List.filter (fun e -> e.O.state = st) entries) in
  let split_scopes = split_scopes entries in
  if json then write_stdout J.write_sig_status entries
  else begin
    Printf.printf "fresh:   %d\nstale:   %d\nunknown: %d\nblocked: %d\n"
      (count `Fresh) (count `Stale) (count `Unknown) (count `Blocked);
    List.iter
      (fun (e : O.sig_status_entry) ->
        if e.state <> `Fresh then
          Printf.printf "  %-8s %s  %s (%s)%s\n" (state_label e.state)
            e.target_scope e.test_scope (Filename.basename e.file)
            (match e.reason with Some r -> ": " ^ r | None -> ""))
      entries;
    List.iter
      (fun (scope, hs) ->
        Printf.printf "  split    %s  %d distinct pins (%s) — cluster part-migrated\n"
          scope (List.length hs)
          (String.concat ", " (List.map (fun h -> String.sub h 0 8) hs)))
      split_scopes
  end;
  (* Gate mode: a drifted (stale) or corrupted (blocked) test fails the build, as
     does a split scope (a half-migrated cluster is not a steady state). Unpinned
     (unknown) tests only warn — they predate the pin and need a `migrate init`,
     not a red build. *)
  if check then begin
    let bad = count `Stale + count `Blocked + List.length split_scopes in
    if bad > 0 then begin
      if json then
        Printf.eprintf
          "migrate: %d test(s) stale/blocked + %d split scope(s) — run `migrate \
           diff`\n"
          (count `Stale + count `Blocked)
          (List.length split_scopes);
      exit 1
    end
  end

(* migrate init: seed a pin onto unpinned ("unknown") tests. Safe only when the
   file typechecks against the live module — then its values are valid by
   definition, so "adopt current" is correct. A file that does NOT typecheck is
   an unpinned-but-drifted case that needs real migration, not a seed, so we
   refuse it. Mechanically this is read|write over the seedable files: reading
   stamps the live signature into every test's tested_scope, and writing emits
   the pin (and the snapshot) from it. *)
let init_file ~sig_dir test_file =
  let parsed = parse_tests_textually (File.contents test_file) in
  if not (List.exists (fun pt -> pt.pt_pin = None) parsed) then `Nothing_to_seed
  else
    let unpinned = List.length (List.filter (fun pt -> pt.pt_pin = None) parsed) in
    let options = Global.enforce_options ~input_src:(Global.FileName test_file) () in
    let path_to_build, include_dirs = lookup_include_dirs options in
    match
      try `Ok (import_catala_tests (read_program include_dirs path_to_build options))
      with e -> `Err (Printexc.to_string e)
    with
    | `Err msg -> `Cannot_seed msg
    | `Ok [] -> `Cannot_seed "no tests recovered"
    | `Ok tests ->
      let dir = Option.value ~default:(Filename.dirname test_file) sig_dir in
      List.iter
        (fun (t : O.test) -> write_sig_snapshot dir t.tested_scope)
        tests;
      let lang = Catala_utils.Cli.file_lang test_file in
      File.with_out_channel test_file (fun oc ->
          let ppf = Format.formatter_of_out_channel oc in
          emit_test_list ppf lang tests;
          Format.pp_print_flush ppf ());
      `Seeded unpinned

let migrate_init sig_dir path _options =
  collect_catala_files path
  |> List.iter (fun test_file ->
         match init_file ~sig_dir test_file with
         | `Nothing_to_seed -> ()
         | `Seeded n ->
           Printf.printf "seeded %s (%d pin%s)\n" test_file n
             (if n = 1 then "" else "s")
         | `Cannot_seed _ ->
           Printf.printf
             "skipped %s: does not typecheck against the live module (needs \
              migration, not a seed)\n"
             test_file)

(* ============================================================================
   migrate diff: show the canonical-projection difference between an old and a
   new signature. The common case (a pinned test) wires up "snapshot vs live"
   for you; --old/--new compares two scope_def JSON files directly.
   ============================================================================ *)

(* Structured canonical diff of two scope_defs, rendered to human lines. *)
let diff_scope_defs (old_sd : O.scope_def) (new_sd : O.scope_def) : string list =
  let old_cs = canonical_model old_sd and new_cs = canonical_model new_sd in
  pp_sig_diff old_cs new_cs (sig_diff old_cs new_cs)

let scope_def_of_file f =
  match parse_scope_defs (File.contents f) with
  | sd :: _ -> sd
  | [] -> failwith (Printf.sprintf "diff: %s contains no scope_def" f)

let migrate_diff old_file new_file sig_dir path _options =
  match old_file, new_file with
  | Some o, Some n ->
    (* explicit: diff two scope_def JSON files directly *)
    let d = diff_scope_defs (scope_def_of_file o) (scope_def_of_file n) in
    if d = [] then print_endline "no difference" else List.iter print_endline d
  | _ ->
    let path =
      match path with
      | Some p -> p
      | None -> failwith "diff: provide a PATH, or both --old and --new"
    in
    collect_catala_files path
    |> List.iter (fun test_file ->
           File.contents test_file
           |> parse_tests_textually
           |> List.iter (fun (pt : parsed_test) ->
                  match pt.pt_pin with
                  | None -> () (* unpinned: nothing to diff against *)
                  | Some pin ->
                    let target = Option.value ~default:"?" pt.pt_target in
                    let module_name, scope_base = split_target target in
                    let dir =
                      Option.value ~default:(Filename.dirname test_file) sig_dir
                    in
                    let snap = File.(dir / (pin ^ ".sig.json")) in
                    let header () =
                      Printf.printf "%s  (%s in %s)\n" target pt.pt_test_scope
                        (Filename.basename test_file)
                    in
                    if not (File.exists snap) then begin
                      header ();
                      Printf.printf "  (snapshot %s missing — cannot diff)\n"
                        (Filename.basename snap)
                    end
                    else (
                      match
                        live_scope_def ~test_file ~module_name ~scope_base
                      with
                      | Error reason ->
                        header ();
                        Printf.printf "  (cannot resolve live signature: %s)\n"
                          reason
                      | Ok live ->
                        let d = diff_scope_defs (scope_def_of_file snap) live in
                        if d <> [] then begin
                          header ();
                          List.iter (fun l -> Printf.printf "  %s\n" l) d
                        end)))

(* ============================================================================
   migrate apply (slice b): recover -> rewrite -> report -> write -> verify.

   For a STALE test (pin != live, snapshot present): synthesize the OLD module as
   a stub from the committed snapshot, read the test against ONLY that stub (so
   `> Using M` resolves to the old signature, not the drifted live one) to recover
   the old VALUES; run the pure rewrite (migrate_record) against (old, live); emit
   the migrated test (re-pinned, new snapshot), leaving holes as a readable
   `Unset`; then VERIFY by reading the result back against the LIVE module. With
   --dry-run, stop after the report (no files touched).
   ============================================================================ *)

(* Recover the old tests (with their values) by reading [test_file] against a
   throwaway stub synthesized from [old_sd]. The stub is the ONLY include, so the
   drifted live module is not consulted; stdlib still comes from the project build. *)
let recover_old_tests ~test_file ~(old_sd : O.scope_def) :
    (O.test list, string) result =
  let lang = Catala_utils.Cli.file_lang test_file in
  let ext = match lang with `Fr -> "catala_fr" | `En -> "catala_en" | _ -> "catala" in
  let tmp = Filename.temp_dir "catala_mig_" "" in
  let finally () =
    (try Array.iter (fun f -> Sys.remove (Filename.concat tmp f)) (Sys.readdir tmp)
     with _ -> ());
    try Sys.rmdir tmp with _ -> ()
  in
  Fun.protect ~finally @@ fun () ->
  List.iter
    (fun (modname, src) ->
      let p = Filename.concat tmp (Printf.sprintf "%s.%s" modname ext) in
      File.with_out_channel p (fun oc -> output_string oc src))
    (stub_modules lang old_sd);
  let content = File.contents test_file in
  let options =
    Global.enforce_options ~input_src:(Global.Contents (content, test_file)) ()
  in
  let path_to_build, _ = lookup_include_dirs options in
  try Ok (import_catala_tests (read_program [ Global.raw_file tmp ] path_to_build options))
  with e -> Error (Printexc.to_string e)

(* The values present in an input/output record (skip the unfilled ones). *)
let record_values (io_list : (string * O.test_io) list) :
    (string * O.runtime_value) list =
  List.filter_map
    (fun (n, (io : O.test_io)) ->
      match io.value with Some vd -> Some (n, vd.value) | None -> None)
    io_list

(* Rebuild a typed input/output record from migrated values + the new types. *)
let rebuild_record (new_fields : (string * O.typ) list)
    (vals : (string * O.runtime_value) list) : (string * O.test_io) list =
  List.map
    (fun (n, typ) ->
      let value =
        match List.assoc_opt n vals with
        | Some rv -> Some { O.value = rv; pos = None }
        | None -> None
      in
      n, { O.typ; value })
    new_fields

(* Migrate one recovered test old -> new; returns the migrated test + the steps. *)
let apply_to_test ~lang ?renames old_sd new_sd (t : O.test) : O.test * step list =
  let renames =
    match renames with Some r -> r | None -> signature_renames old_sd new_sd
  in
  let in_typs sd = List.map (fun (n, (si : O.scope_input)) -> n, si.typ) sd.O.inputs in
  let tag pre = List.map (fun (s : step) -> { s with path = pre ^ s.path }) in
  let new_in_vals, in_steps =
    migrate_record lang renames ~old_fields:(in_typs old_sd)
      ~new_fields:(in_typs new_sd) ~values:(record_values t.test_inputs) ()
  in
  let new_out_vals, out_steps =
    migrate_record lang renames ~old_fields:old_sd.O.outputs
      ~new_fields:new_sd.O.outputs ~values:(record_values t.test_outputs)
      ~added_is_hole:false ()
  in
  let new_test =
    {
      t with
      O.tested_scope = new_sd;
      test_inputs = rebuild_record (in_typs new_sd) new_in_vals;
      test_outputs = rebuild_record new_sd.O.outputs new_out_vals;
      sig_pin = Some (scope_signature_pin new_sd);
    }
  in
  new_test, tag "in." in_steps @ tag "out." out_steps

let pp_step (s : step) : string =
  match s.outcome with
  | Resolved (Renamed (a, b)) -> Printf.sprintf "  rename  %s: %s -> %s" s.path a b
  | Resolved Wrapped -> Printf.sprintf "  wrap    %s" s.path
  | Resolved Unwrapped -> Printf.sprintf "  unwrap  %s" s.path
  | Resolved Dropped -> Printf.sprintf "  drop    %s" s.path
  | NeedsResolving NeedsValue ->
    Printf.sprintf "  NEEDS VALUE     %s  (left unset)" s.path
  | NeedsResolving (NeedsDecision r) ->
    Printf.sprintf "  NEEDS DECISION  %s: %s" s.path r

(* Verify a (written) test file typechecks against the LIVE project. *)
let reads_against_live test_file : (unit, string) result =
  let options = Global.enforce_options ~input_src:(Global.FileName test_file) () in
  let path_to_build, include_dirs = lookup_include_dirs options in
  try
    ignore (import_catala_tests (read_program include_dirs path_to_build options));
    Ok ()
  with e -> Error (Printexc.to_string e)

(* The shared front half of every migration verb: locate the pinned tested scope
   of [test_file], resolve its live signature, and — if drifted — recover the old
   values and run the pure rewrite. Both [apply] (write) and [plan] (emit a
   worklist) consume this; only the back half differs. *)
type prep =
  | Not_pinned (* no pinned test in the file: silently ignore *)
  | Fresh (* pin = live: nothing to migrate *)
  | Skip of string (* couldn't prepare: reason (snapshot/resolve/recover) *)
  | Ready of {
      target : string; (* "<Module>.<Scope>" as written in the test (alias-safe) *)
      old_sd : O.scope_def;
      new_sd : O.scope_def;
      old_tests : O.test list; (* recovered values, for re-running under a plan *)
      migrated : (O.test * step list) list; (* under the auto rename map *)
      lang : Global.backend_lang;
    }

let prepare_file ~sig_dir test_file : prep =
  let parsed = parse_tests_textually (File.contents test_file) in
  let pinned =
    List.filter_map
      (fun pt ->
        match pt.pt_pin, pt.pt_target with
        | Some pin, Some target -> Some (pin, target)
        | _ -> None)
      parsed
  in
  match pinned with
  | [] -> Not_pinned
  | (pin, target) :: _ ->
    let module_name, scope_base = split_target target in
    let dir = Option.value ~default:(Filename.dirname test_file) sig_dir in
    let snap = File.(dir / (pin ^ ".sig.json")) in
    if not (File.exists snap) then
      Skip
        (Printf.sprintf "snapshot %s missing — cannot recover"
           (Filename.basename snap))
    else (
      match live_scope_def ~test_file ~module_name ~scope_base with
      | Error reason ->
        Skip (Printf.sprintf "cannot resolve live signature: %s" reason)
      | Ok new_sd ->
        let old_sd = scope_def_of_file snap in
        if
          String.equal (scope_signature_hash old_sd)
            (scope_signature_hash new_sd)
        then Fresh
        else (
          match recover_old_tests ~test_file ~old_sd with
          | Error e -> Skip (Printf.sprintf "value recovery failed: %s" e)
          | Ok old_tests ->
            let lang = Catala_utils.Cli.file_lang test_file in
            let migrated =
              List.map (apply_to_test ~lang old_sd new_sd) old_tests
            in
            Ready { target; old_sd; new_sd; old_tests; migrated; lang }))

(* Minimal navigation over the parsed plan (we control the format, so match the
   raw constructors directly rather than pull in otoml's path API). *)
let toml_assoc k = function
  | Otoml.TomlTable kvs | Otoml.TomlInlineTable kvs -> List.assoc_opt k kvs
  | _ -> None

let toml_as_array = function
  | Otoml.TomlArray l | Otoml.TomlTableArray l -> l
  | _ -> []

let toml_str k t =
  match toml_assoc k t with Some (Otoml.TomlString s) -> Some s | _ -> None

let toml_migrations content =
  let toml = Otoml.Parser.from_string content in
  match toml_assoc "migration" toml with Some v -> toml_as_array v | None -> []

(* The human's decisions, for `apply --plan` to execute. One per cluster:
   the confirmed renames (the rename map override) and the fills that carry a
   value (raw Catala text, to be parsed against live). Transforms are step 2. *)
type plan_decisions = {
  pd_file : string;
  pd_scope : string;
  pd_renames : (string * string) list; (* confirmed (from, to) *)
  pd_fills : (string * string) list; (* path, value-text (value present only) *)
}

let parse_plan_decisions_string (content : string) : plan_decisions list =
  List.map
    (fun m ->
      let arr k = match toml_assoc k m with Some v -> toml_as_array v | None -> [] in
      let renames =
        List.filter_map
          (fun t ->
            match toml_assoc "confirm" t, toml_str "from" t, toml_str "to" t with
            | Some (Otoml.TomlBoolean true), Some f, Some tto -> Some (f, tto)
            | _ -> None)
          (arr "rename")
      in
      let fills =
        List.filter_map
          (fun t ->
            match toml_str "path" t, toml_str "value" t with
            | Some p, Some v when String.trim v <> "" -> Some (p, v)
            | _ -> None)
          (arr "fill")
      in
      {
        pd_file = Option.value ~default:"?" (toml_str "file" m);
        pd_scope = Option.value ~default:"?" (toml_str "scope" m);
        pd_renames = renames;
        pd_fills = fills;
      })
    (toml_migrations content)

(* A plan fill targets a top-level input when its path is exactly "in.<name>"
   (nested fills and output fills are not placed by step 1; they stay holes). *)
let toplevel_input_fill path =
  match String.split_on_char '.' path with [ "in"; name ] -> Some name | _ -> None

(* Parse top-level input fills (raw Catala value text) into typed runtime_values
   by reading a synthesized probe test against the LIVE module: a throwaway test
   scope that sets only the filled inputs. The compiler does the parsing AND the
   typechecking — an ill-typed fill surfaces as a read error (a feature, not a
   crash). [target] is the tested scope token exactly as written in the test, so
   an aliased `> Using M as O` import still resolves. *)
let parse_fill_values ~target ~test_file (fills : (string * string) list) :
    ((string * O.runtime_value) list, string) result =
  if fills = [] then Ok []
  else
    let headers =
      File.contents test_file
      |> String.split_on_char '\n'
      |> List.filter (fun l ->
             let l = String.trim l in
             String.length l > 0 && l.[0] = '>')
    in
    let defs =
      List.map
        (fun (n, v) -> Printf.sprintf "  definition probe.%s equals %s" n v)
        fills
      |> String.concat "\n"
    in
    let content =
      String.concat "\n" headers
      ^ "\n\n```catala-metadata\n#[test]\n#[testcase.testui]\n\
         declaration scope Migrate_fill_probe:\n  output probe scope " ^ target
      ^ "\n```\n\n```catala\nscope Migrate_fill_probe:\n" ^ defs ^ "\n```\n"
    in
    let options =
      Global.enforce_options ~input_src:(Global.Contents (content, test_file)) ()
    in
    let path_to_build, include_dirs = lookup_include_dirs options in
    try
      match import_catala_tests (read_program include_dirs path_to_build options) with
      | [] -> Error "fill probe produced no test"
      | t :: _ ->
        Ok
          (List.filter_map
             (fun (n, _) ->
               match List.assoc_opt n t.O.test_inputs with
               | Some ({ value = Some vd; _ } : O.test_io) -> Some (n, vd.value)
               | _ -> None)
             fills)
    with e -> Error (Printexc.to_string e)

(* Drop the parsed fill value into each test's matching hole (a top-level input
   left Unset by the rewrite). Only fills an actual hole — never overwrites a
   recovered value. *)
let fill_holes (parsed : (string * O.runtime_value) list)
    ((t, steps) : O.test * step list) : O.test * step list =
  let inputs =
    List.map
      (fun (name, (io : O.test_io)) ->
        match List.assoc_opt name parsed, io.value with
        | Some rv, Some { O.value = { O.value = O.Unset; _ }; _ } ->
          name, { io with value = Some { O.value = rv; pos = None } }
        | _ -> name, io)
      t.test_inputs
  in
  { t with test_inputs = inputs }, steps

let migrate_apply dry_run plan sig_dir path _options =
  let decisions =
    match plan with
    | None -> []
    | Some f -> parse_plan_decisions_string (File.contents f)
  in
  collect_catala_files path
  |> List.iter (fun test_file ->
         let base = Filename.basename test_file in
         match prepare_file ~sig_dir test_file with
         | Not_pinned | Fresh -> ()
         | Skip msg -> Printf.printf "%s: %s\n" base msg
         | Ready { target; old_sd; new_sd; old_tests; migrated; lang } ->
           (* Find this file's plan cluster (if any). Under a plan, the confirmed
              renames REPLACE the auto-detected map, so re-run the rewrite. *)
           let cluster =
             List.find_opt
               (fun d -> String.equal (Filename.basename d.pd_file) base)
               decisions
           in
           let migrated =
             match cluster with
             | Some d ->
               List.map
                 (apply_to_test ~lang ~renames:d.pd_renames old_sd new_sd)
                 old_tests
             | None -> migrated
           in
           (* Parse the plan's top-level input fills against live, then place
              them into the holes. An unparseable fill is reported and left a
              hole (never written broken). *)
           let parsed_fills, fill_err =
             match cluster with
             | None -> [], None
             | Some d -> (
               let kv =
                 List.filter_map
                   (fun (p, v) ->
                     Option.map (fun n -> n, v) (toplevel_input_fill p))
                   d.pd_fills
               in
               match parse_fill_values ~target ~test_file kv with
               | Ok parsed -> parsed, None
               | Error e -> [], Some e)
           in
           let migrated = List.map (fill_holes parsed_fills) migrated in
           let all_steps = List.concat_map snd migrated in
           Printf.printf "%s  (%s -> %s)\n" base (scope_signature_pin old_sd)
             (scope_signature_pin new_sd);
           List.iter (fun s -> print_endline (pp_step s)) all_steps;
           Option.iter
             (fun e ->
               Printf.printf
                 "  WARNING: plan fill value(s) did not parse, left as holes: \
                  %s\n"
                 e)
             fill_err;
           let count pred = List.length (List.filter pred all_steps) in
           let is_needs_value (s : step) = s.outcome = NeedsResolving NeedsValue in
           let n_decisions =
             count (fun (s : step) ->
                 match s.outcome with
                 | NeedsResolving (NeedsDecision _) -> true
                 | _ -> false)
           in
           let filled_paths = List.map (fun (n, _) -> "in." ^ n) parsed_fills in
           let n_filled =
             count (fun s -> is_needs_value s && List.mem s.path filled_paths)
           in
           let n_todo = count is_needs_value - n_filled in
           let filled_note =
             if n_filled > 0 then Printf.sprintf " %d filled from plan;" n_filled
             else ""
           in
           if dry_run then
             Printf.printf
               "  (dry-run: no files written;%s %d to fill, %d need a decision)\n"
               filled_note n_todo n_decisions
           else if n_decisions > 0 then
             (* A NeedsDecision slot can't be written without destroying the old
                value it needs as a transform input. Leave the whole test STALE
                (untouched): old values stay intact and re-recoverable, and the
                gate keeps flagging it. (Transforms are resolved in a later
                slice; the plan's fn is not executed here.) *)
             Printf.printf
               "  left STALE: %d item(s) need a decision (a transform) — not \
                writing, old values preserved\n"
               n_decisions
           else begin
             let new_tests = List.map fst migrated in
             let dir =
               Option.value ~default:(Filename.dirname test_file) sig_dir
             in
             write_sig_snapshot dir new_sd;
             File.with_out_channel test_file (fun oc ->
                 let ppf = Format.formatter_of_out_channel oc in
                 emit_test_list ppf lang new_tests;
                 Format.pp_print_flush ppf ());
             match reads_against_live test_file with
             | Ok () ->
               Printf.printf "  written + re-pinned; verified against live%s%s\n"
                 (if n_filled > 0 then Printf.sprintf " (%d filled from plan)" n_filled
                  else "")
                 (if n_todo > 0 then
                    Printf.sprintf
                      " (%d slot(s) to fill, marked #[testcase.todo])" n_todo
                  else "")
             | Error e ->
               Printf.printf
                 "  WARNING: written but does NOT read back against live: %s\n" e
           end)

(* ============================================================================
   migrate plan: the migration as an editable, persistent artifact (TOML).

   `apply` does everything FORCED automatically; the plan is where the human
   answers the few things that aren't: confirm/reject a suggested rename, give a
   value to a new input (or leave a #[testcase.todo] hole), and point a
   NeedsDecision at a Catala transform. One plan groups every drifted cluster
   under PATH; it is the durable progress ledger (replacing the by-hand
   spreadsheet) — `migrate plan` regenerates the worklist, `migrate status
   --plan` shows resolution progress, and (later) `apply --plan` consumes it.

   The four sections mirror the rewrite's [outcome] ADT:
     auto       <- Resolved   (applied automatically; shown for review only)
     [[rename]] <- Resolved (Renamed)      confirm = true | false
     [[fill]]   <- NeedsValue               value = "..."  | todo = true
     [[transform]] <- NeedsDecision         fn = "Mod.fn"  | "?"  (still pending)
   ============================================================================ *)

type plan_rename = { pr_kind : string; pr_from : string; pr_to : string }
type plan_fill = { pf_path : string; pf_type : string }

type plan_transform = {
  ptr_produces : string list;
  ptr_consumes : string list;
  ptr_reason : string;
}

type plan_cluster = {
  pc_file : string;
  pc_scope : string;
  pc_from : string;
  pc_to : string;
  pc_auto : string list;
  pc_renames : plan_rename list;
  pc_fills : plan_fill list;
  pc_transforms : plan_transform list;
}

(* Strip the "@<hash>" tail of a pin to get the bare "<Module>.<Scope>". *)
let scope_of_pin p =
  match String.index_opt p '@' with Some k -> String.sub p 0 k | None -> p

let build_plan_cluster ~sig_dir test_file : plan_cluster option =
  match prepare_file ~sig_dir test_file with
  | Not_pinned | Fresh | Skip _ -> None
  | Ready { old_sd; new_sd; migrated; _ } ->
    let all_steps = List.concat_map snd migrated in
    let auto_str path = function
      | Renamed (a, b) -> Printf.sprintf "rename %s: %s -> %s" path a b
      | Wrapped -> "wrap " ^ path
      | Unwrapped -> "unwrap " ^ path
      | Dropped -> "drop " ^ path
    in
    (* Steps repeat across the cluster's tests; the plan is cluster-wide, so
       collapse each worklist item to one row (sorted, deduped). *)
    let auto =
      List.filter_map
        (fun (s : step) ->
          match s.outcome with Resolved r -> Some (auto_str s.path r) | _ -> None)
        all_steps
      |> List.sort_uniq String.compare
    in
    (* Type hint for a fill row: easy for a top-level in./out. field. *)
    let fill_type path =
      match String.split_on_char '.' path with
      | [ "in"; name ] -> (
        match List.assoc_opt name new_sd.O.inputs with
        | Some (si : O.scope_input) -> tystr si.typ
        | None -> "")
      | [ "out"; name ] -> (
        match List.assoc_opt name new_sd.O.outputs with
        | Some t -> tystr t
        | None -> "")
      | _ -> ""
    in
    let fills =
      List.filter_map
        (fun (s : step) ->
          if s.outcome = NeedsResolving NeedsValue then Some s.path else None)
        all_steps
      |> List.sort_uniq String.compare
      |> List.map (fun path -> { pf_path = path; pf_type = fill_type path })
    in
    let transforms =
      List.filter_map
        (fun (s : step) ->
          match s.outcome with
          | NeedsResolving (NeedsDecision r) -> Some (s.path, r)
          | _ -> None)
        all_steps
      |> List.sort_uniq compare
      |> List.map (fun (path, reason) ->
             {
               ptr_produces = [ path ];
               ptr_consumes = [ path ];
               (* default: the same slot; the dev widens if the transform reads
                  more of the old record *)
               ptr_reason = reason;
             })
    in
    let renames =
      let oc = canonical_model old_sd and nc = canonical_model new_sd in
      detect_renames oc nc (structural_diff oc nc)
      |> List.map (fun (k, o, n) ->
             {
               pr_kind = (match k with `Struct -> "struct" | `Enum -> "enum");
               pr_from = o;
               pr_to = n;
             })
    in
    Some
      {
        pc_file = test_file;
        pc_scope = scope_of_pin (scope_signature_pin new_sd);
        pc_from = scope_signature_pin old_sd;
        pc_to = scope_signature_pin new_sd;
        pc_auto = auto;
        pc_renames = renames;
        pc_fills = fills;
        pc_transforms = transforms;
      }

let toml_of_clusters (clusters : plan_cluster list) : Otoml.t =
  let s x = Otoml.TomlString x in
  let arr_str l = Otoml.TomlArray (List.map s l) in
  let rename_tbl r =
    Otoml.TomlTable
      [
        "kind", s r.pr_kind;
        "from", s r.pr_from;
        "to", s r.pr_to;
        "confirm", Otoml.TomlBoolean true;
      ]
  in
  let fill_tbl f =
    Otoml.TomlTable
      [ "path", s f.pf_path; "type", s f.pf_type; "todo", Otoml.TomlBoolean true ]
  in
  let transform_tbl t =
    Otoml.TomlTable
      [
        "produces", arr_str t.ptr_produces;
        "consumes", arr_str t.ptr_consumes;
        "reason", s t.ptr_reason;
        "fn", s "?";
      ]
  in
  let cluster_tbl c =
    Otoml.TomlTable
      [
        "file", s c.pc_file;
        "scope", s c.pc_scope;
        "from", s c.pc_from;
        "to", s c.pc_to;
        "auto", arr_str c.pc_auto;
        "rename", Otoml.TomlTableArray (List.map rename_tbl c.pc_renames);
        "fill", Otoml.TomlTableArray (List.map fill_tbl c.pc_fills);
        "transform", Otoml.TomlTableArray (List.map transform_tbl c.pc_transforms);
      ]
  in
  Otoml.TomlTable
    [ "migration", Otoml.TomlTableArray (List.map cluster_tbl clusters) ]

let plan_header =
  "# Migration plan — generated by `catala testcase migrate plan`.\n\
   # Resolve the open work, then re-run `migrate status --plan <this-file>` to\n\
   # track progress (and, later, `migrate apply --plan <this-file>` to write).\n\
   #   auto        applied automatically — review only, do not edit\n\
   #   [[rename]]  set confirm = false to reject a suggested rename\n\
   #   [[fill]]    add  value = \"...\"  for a new input, or leave todo = true\n\
   #   [[transform]]  set  fn = \"Module.scope\"  (write it in migrations/)\n\n"

let plan_to_string (clusters : plan_cluster list) : string =
  plan_header ^ Otoml.Printer.to_string (toml_of_clusters clusters)

let migrate_plan out sig_dir path _options =
  let clusters =
    collect_catala_files path |> List.filter_map (build_plan_cluster ~sig_dir)
  in
  if clusters = [] then
    prerr_endline "migrate plan: nothing to migrate (all fresh, or unpinned)"
  else
    let text = plan_to_string clusters in
    match out with
    | None -> print_string text
    | Some f ->
      File.with_out_channel f (fun oc -> output_string oc text);
      let n = List.length clusters in
      Printf.eprintf "wrote %s (%d cluster%s)\n" f n (if n = 1 then "" else "s")

(* ============================================================================
   migrate status --plan: progress against a plan (the spreadsheet's progress
   bar). A row is RESOLVED when it no longer needs human attention:
     rename     confirm is a bool (we always emit it pre-confirmed)
     fill       a non-empty value = "..."  (todo = true alone is still pending)
     transform  fn set to something other than "?"
   With --check, exit non-zero while any fill/transform is still pending (the
   `?`-worklist gate).
   ============================================================================ *)

type plan_progress = {
  pp_scope : string;
  pp_file : string;
  pp_renames : int * int; (* done, total *)
  pp_fills : int * int;
  pp_transforms : int * int;
}

let parse_plan_progress_string (content : string) : plan_progress list =
  let table_assoc = toml_assoc and as_array = toml_as_array and str_field = toml_str in
  let count_done done_p l = List.length (List.filter done_p l), List.length l in
  let migs = toml_migrations content in
  List.map
    (fun m ->
      let arr k = match table_assoc k m with Some v -> as_array v | None -> [] in
      let rename_done t =
        match table_assoc "confirm" t with
        | Some (Otoml.TomlBoolean _) -> true
        | _ -> false
      in
      let fill_done t =
        match str_field "value" t with
        | Some v -> String.trim v <> ""
        | None -> false
      in
      let transform_done t =
        match str_field "fn" t with
        | Some v -> String.trim v <> "" && String.trim v <> "?"
        | None -> false
      in
      {
        pp_scope = Option.value ~default:"?" (str_field "scope" m);
        pp_file = Option.value ~default:"?" (str_field "file" m);
        pp_renames = count_done rename_done (arr "rename");
        pp_fills = count_done fill_done (arr "fill");
        pp_transforms = count_done transform_done (arr "transform");
      })
    migs

let parse_plan_progress plan_file : plan_progress list =
  parse_plan_progress_string (File.contents plan_file)

let report_plan_progress ~check plan_file =
  let progs = parse_plan_progress plan_file in
  let pending_of p =
    let pend (d, t) = t - d in
    pend p.pp_fills + pend p.pp_transforms
  in
  List.iter
    (fun p ->
      let d (a, _) = a and t (_, b) = b in
      let total = t p.pp_renames + t p.pp_fills + t p.pp_transforms in
      let done_ = d p.pp_renames + d p.pp_fills + d p.pp_transforms in
      Printf.printf "%s  (%s)  %d/%d resolved\n" p.pp_scope
        (Filename.basename p.pp_file) done_ total;
      Printf.printf "  renames     %d/%d\n" (d p.pp_renames) (t p.pp_renames);
      let pend (a, b) = b - a in
      Printf.printf "  fills       %d/%d%s\n" (d p.pp_fills) (t p.pp_fills)
        (if pend p.pp_fills > 0 then
           Printf.sprintf "   (%d still #[testcase.todo])" (pend p.pp_fills)
         else "");
      Printf.printf "  transforms  %d/%d%s\n" (d p.pp_transforms)
        (t p.pp_transforms)
        (if pend p.pp_transforms > 0 then
           Printf.sprintf "   (%d awaiting a Catala fn)" (pend p.pp_transforms)
         else ""))
    progs;
  let pending = List.fold_left (fun a p -> a + pending_of p) 0 progs in
  if check && pending > 0 then exit 1

(* `migrate status`: drift triage by default, or — with --plan FILE — progress
   against that plan. (Dispatcher; [migrate_status] above is the triage half.) *)
let migrate_status_cmd check json plan sig_dir path options =
  match plan with
  | Some plan_file -> report_plan_progress ~check plan_file
  | None -> migrate_status check json sig_dir path options
