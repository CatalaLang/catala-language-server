(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria, contributor:
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
(****************************************************************)

open Server_state
open Catala_utils
open Utils
open Shared_ast
open Linol_lwt
open Surface
open Server_types

let _lookup_suggestions doc_id diagnostics range =
  let*? rmap = Doc_id.Map.find_opt doc_id diagnostics in
  Range.Map.find_opt range rmap
  |> function
  | None -> None
  | Some { range; lsp_error; _ } -> (
    let*? lsp_error = lsp_error in
    match lsp_error.suggestion with
    | None | Some [] -> None
    | Some suggs -> Some (range, suggs))

let _lookup_suggestions_by_pos doc_id diagnostics range =
  let open Linol_lwt in
  _lookup_suggestions doc_id diagnostics range
  |> function
  | None -> None
  | Some (range, completions) ->
    Some
      (List.map
         (fun sugg ->
           let textEdit = `TextEdit (TextEdit.create ~range ~newText:sugg) in
           CompletionItem.create ~kind:Keyword ~label:sugg ~textEdit ())
         completions)

let[@ocaml.warning "-32"] pp_token =
 fun fmt (lex, _, pos) ->
  Format.fprintf fmt "%s:%s" lex Pos.(from_lpos pos |> to_string_shorter)

let lex_line (doc : document_state) ~content (pos : Position.t) =
  let lexbuf = Sedlexing.Utf8.from_string content in
  let lexer =
    match doc.locale with
    | En -> Lexer_en.lexer
    | Fr -> Lexer_fr.lexer
    | Pl -> Lexer_pl.lexer
  in
  let real_lpos = pos.line + 1 in
  let line_tokens =
    try
      Lexer_common.with_lexing_context (doc.document_id :> string)
      @@ fun () ->
      let rec loop acc =
        let token = lexer lexbuf in
        let ((l, _r) as lpos) = Sedlexing.lexing_positions lexbuf in
        let lnum = l.Lexing.pos_lnum in
        if token = EOF || lnum > real_lpos then Option.map List.rev acc
        else if lnum < pos.line then
          (* skip *)
          loop acc
        else if l.Lexing.pos_lnum = real_lpos then
          if !Lexer_common.context <> Code then loop acc
          else
            let elt = Sedlexing.Utf8.lexeme lexbuf, token, lpos in
            match acc with
            | None -> loop (Some [elt])
            | Some acc -> loop (Some (elt :: acc))
        else loop acc
      in
      let r = loop None in
      (* Flush potential delayed errors *)
      (try Message.report_delayed_errors_if_any () with _ -> ());
      r
    with e ->
      begin
        (match e with
        | Message.CompilerError c ->
          Log.warn (fun m ->
              m "lexing exception: %t" (fun ppf ->
                  Message.Content.emit ~ppf c Error))
        | Message.CompilerErrors l ->
          Log.warn (fun m ->
              List.iter
                (fun (c, _) ->
                  m "lexing exception: %t" (fun ppf ->
                      Message.Content.emit ~ppf c Error))
                l)
        | e ->
          Log.warn (fun m ->
              m "uncaught lexing exception: %s" (Printexc.to_string e)));
        None
      end
  in
  line_tokens

let retrieve_tokens (doc : document_state) ~content (pos : Position.t) =
  let*? line = lex_line (doc : document_state) ~content (pos : Position.t) in
  let real_cpos = pos.Position.character + 1 in
  let rec loop rev_preds = function
    | ((_, _, (l, r)) as tok) :: tl ->
      let lc = l.Lexing.pos_cnum - l.Lexing.pos_bol + 1 in
      let rc = r.Lexing.pos_cnum - r.Lexing.pos_bol + 1 in
      if lc <= real_cpos && real_cpos <= rc then
        let trim_token (_lexem, t, p) =
          let trim_str s = String.sub s 0 (real_cpos - lc) in
          match t with
          | Tokens.UIDENT s ->
            let ts = trim_str s in
            ts, Tokens.UIDENT ts, p
          | LIDENT s ->
            let ts = trim_str s in
            ts, LIDENT ts, p
          | _ -> tok
        in
        trim_token tok :: rev_preds, tl
      else loop (tok :: rev_preds) tl
    | [] -> (* shouldn't happen *) rev_preds, []
  in
  Some (loop [] line)

let lookup_alias_name =
  let en_names =
    [
      "Date";
      "Duration";
      "MonthYear";
      "Period";
      "Money";
      "Integer";
      "Decimal";
      "List";
    ]
  in
  let en_aliases = List.map (fun s -> s ^ "_en") en_names in
  let fr_aliases = List.map (fun s -> s ^ "_fr") en_names in
  let fr_names =
    [
      "Date";
      "Durée";
      "MoisAnnée";
      "Période";
      "Argent";
      "Entier";
      "Décimal";
      "Liste";
    ]
  in
  let en_alias_map = String.Map.of_list (List.combine en_aliases en_names) in
  let fr_alias_map = String.Map.of_list (List.combine fr_aliases fr_names) in
  let lookup_alias_name lang s =
    match lang with
    | Catala_utils.Global.Fr -> String.Map.find s fr_alias_map
    | Catala_utils.Global.En | _ -> String.Map.find s en_alias_map
  in
  lookup_alias_name

type completion = {
  label : string;
  detail : string;
  kind : CompletionItemKind.t;
  documentation :
    [ `MarkupContent of MarkupContent.t | `String of string ] option;
}

module Kind = struct
  type t =
    | Module of ModuleName.t
    | StdlibModule of (ModuleName.t * string (* alias *))
    | Struct of StructName.t
    | Enum of EnumName.t
    | Constructor of EnumConstructor.t
    | Scope of ScopeName.t
    | Topdef of {
        name : TopdefName.t;
        ty : Shared_ast.typ;
        extra_doc : string option;
      }
    | ScopeVar of { name : ScopeVar.t; ty : Shared_ast.typ }
    | StructField of { name : StructField.t; ty : Shared_ast.typ }
    | Keyword of { lexeme : string; token : Tokens.token }

  let to_string = function
    | Module m -> ModuleName.to_string m
    | StdlibModule (_m, alias) -> alias
    | Struct s -> StructName.base s
    | Enum e -> EnumName.base e
    | Constructor c -> EnumConstructor.to_string c
    | Scope s -> ScopeName.base s
    | Topdef { name; _ } -> TopdefName.base name
    | ScopeVar { name; _ } -> ScopeVar.to_string name
    | StructField { name; _ } -> StructField.to_string name
    | Keyword { lexeme; _ } -> lexeme

  let[@ocaml.warning "-32"] format ppf k =
    match k with
    | Module _ -> Format.fprintf ppf "Module(%s)" (to_string k)
    | StdlibModule _ -> Format.fprintf ppf "StdlibModule(%s)" (to_string k)
    | Struct _ -> Format.fprintf ppf "Struct(%s)" (to_string k)
    | Enum _ -> Format.fprintf ppf "Enum(%s)" (to_string k)
    | Constructor _ -> Format.fprintf ppf "Constructor(%s)" (to_string k)
    | Scope _ -> Format.fprintf ppf "Scope(%s)" (to_string k)
    | Topdef _ -> Format.fprintf ppf "Topdef(%s)" (to_string k)
    | ScopeVar { ty; _ } ->
      Format.fprintf ppf "ScopeVar(%s, %a)" (to_string k) Shared_ast.Print.typ
        ty
    | StructField { ty; _ } ->
      Format.fprintf ppf "StructField(%s, %a)" (to_string k)
        Shared_ast.Print.typ ty
    | Keyword { lexeme; _ } -> Format.fprintf ppf "Keyword(%s)" lexeme

  let itemkind : t -> completion =
   fun k ->
    let doc ?extra_doc ty =
      let extra_doc =
        match extra_doc with None -> "" | Some s -> Format.sprintf "\n\n%s" s
      in
      Some
        (`MarkupContent
           (MarkupContent.create ~kind:MarkupKind.Markdown
              ~value:
                (Format.asprintf "`%a`%s" Shared_ast.Print.typ ty extra_doc)))
    in
    let label, detail, kind, documentation =
      match k with
      | Module m ->
        ModuleName.to_string m, "Module", CompletionItemKind.Module, None
      | StdlibModule (_m, alias) -> alias, "Stdlib", Module, None
      | Struct s -> StructName.base s, "Structure", Struct, None
      | Enum e -> EnumName.base e, "Enumeration", Enum, None
      | Constructor c ->
        EnumConstructor.to_string c, "Constructor", Constructor, None
      | Scope s -> ScopeName.base s, "Scope", Function, None
      | Topdef { name; ty; extra_doc } ->
        TopdefName.base name, "Function", Function, doc ?extra_doc ty
      | ScopeVar { name; ty } ->
        ScopeVar.to_string name, "Variable", Variable, doc ty
      | StructField { name; ty } ->
        StructField.to_string name, "Field", Field, doc ty
      | Keyword { lexeme; _ } -> lexeme, "Keyword", Keyword, None
    in
    { label; detail; kind; documentation }

  let to_completion : t -> Linol_lwt.CompletionItem.t =
   fun k ->
    let { label; detail; kind; documentation } = itemkind k in
    CompletionItem.create ~label ~detail ~kind ?documentation ()

  module Set = struct
    include Set.Make (struct
      type nonrec t = t

      let compare l r =
        match l, r with
        | Module l, Module r -> ModuleName.compare l r
        | StdlibModule (l, _), StdlibModule (r, _) -> ModuleName.compare l r
        | Struct l, Struct r -> StructName.compare l r
        | Enum l, Enum r -> EnumName.compare l r
        | Constructor l, Constructor r -> EnumConstructor.compare l r
        | Scope l, Scope r -> ScopeName.compare l r
        | Topdef { name = l; _ }, Topdef { name = r; _ } ->
          TopdefName.compare l r
        | ScopeVar { name = l; _ }, ScopeVar { name = r; _ } ->
          ScopeVar.compare l r
        | l, r -> compare l r
    end)
  end
end

module CompletionMap = String.Map

type prefix =
  | PartialUident of string
  | PartialLident of string
  | FullUident of string
  | FullLident of string

let update ?prefix kind m =
  let name = Kind.to_string kind in
  match prefix with
  | Some (FullLident f | FullUident f) when not (String.equal name f) -> m
  | Some (PartialLident prefix | PartialUident prefix)
    when not (String.starts_with ~prefix name) ->
    m
  | None | Some (FullLident _ | FullUident _ | PartialLident _ | PartialUident _)
    ->
    String.Map.update name
      (function
        | None -> Some (Kind.Set.singleton kind)
        | Some s -> Some (Kind.Set.add kind s))
      m

let update_all ?prefix map kinds =
  Kind.Set.fold (fun k m -> update ?prefix k m) kinds map

let lookup_modules ?prefix (prg : typed Scopelang.Ast.program) m =
  let stdlib_modules, nonstdlib_modules =
    ModuleName.Map.partition
      (fun _m -> function
        | { intf_id = { is_stdlib = true; _ }; _ } -> true
        | { intf_id = { is_stdlib = false; _ }; _ } -> false)
      prg.program_ctx.ctx_modules
  in
  let stdlib_modules =
    ModuleName.Map.to_seq stdlib_modules
    |> Seq.filter_map (fun (m, _) ->
        if String.starts_with ~prefix:"Stdlib_" (ModuleName.to_string m) then
          None
        else
          let alias =
            lookup_alias_name prg.program_lang (ModuleName.to_string m)
          in
          Some (Kind.StdlibModule (m, alias)))
    |> Kind.Set.of_seq
  in
  let nonstdlib_modules =
    ModuleName.Map.to_seq nonstdlib_modules
    |> Seq.map (fun (m, _) -> Kind.Module m)
    |> Kind.Set.of_seq
  in
  update_all ?prefix m (Kind.Set.union stdlib_modules nonstdlib_modules)

let lookup_scopes ?prefix ?modname (prg : typed Scopelang.Ast.program) m =
  let scopes =
    match modname with
    | None -> prg.program_scopes
    | Some m -> (
      ModuleName.Map.find_opt m prg.program_modules
      |> function None -> ScopeName.Map.empty | Some scopes -> scopes)
  in
  ScopeName.Map.to_seq scopes
  |> Seq.map (fun (s, _) -> Kind.Scope s)
  |> Kind.Set.of_seq
  |> update_all ?prefix m

let lookup_structs ?prefix ?modname (prg : typed Scopelang.Ast.program) m =
  let structs =
    match modname with
    | None -> prg.program_ctx.ctx_structs
    | Some m ->
      StructName.Map.filter_map
        (fun s v ->
          let*? m' =
            StructName.path s
            |> List.rev
            |> function [] -> None | h :: _ -> Some h
          in
          if ModuleName.equal m m' then Some v else None)
        prg.program_ctx.ctx_structs
  in
  StructName.Map.to_seq structs
  |> Seq.map (fun (s, _) -> Kind.Struct s)
  |> Kind.Set.of_seq
  |> update_all ?prefix m

let lookup_struct_fields
    ?prefix
    ~structname
    (prg : typed Scopelang.Ast.program)
    m =
  let r = StructName.Map.find_opt structname prg.program_ctx.ctx_structs in
  match r with
  | None -> m
  | Some fields ->
    StructField.Map.to_seq fields
    |> Seq.map (fun (name, ty) -> Kind.StructField { name; ty })
    |> Kind.Set.of_seq
    |> update_all ?prefix m

let lookup_enums ?prefix ?modname (prg : typed Scopelang.Ast.program) m =
  let enums =
    match modname with
    | None -> prg.program_ctx.ctx_enums
    | Some m ->
      EnumName.Map.filter_map
        (fun s v ->
          let*? m' =
            EnumName.path s
            |> List.rev
            |> function [] -> None | h :: _ -> Some h
          in
          if ModuleName.equal m m' then Some v else None)
        prg.program_ctx.ctx_enums
  in
  EnumName.Map.to_seq enums
  |> Seq.map (fun (s, _) -> Kind.Enum s)
  |> Kind.Set.of_seq
  |> update_all ?prefix m

let lookup_constructors ?prefix ?enum_name (prg : typed Scopelang.Ast.program) m
    =
  let constructors =
    match enum_name with
    | None ->
      Ident.Map.fold
        (fun _id enum_map acc ->
          EnumName.Map.to_seq enum_map
          |> Seq.map snd
          |> fun sq -> EnumConstructor.Set.add_seq sq acc)
        prg.program_ctx.ctx_enum_constrs EnumConstructor.Set.empty
    | Some e -> (
      let enum_opt = EnumName.Map.find_opt e prg.program_ctx.ctx_enums in
      match enum_opt with
      | None -> EnumConstructor.Set.empty
      | Some cs ->
        EnumConstructor.Map.to_seq cs
        |> Seq.map fst
        |> EnumConstructor.Set.of_seq)
  in
  EnumConstructor.Set.to_seq constructors
  |> Seq.map (fun s -> Kind.Constructor s)
  |> Kind.Set.of_seq
  |> update_all ?prefix m

let lookup_topdefs ?prefix ?modname (prg : typed Scopelang.Ast.program) m =
  let extract_doc td =
    TopdefName.get_info td
    |> Mark.get
    |> Pos.attrs
    |> List.filter_map (function Doc (d, _) -> Some d | _ -> None)
    |> String.concat "\n"
    |> Option.some
  in
  let topdefs =
    match modname with
    | None ->
      (* Discard topdefs from other modules -- in the future, we would like to
         insert the required module *)
      TopdefName.Map.filter_map
        (fun td (typ, _vis) ->
          let extra_doc = extract_doc td in
          if TopdefName.path td = [] then Some (typ, extra_doc) else None)
        prg.program_ctx.ctx_topdefs
    | Some m ->
      TopdefName.Map.filter_map
        (fun td (typ, _vis) ->
          let*? m' =
            TopdefName.path td
            |> List.rev
            |> function [] -> None | h :: _ -> Some h
          in
          let extra_doc = extract_doc td in
          if ModuleName.equal m m' then Some (typ, extra_doc) else None)
        prg.program_ctx.ctx_topdefs
  in
  TopdefName.Map.to_seq topdefs
  |> Seq.map (fun (name, (ty, extra_doc)) ->
      Kind.Topdef { name; ty; extra_doc })
  |> Kind.Set.of_seq
  |> update_all ?prefix m

let lookup_scopevars ?prefix (prg : typed Scopelang.Ast.program) m =
  ScopeName.Map.to_seq prg.program_scopes
  |> Seq.map (fun (_, sdecl) ->
      ScopeVar.Map.to_seq (Mark.remove sdecl).Scopelang.Ast.scope_sig
      |> Seq.concat_map (fun (name, (ty : Scopelang.Ast.scope_var_ty)) ->
          List.to_seq
          @@ List.sort_uniq compare
               [
                 Kind.ScopeVar { name; ty = ty.svar_in_ty };
                 Kind.ScopeVar { name; ty = ty.svar_out_ty };
               ]))
  |> Seq.concat
  |> Kind.Set.of_seq
  |> update_all ?prefix m

let find_qualifiers prg uident =
  let prefix = FullUident uident in
  CompletionMap.empty |> lookup_enums ~prefix prg |> lookup_modules ~prefix prg

let follow_ups ?prefix prg (kind : Kind.t) m =
  match kind, prefix with
  | _, Some (FullUident _ | FullLident _) -> (* Not a partial prefix *) m
  | Enum _, Some (PartialLident _) -> m
  | (Module modname | StdlibModule (modname, _)), None ->
    m
    |> lookup_topdefs ?prefix ~modname prg
    |> lookup_structs ?prefix ~modname prg
    |> lookup_enums ?prefix ~modname prg
    |> lookup_scopes ?prefix ~modname prg
  | (Module modname | StdlibModule (modname, _)), Some (PartialUident _) ->
    m
    |> lookup_structs ?prefix ~modname prg
    |> lookup_enums ?prefix ~modname prg
    |> lookup_scopes ?prefix ~modname prg
  | (Module modname | StdlibModule (modname, _)), Some (PartialLident _) ->
    m |> lookup_topdefs ?prefix ~modname prg
  | Enum enum_name, _ -> lookup_constructors ?prefix ~enum_name prg m
  | ScopeVar { ty = TStruct structname, _; _ }, _
  | ScopeVar { ty = TDefault (TStruct structname, _), _; _ }, _ ->
    lookup_struct_fields ?prefix ~structname prg m
  | Struct _, _
  | Constructor _, _
  | Scope _, _
  | Topdef _, _
  | ScopeVar _, _
  | StructField _, _
  | Keyword _, _ ->
    m

let lookup_keywords ~prefix (prg : typed Scopelang.Ast.program) m =
  let token_list =
    match prg.program_lang with
    | En -> Lexer_en.token_list
    | Fr -> Lexer_fr.token_list
    | Pl -> Lexer_pl.token_list
  in
  List.map (fun (lexeme, token) -> Kind.Keyword { lexeme; token }) token_list
  |> Kind.Set.of_list
  |> update_all ~prefix m

let tok_to_prefix full = function
  | Tokens.UIDENT s -> if full then FullUident s else PartialUident s
  | Tokens.LIDENT s -> if full then FullLident s else PartialLident s
  | _ -> assert false

let partial_prefix = tok_to_prefix false
let[@ocaml.warning "-32"] full_prefix = tok_to_prefix true

let uident_dot_prefix uident ?prefix_tok prg =
  let map = find_qualifiers prg uident in
  String.Map.fold
    (fun _qual s m ->
      let prefix = Option.map partial_prefix prefix_tok in
      Kind.Set.fold (fun k m -> follow_ups ?prefix prg k m) s m)
    map CompletionMap.empty

let single_uident prefix_tok prg =
  let prefix = tok_to_prefix false prefix_tok in
  CompletionMap.empty
  |> lookup_modules ~prefix prg
  |> lookup_structs ~prefix prg
  |> lookup_enums ~prefix prg
  |> lookup_constructors ~prefix prg
  |> lookup_scopes ~prefix prg

let single_lident ?(full = false) prefix_tok prg =
  let prefix = tok_to_prefix full prefix_tok in
  CompletionMap.empty
  |> lookup_topdefs ~prefix prg
  |> lookup_scopevars ~prefix prg
  |> lookup_keywords ~prefix prg
(* TODO: also inspect the AST *)

let lident_dot_prefix lident ?prefix_tok prg =
  let map = single_lident ~full:true (LIDENT lident) prg in
  let prefix = Option.map partial_prefix prefix_tok in
  String.Map.fold
    (fun _qual s m -> Kind.Set.fold (fun k m -> follow_ups ?prefix prg k m) s m)
    map CompletionMap.empty

let to_completions completion_map =
  CompletionMap.map
    (fun kinds ->
      (* Scope always have a associated struct => filter it out to prevent
         spurious completions *)
      if Kind.Set.exists (function Kind.Scope _ -> true | _ -> false) kinds
      then Kind.Set.filter (function Kind.Struct _ -> false | _ -> true) kinds
      else kinds)
    completion_map
  |> CompletionMap.bindings
  |> List.concat_map (fun (_, sk) ->
      Kind.Set.elements sk |> List.map Kind.to_completion)
  |> Utils.option_of_list

let lookup_completions (doc : document_state) ~doc_content pos =
  (* TODO: capture the 'scope' using the tokens to help completion, e.g., we
     know that we are in the scope S so on 'definition ' we know which variables
     are allowed. *)
  let open Surface in
  let*? rev_tokens, _next_tokens =
    retrieve_tokens doc ~content:doc_content pos
  in
  let on_token =
    match rev_tokens with
    | [] -> false
    | (_, _, (_, rp)) :: _ ->
      let rc = rp.Lexing.pos_cnum - rp.Lexing.pos_bol + 1 in
      pos.Position.character + 1 <= rc
  in
  let open Tokens in
  let r =
    match rev_tokens with
    | (_, ((UIDENT _ | LIDENT _) as tok), _) (* <= curr token *)
      :: (_, DOT, _)
      :: (_, UIDENT qualif, _)
      :: _ ->
      let*? last_valid_result = doc.last_valid_result in
      let prg = last_valid_result.prg in
      let completion_map = uident_dot_prefix qualif ~prefix_tok:tok prg in
      to_completions completion_map
    | (_, DOT, _) :: (_, UIDENT qualif, _) :: _ ->
      let*? last_valid_result = doc.last_valid_result in
      let prg = last_valid_result.prg in
      let completions_map = uident_dot_prefix qualif prg in
      to_completions completions_map
    | (_, DOT, _) :: (_, LIDENT qualif, _) :: _ ->
      let*? last_valid_result = doc.last_valid_result in
      let prg = last_valid_result.prg in
      lident_dot_prefix qualif prg |> to_completions
    | (_, (LIDENT _ as prefix_tok), _)
      :: (_, DOT, _)
      :: (_, LIDENT qualif, _)
      :: _ ->
      let*? last_valid_result = doc.last_valid_result in
      let prg = last_valid_result.prg in
      lident_dot_prefix ~prefix_tok qualif prg |> to_completions
    | (_, (UIDENT _s as tok), _) :: _ ->
      let*? last_valid_result = doc.last_valid_result in
      let prg = last_valid_result.prg in
      single_uident tok prg |> to_completions
    | (_, (LIDENT _s as tok), _) :: _ when on_token ->
      let*? last_valid_result = doc.last_valid_result in
      let prg = last_valid_result.prg in
      single_lident tok prg |> to_completions
    | _ -> None
  in
  (* Log.debug (fun m -> *)
  (*     m "@[<v 2>Full completion candidates:@ %a@]" *)
  (*       (pp_opt *)
  (*          Format.( *)
  (*            pp_print_list (fun fmt { CompletionItem.label; _ } -> *)
  (*                Format.pp_print_string fmt label))) *)
  (*       r); *)
  r

let lookup_completions
    (doc : document_state)
    ~doc_content
    pos
    (_diagnostics : diagnostic Range.Map.t Doc_id.Map.t) =
  (* FIXME: restore parsing "contextual" completions *)
  try lookup_completions doc ~doc_content pos
  with e ->
    Message.warning "Uncaught completion exception: %s" (Printexc.to_string e);
    None
