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
    | StdlibModule of { name : ModuleName.t; alias : string }
    | Struct of { name : StructName.t; fields : typ StructField.Map.t }
    | Enum of { name : EnumName.t; constrs : typ EnumConstructor.Map.t }
    | Constructor of { constr : EnumConstructor.t; enum_name : EnumName.t }
    | Scope of { name : ScopeName.t; decl : typed Scopelang.Ast.scope_decl }
    | Topdef of { name : TopdefName.t; ty : Shared_ast.typ }
    | ScopeVar of { name : ScopeVar.t; ty : Scopelang.Ast.scope_var_ty }
    | StructField of { name : StructField.t; ty : Shared_ast.typ }
    | Keyword of { lexeme : string; token : Tokens.token }

  let to_string = function
    | Module m -> ModuleName.to_string m
    | StdlibModule { name = _m; alias } -> alias
    | Struct { name; _ } -> StructName.base name
    | Enum { name; _ } -> EnumName.base name
    | Constructor { constr; _ } -> EnumConstructor.to_string constr
    | Scope { name; _ } -> ScopeName.base name
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
        ty.svar_out_ty
    | StructField { ty; _ } ->
      Format.fprintf ppf "StructField(%s, %a)" (to_string k)
        Shared_ast.Print.typ ty
    | Keyword { lexeme; _ } -> Format.fprintf ppf "Keyword(%s)" lexeme

  let make_doc locale k =
    let extract_doc get_info x =
      let*? doc =
        get_info x
        |> Mark.get
        |> Pos.attrs
        |> List.filter_map (function Doc (d, _) -> Some d | _ -> None)
        |> option_of_list
        |> Option.map (String.concat "\n")
      in
      Some doc
    in
    let open Format in
    let signature, doc =
      match k with
      | Module _ -> None, None (* TODO: print module content *)
      | StdlibModule _ -> None, None (* TODO: print module content *)
      | Struct { name; fields } ->
        ( Some (Type_printing.struct_code ~markdown:true locale (name, fields)),
          extract_doc StructName.get_info name )
      | Enum { name; constrs } ->
        ( Some (Type_printing.enum_code ~markdown:true locale (name, constrs)),
          extract_doc EnumName.get_info name )
      | Constructor { constr; enum_name } ->
        ( Some
            (asprintf "```catala_code_%s@\n%a.%s@\n```"
               (Type_printing.locale_s locale)
               EnumName.format_shortpath enum_name
               (EnumConstructor.to_string constr)),
          extract_doc EnumConstructor.get_info constr )
      | Scope { name; decl } ->
        ( Some (Type_printing.sig_type ~markdown:true locale name decl.scope_sig),
          extract_doc ScopeName.get_info name )
      | Topdef { name; ty } ->
        ( Some
            (asprintf "```catala_code_%s@\n%s : %a@\n```"
               (Type_printing.locale_s locale)
               (TopdefName.base name)
               (Type_printing.pp_typ locale)
               ty),
          extract_doc TopdefName.get_info name )
      | ScopeVar { name; ty } ->
        ( Some
            (asprintf "```catala_code_%s@\n%a@\n```"
               (Type_printing.locale_s locale)
               (Type_printing.pp_scope_var locale)
               (name, ty)),
          extract_doc ScopeVar.get_info name )
      | StructField { name; ty } ->
        ( Some
            (asprintf "```catala_code_%s@\n%a@\n```"
               (Type_printing.locale_s locale)
               (Type_printing.pp_struct_field locale)
               (name, ty)),
          extract_doc StructField.get_info name )
      | Keyword _ -> None, None
    in
    let*? value =
      match signature, doc with
      | None, None -> None
      | Some s, None | None, Some s -> Some s
      | Some sigz, Some doc -> Some Format.(asprintf "%s@\n%s" sigz doc)
    in
    Some
      (`MarkupContent (MarkupContent.create ~kind:MarkupKind.Markdown ~value))

  let itemkind locale : t -> completion =
   fun k ->
    let label, detail, kind =
      match k with
      | Module m -> ModuleName.to_string m, "Module", CompletionItemKind.Module
      | StdlibModule { alias; _ } -> alias, "Stdlib Module", Module
      | Struct { name; _ } -> StructName.base name, "Structure", Struct
      | Enum { name; _ } -> EnumName.base name, "Enumeration", Enum
      | Constructor { constr; _ } ->
        EnumConstructor.to_string constr, "Constructor", Constructor
      | Scope { name; _ } -> ScopeName.base name, "Scope", Function
      | Topdef { name; _ } -> TopdefName.base name, "Function", Function
      | ScopeVar { name; _ } -> ScopeVar.to_string name, "Variable", Variable
      | StructField { name; _ } -> StructField.to_string name, "Field", Field
      | Keyword { lexeme; _ } -> lexeme, "Keyword", Keyword
    in
    let documentation = make_doc locale k in
    { label; detail; kind; documentation }

  let to_completion locale : t -> Linol_lwt.CompletionItem.t =
   fun k ->
    let { label; detail; kind; documentation } = itemkind locale k in
    CompletionItem.create ~label ~detail ~kind ?documentation ()

  module Set = struct
    include Set.Make (struct
      type nonrec t = t

      let compare l r =
        match l, r with
        | Module l, Module r -> ModuleName.compare l r
        | StdlibModule { name = l; _ }, StdlibModule { name = r; _ } ->
          ModuleName.compare l r
        | Struct { name = l; _ }, Struct { name = r; _ } ->
          StructName.compare l r
        | Enum { name = l; _ }, Enum { name = r; _ } -> EnumName.compare l r
        | Constructor { constr = l; _ }, Constructor { constr = r; _ } ->
          EnumConstructor.compare l r
        | Scope { name = l; _ }, Scope { name = r; _ } -> ScopeName.compare l r
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
          Some (Kind.StdlibModule { name = m; alias }))
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
  |> Seq.map (fun (name, (decl, _)) ->
      let name =
        rename_alias_in_path prg.program_lang ScopeName.map_info name
      in
      Kind.Scope { name; decl })
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
  |> Seq.map (fun (name, fields) ->
      let name =
        rename_alias_in_path prg.program_lang StructName.map_info name
      in
      Kind.Struct { name; fields })
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
  |> Seq.map (fun (name, constrs) ->
      let name = rename_alias_in_path prg.program_lang EnumName.map_info name in
      Kind.Enum { name; constrs })
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
          |> Seq.map (fun (a, b) -> b, a)
          |> fun sq -> EnumConstructor.Map.add_seq sq acc)
        prg.program_ctx.ctx_enum_constrs EnumConstructor.Map.empty
    | Some e -> (
      let enum_opt = EnumName.Map.find_opt e prg.program_ctx.ctx_enums in
      match enum_opt with
      | None -> EnumConstructor.Map.empty
      | Some cs ->
        EnumConstructor.Map.to_seq cs
        |> Seq.map (fun (c, _) -> c, e)
        |> EnumConstructor.Map.of_seq)
  in
  EnumConstructor.Map.to_seq constructors
  |> Seq.map (fun (constr, enum_name) ->
      let enum_name =
        rename_alias_in_path prg.program_lang EnumName.map_info enum_name
      in
      Kind.Constructor { constr; enum_name })
  |> Kind.Set.of_seq
  |> update_all ?prefix m

let lookup_topdefs ?prefix ?modname (prg : typed Scopelang.Ast.program) m =
  let topdefs =
    match modname with
    | None ->
      (* Discard topdefs from other modules -- in the future, we would like to
         insert the required module *)
      TopdefName.Map.filter_map
        (fun td (typ, _vis) ->
          if TopdefName.path td = [] then Some typ else None)
        prg.program_ctx.ctx_topdefs
    | Some m ->
      TopdefName.Map.filter_map
        (fun td (typ, _vis) ->
          let*? m' =
            TopdefName.path td
            |> List.rev
            |> function [] -> None | h :: _ -> Some h
          in
          if ModuleName.equal m m' then Some typ else None)
        prg.program_ctx.ctx_topdefs
  in
  TopdefName.Map.to_seq topdefs
  |> Seq.map (fun (name, (ty : Shared_ast.typ)) ->
      let name =
        rename_alias_in_path prg.program_lang TopdefName.map_info name
      in
      Kind.Topdef { name; ty })
  |> Kind.Set.of_seq
  |> update_all ?prefix m

let lookup_scopevars ?prefix (prg : typed Scopelang.Ast.program) m =
  ScopeName.Map.to_seq prg.program_scopes
  |> Seq.map (fun (_, sdecl) ->
      ScopeVar.Map.to_seq (Mark.remove sdecl).Scopelang.Ast.scope_sig
      |> Seq.map (fun (name, (ty : Scopelang.Ast.scope_var_ty)) ->
          Kind.ScopeVar { name; ty }))
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
  | (Module modname | StdlibModule { name = modname; _ }), None ->
    m
    |> lookup_topdefs ?prefix ~modname prg
    |> lookup_structs ?prefix ~modname prg
    |> lookup_enums ?prefix ~modname prg
    |> lookup_scopes ?prefix ~modname prg
  | ( (Module modname | StdlibModule { name = modname; _ }),
      Some (PartialUident _) ) ->
    m
    |> lookup_structs ?prefix ~modname prg
    |> lookup_enums ?prefix ~modname prg
    |> lookup_scopes ?prefix ~modname prg
  | ( (Module modname | StdlibModule { name = modname; _ }),
      Some (PartialLident _) ) ->
    m |> lookup_topdefs ?prefix ~modname prg
  | Enum { name = enum_name; _ }, _ ->
    lookup_constructors ?prefix ~enum_name prg m
  | ScopeVar { ty = { svar_out_ty = TStruct structname, _; _ }; _ }, _
  | ScopeVar { ty = { svar_in_ty = TStruct structname, _; _ }; _ }, _
  | ( ScopeVar
        { ty = { svar_out_ty = TDefault (TStruct structname, _), _; _ }; _ },
      _ )
  | ( ScopeVar
        { ty = { svar_in_ty = TDefault (TStruct structname, _), _; _ }; _ },
      _ ) ->
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

let to_completions locale completion_map =
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
      Kind.Set.elements sk |> List.map (Kind.to_completion locale))
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
  let*? last_valid_result = doc.last_valid_result in
  let prg = last_valid_result.prg in
  let to_completions = to_completions prg.program_lang in
  let open Tokens in
  let r =
    match rev_tokens with
    | (_, ((UIDENT _ | LIDENT _) as tok), _) (* <= curr token *)
      :: (_, DOT, _)
      :: (_, UIDENT qualif, _)
      :: _ ->
      let completion_map = uident_dot_prefix qualif ~prefix_tok:tok prg in
      to_completions completion_map
    | (_, DOT, _) :: (_, UIDENT qualif, _) :: _ ->
      let completions_map = uident_dot_prefix qualif prg in
      to_completions completions_map
    | (_, DOT, _) :: (_, LIDENT qualif, _) :: _ ->
      lident_dot_prefix qualif prg |> to_completions
    | (_, (LIDENT _ as prefix_tok), _)
      :: (_, DOT, _)
      :: (_, LIDENT qualif, _)
      :: _ ->
      lident_dot_prefix ~prefix_tok qualif prg |> to_completions
    | (_, (UIDENT _s as tok), _) :: _ -> single_uident tok prg |> to_completions
    | (_, (LIDENT _s as tok), _) :: _ when on_token ->
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
