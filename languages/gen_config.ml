(* This file is part of the Catala compiler, a specification language
   for tax and social benefits computation rules. Copyright (C) 2026
   Inria, contributors: Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License. You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. *)

(* Rewrites the per-language keys of package.json; everything else is kept
   as is. The output follows JSON.stringify(_, null, 2), npm's layout, so npm
   and this generator never fight over the formatting. *)

let langs =
  List.map
    (fun (_, l) -> l.Locale.code, String.capitalize_ascii l.Locale.name)
    (Languages.languages ())
let str s = `String s
let id code = "catala_" ^ code

let languages =
  List.concat_map
    (fun (code, name) ->
      [ `Assoc
          [ "id", str (id code);
            "aliases", `List [ str (Printf.sprintf "Catala (%s)" name) ];
            "extensions",
            `List [ str (".catala_" ^ code); str (".catala_" ^ code ^ ".md") ];
            "configuration", str "./language-configuration.json" ];
        `Assoc
          [ "id", str ("catala_code_" ^ code);
            "configuration", str "./language-configuration.json" ] ])
    langs

let grammars =
  List.concat_map
    (fun (code, _) ->
      [ `Assoc
          [ "language", str (id code);
            "scopeName", str ("source.catala_" ^ code);
            "path",
            str (Printf.sprintf "./syntaxes/catala_source_%s.tmLanguage.json" code) ];
        `Assoc
          [ "language", str ("catala_code_" ^ code);
            "scopeName", str ("code.catala_" ^ code);
            "path",
            str (Printf.sprintf "./syntaxes/catala_code_%s.tmLanguage.json" code) ] ])
    langs

let breakpoints =
  List.map (fun (code, _) -> `Assoc [ "language", str (id code) ]) langs

let ids = List.map (fun (code, _) -> str (id code)) langs

(* Snippets are written by hand, per language: an entry for each file. *)
let snippets =
  List.filter_map
    (fun (code, _) ->
      let path = Printf.sprintf "snippets/catala_%s.json" code in
      if Sys.file_exists path then
        Some (`Assoc [ "language", str (id code); "path", str ("./" ^ path) ])
      else None)
    langs

let set k v l = List.map (fun (k', v') -> if k' = k then k, v else k', v') l

let update_debuggers = function
  | `List ds ->
    `List
      (List.map
         (function
           | `Assoc d when List.mem_assoc "languages" d ->
             `Assoc (set "languages" (`List ids) d)
           | d -> d)
         ds)
  | j -> j

(* Per-language entries ("[catala_xx]") are regenerated in place of the old
   ones; other defaults keep their position. *)
let update_defaults = function
  | `Assoc kvs ->
    let is_lang k = String.length k > 8 && String.sub k 0 8 = "[catala_" in
    let rec go = function
      | [] -> []
      | (k, v) :: rest when is_lang k ->
        let rest = List.filter (fun (k, _) -> not (is_lang k)) rest in
        List.map (fun (code, _) -> "[" ^ id code ^ "]", v) langs @ go rest
      | kv :: rest -> kv :: go rest
    in
    `Assoc (go kvs)
  | j -> j

let update_contributes = function
  | `Assoc c ->
    `Assoc
      (List.map
         (fun (k, v) ->
           match k with
           | "languages" -> k, `List languages
           | "grammars" -> k, `List grammars
           | "breakpoints" -> k, `List breakpoints
           | "snippets" -> k, `List snippets
           | "debuggers" -> k, update_debuggers v
           | "configurationDefaults" -> k, update_defaults v
           | _ -> k, v)
         c)
  | j -> j

let rec pp ind b (j : Yojson.Safe.t) =
  let nl i = Buffer.add_char b '\n'; Buffer.add_string b (String.make (2 * i) ' ') in
  let seq o c items f =
    if items = [] then (Buffer.add_char b o; Buffer.add_char b c)
    else begin
      Buffer.add_char b o;
      List.iteri (fun n x -> if n > 0 then Buffer.add_char b ','; nl (ind + 1); f x) items;
      nl ind; Buffer.add_char b c
    end
  in
  match j with
  | `Assoc kvs ->
    seq '{' '}' kvs (fun (k, v) ->
        Buffer.add_string b (Yojson.Safe.to_string (`String k));
        Buffer.add_string b ": "; pp (ind + 1) b v)
  | `List l -> seq '[' ']' l (pp (ind + 1) b)
  | j -> Buffer.add_string b (Yojson.Safe.to_string j)

let () =
  let j =
    match Yojson.Safe.from_file Sys.argv.(1) with
    | `Assoc top -> `Assoc (List.map (fun (k, v) -> if k = "contributes" then k, update_contributes v else k, v) top)
    | j -> j
  in
  let b = Buffer.create 65536 in
  pp 0 b j;
  Buffer.add_char b '\n';
  print_string (Buffer.contents b)
