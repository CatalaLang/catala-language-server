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

open Catala_utils
open Locale

let expand_all_tokens lang =
  (* Expanding all records for exhaustivity check *)
  let { name = _; code; keywords; specific_delimiters; builtins; directives } =
    lang
  in
  (* keywords *)
  let {
    all;
    among;
    and_;
    and_then;
    assertion;
    but_replace;
    combine;
    condition;
    consequence;
    contains;
    content;
    context;
    data;
    day;
    declaration;
    decreasing;
    defined_as;
    definition;
    depends;
    else_;
    enum;
    exception_;
    exists;
    false_;
    filled;
    for_;
    if_;
    in_;
    increasing;
    initially;
    input;
    internal;
    is;
    label;
    let_;
    list;
    map_each;
    match_;
    maximum;
    minimum;
    month;
    not_;
    of_;
    option;
    or_;
    or_if_list_empty;
    order_increasing;
    order_decreasing;
    output;
    rule;
    scope;
    sort;
    state;
    struct_;
    such;
    sum;
    that;
    then_;
    to_;
    true_;
    type_;
    under_condition;
    we_have;
    wildcard;
    with_;
    with_v;
    xor;
    year;
  } =
    keywords
  in
  (* builtins *)
  let { types; optionals; functions } = builtins in
  let { boolean; date; decimal; duration; integer; money; position } = types in
  let { present; absent } = optionals in
  let { round; impossible; cardinal } = functions in
  (* specific_delimiters *)
  let { decimal_separator; money_unit; money_unit_position = _; money_delim } =
    specific_delimiters
  in
  (* directives *)
  let { external_; law_include; module_alias; module_def; module_use } =
    directives
  in
  [
    (* We also add the code *)
    "code", code;
    "absent", absent;
    "all", all;
    "among", among;
    "and", and_;
    "and_then", and_then;
    "assertion", assertion;
    "boolean", boolean;
    "but_replace", but_replace;
    "cardinal", cardinal;
    "combine", combine;
    "condition", condition;
    "consequence", consequence;
    "contains", contains;
    "content", content;
    "context", context;
    "data", data;
    "date", date;
    "day", day;
    "decimal", decimal;
    "decimal_separator", decimal_separator;
    "declaration", declaration;
    "decreasing", decreasing;
    "defined_as", defined_as;
    "definition", definition;
    "depends", depends;
    "duration", duration;
    "else", else_;
    "enum", enum;
    "exception", exception_;
    "exists", exists;
    "external", external_;
    "false", false_;
    "filled", filled;
    "for", for_;
    "if", if_;
    "impossible", impossible;
    "in", in_;
    "increasing", increasing;
    "initially", initially;
    "input", input;
    "integer", integer;
    "internal", internal;
    "is", is;
    "label", label;
    "law_include", law_include;
    "let", let_;
    "list", list;
    "map_each", map_each;
    "match", match_;
    "maximum", maximum;
    "minimum", minimum;
    "module_alias", module_alias;
    "module_def", module_def;
    "module_use", module_use;
    "money", money;
    "money_delim", money_delim;
    "money_unit", money_unit;
    "year", year;
    "month", month;
    "not", not_;
    "of", of_;
    "option", option;
    "or", or_;
    "or_if_list_empty", or_if_list_empty;
    "order_decreasing", order_decreasing;
    "order_increasing", order_increasing;
    "output", output;
    "position", position;
    "present", present;
    "round", round;
    "rule", rule;
    "scope", scope;
    "sort", sort;
    "state", state;
    "struct", struct_;
    "such", such;
    "sum", sum;
    "that", that;
    "then", then_;
    "to", to_;
    "true", true_;
    "type", type_;
    "under_condition", under_condition;
    "we_have", we_have;
    "wildcard", wildcard;
    "with", with_;
    "with_v", with_v;
    "xor", xor;
  ]
  |> String.Map.of_list

let replace_all_tokens tok_map contents =
  let open Re in
  let re =
    compile
      (seq
         [
           str "%%";
           group ~name:"tok_name" (rep1 (alt [alpha; char '_']));
           str "%%";
         ])
  in
  let to_valid_regexp s =
    Re.replace_string ~all:true (compile space) ~by:"\\s+" s
  in
  Re.replace ~all:true re
    ~f:(fun g ->
      let tok_name = Group.get g 1 in
      String.Map.find tok_name tok_map |> to_valid_regexp)
    contents

let () =
  let source_template_file = "catala_source.tmLanguage.template" in
  let code_template_file = "catala_code.tmLanguage.template" in
  List.iter
    (fun (_, language) ->
      let code = language.code in
      let output_source_file =
        Format.sprintf "catala_source_%s.tmLanguage.yaml" code
      in
      let output_code_file =
        Format.sprintf "catala_code_%s.tmLanguage.yaml" code
      in
      let source_contents = File.contents source_template_file in
      let code_contents = File.contents code_template_file in
      let toks = expand_all_tokens language in
      let generate_file contents out_f =
        File.with_out_channel out_f (fun oc ->
            let new_contents = replace_all_tokens toks contents in
            output_string oc new_contents);
        File.with_out_channel
          File.(out_f -.- "json")
          (fun oc ->
            output_string oc (File.process_out "npx" ["js-yaml"; out_f]))
      in
      generate_file source_contents output_source_file;
      generate_file code_contents output_code_file)
    (Languages.languages ())
