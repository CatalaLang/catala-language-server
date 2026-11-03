open Catala_utils
open Format
open Language_t

let langs = Languages.languages () |> List.map snd
let pp_comma fmt () = fprintf fmt ","
let pp_comma_spaced fmt () = fprintf fmt ",@ "
let pp_pipe fmt () = fprintf fmt "|"

let or_patt_regexp =
  asprintf "@[<h>%a@]"
    (pp_print_list ~pp_sep:pp_pipe pp_print_string)
    (List.map (fun l -> l.code) langs)

let or_patt_glob =
  asprintf "@[<h>%a@]"
    (pp_print_list ~pp_sep:pp_comma pp_print_string)
    (List.map (fun l -> l.code) langs)

let pp_string_id fmt lang = fprintf fmt {|"catala_%s"|} lang.code

let id_list =
  asprintf "@[<h>[ %a ]@]"
    (pp_print_list ~pp_sep:pp_comma_spaced pp_string_id)
    langs

let breakpoints =
  let pp_language fmt lang =
    fprintf fmt {|{ "language": %a }|} pp_string_id lang
  in
  asprintf "@[<h>[ %a ]@]"
    (pp_print_list ~pp_sep:pp_comma_spaced pp_language)
    langs

let languages =
  let pp_language fmt lang =
    fprintf fmt
      "@[<v 2>{ \"id\": \"catala_%s\",@\n\
       \"aliases\": [ \"Catala (%s)\" ],@\n\
       @[<h>\"extensions\": [ \".catala_%s\", \".catala_%s.md\" ],@]@\n\
       \"configuration\": \"./language-configuration.json\" },@]@\n\
       @[<v 2>{ \"id\": \"catala_code_%s\",@\n\
       \"configuration\": \"./language-configuration.json\" }@],@\n\
       @[<v 2>{ \"id\": \"catala_type_%s\",@\n\
       \"configuration\": \"./language-configuration.json\" }@]"
      lang.code
      (String.capitalize_ascii lang.name)
      lang.code lang.code lang.code lang.code
  in
  let pp_grammar fmt lang =
    fprintf fmt
      "@[<v 2>{ \"language\": \"catala_%s\",@\n\
       \"scopeName\": \"source.catala_%s\",@\n\
       \"path\": \"./syntaxes/%s.xml\" },@]@\n\
       @[<v 2>{ \"language\": \"catala_code_%s\",@\n\
       \"scopeName\": \"source.catala_code_%s\",@\n\
       \"path\": \"./syntaxes/code_%s.xml\" },@]@\n\
       @[<v 2>{ \"language\": \"catala_type_%s\",@\n\
       \"scopeName\": \"source.catala_type_%s\",@\n\
       \"path\": \"./syntaxes/types_%s.xml\" }@]"
      lang.code lang.code lang.code lang.code lang.code lang.code lang.code
      lang.code lang.code
  in
  let pp_configuration_defaults fmt lang =
    fprintf fmt
      "@[<v 2>\"[catala_%s]\": {@\n\
       \"editor.tabSize\": 2,@ \"editor.insertSpaces\": true }@]"
      lang.code
  in
  let indent = "    " in
  asprintf
    "@[<v 6>\"languages\": [@ %a@]@\n\
     %s],@\n\
     @[<v 6>%s\"grammars\": [@ %a@]@\n\
     %s],@\n\
     @[<v 6>%s\"configurationDefaults\": {@ %a@]@\n\
     %s}"
    (pp_print_list ~pp_sep:pp_comma_spaced pp_language)
    langs indent indent
    (pp_print_list ~pp_sep:pp_comma_spaced pp_grammar)
    langs indent indent
    (pp_print_list ~pp_sep:pp_comma_spaced pp_configuration_defaults)
    langs indent

let to_replace =
  [
    {|%%CATALA_LANGS_OR_PATT_REG%%|}, or_patt_regexp;
    {|%%CATALA_LANGS_OR_PATT_GLOB%%|}, or_patt_glob;
    {|%%CATALA_LANGS_ID_LIST%%|}, id_list;
    {|%%CATALA_LANGS_BREAKPOINTS%%|}, breakpoints;
    {|%%CATALA_LANGS_DEF_AND_GRAMMAR%%|}, languages;
  ]

let () =
  let in_file = File.contents Sys.argv.(1) in
  List.fold_left
    (fun file (re, content) -> Str.global_replace (Str.regexp re) content file)
    in_file to_replace
  |> print_string
