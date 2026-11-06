let () =
  let lang_codes =
    Languages.languages ()
    |> List.map (fun (_, l) -> l.Language_t.code)
    |> List.map (fun c ->
        Printf.sprintf
          "    catala_source_%s.tmLanguage.json\n\
          \    catala_code_%s.tmLanguage.json"
          c c)
  in
  Printf.printf
    {dune|;; generated rule, do not modify directly
(rule
 (mode promote)
 (alias generate-syntaxes)
 (targets
%s
 )
 (deps catala_source.tmLanguage.template catala_code.tmLanguage.template)
 (action
  (run %%{exe:./gen_syntaxes.exe}))
)
|dune}
    (String.concat "\n" lang_codes)
