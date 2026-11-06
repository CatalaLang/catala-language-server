;; generated rule, do not modify directly
(rule
 (mode promote)
 (alias generate-syntaxes)
 (targets
    catala_source_en.tmLanguage.json
    catala_code_en.tmLanguage.json
    catala_source_fr.tmLanguage.json
    catala_code_fr.tmLanguage.json
    catala_source_pl.tmLanguage.json
    catala_code_pl.tmLanguage.json
 )
 (deps catala_source.tmLanguage.template catala_code.tmLanguage.template)
 (action
  (run %{exe:./gen_syntaxes.exe}))
)
