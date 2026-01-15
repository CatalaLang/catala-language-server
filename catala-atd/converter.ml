module R = Catala_types_j

let rec convert_to_json_input ({ value ; _ } : R.runtime_value) : Yojson.Safe.t =
  let open R in
  let convert_runtime_raw = function
    | Bool b -> `Bool b
    | Money i -> `String (string_of_float (float i /. 100.) )
    | Integer i -> `String (string_of_int i)
    | Decimal f -> `String (string_of_float f)
    | Date {year;month;day} -> `String (Format.sprintf "%04d-%02d-%02d" year month day)
    | Duration {years; months; days} ->
      `Assoc [ "years", `Int years ; "months", `Int months ; "days", `Int days ;   ]
    | Enum (_decl, (constr, None)) -> `String constr
    | Enum (_decl, (constr, Some v)) -> `Assoc [constr, convert_to_json_input v]
    | Struct (_decl, fl) ->
      `Assoc (List.map (fun (fname, v) -> fname, convert_to_json_input v) fl)
    | Array l -> `List (Array.to_list l |> List.map convert_to_json_input)
    | Unset -> failwith "convert_to_json_input: cannot convert 'unset' values"
    | Empty -> failwith "convert_to_json_input: cannot convert 'empty' values"
  in
  convert_runtime_raw value
