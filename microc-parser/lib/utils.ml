(* todo: sposta o cambia che è copiaincollato *)
exception Semantic_error of string

let raise_semantic_error code_position msg =
  let start_line_number = code_position.Location.start_line in
  let start_column_number = code_position.Location.start_column in 
  let end_line_number = code_position.Location.end_line in
  let end_column_number = code_position.Location.end_line in 
  let line = if start_line_number = end_line_number then 
               string_of_int start_line_number
             else
               Printf.sprintf "%d-%d" start_line_number end_line_number
  in 
  let column = if start_column_number = end_column_number then 
               string_of_int start_column_number
             else
               Printf.sprintf "%d-%d" start_column_number end_column_number
  in 
  let log = Printf.sprintf "%s:%s: %s" line column msg in 
  raise (Semantic_error log)