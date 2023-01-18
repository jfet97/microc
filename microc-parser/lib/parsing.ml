exception Syntax_error of Location.lexeme_pos * string

let parse _scanner _lexbuf =
  try Parser.program _scanner _lexbuf with
  | Scanner.Lexing_error _ as sle ->
      (* Printf.fprintf stderr "\n%s: Scanner.Lexing_error %s\n"
         (Location.show_lexeme_pos pos)
         msg; *)
      raise sle
  | Syntax_error _ as spe ->
      (* Printf.fprintf stderr "\n%s: Syntax_error (%s)\n"
         (Location.show_lexeme_pos pos)
         msg; *)
      raise spe
  | Parser.Error ->
      raise (Syntax_error (Location.to_lexeme_position _lexbuf, "Syntax error"))
