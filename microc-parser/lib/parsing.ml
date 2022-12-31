open Lexing

exception Syntax_error of Location.lexeme_pos * string

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s: line %d: column %d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse _scanner _lexbuf =
  try Parser.program _scanner _lexbuf with
  | Scanner.Lexing_error (pos, msg) as sle ->
      (* Printf.fprintf stderr "\n%s: Scanner.Lexing_error %s\n"
         (Location.show_lexeme_pos pos)
         msg; *)
      raise sle
  | Syntax_error (pos, msg) as spe ->
      (* Printf.fprintf stderr "\n%s: Syntax_error (%s)\n"
         (Location.show_lexeme_pos pos)
         msg; *)
      raise spe
