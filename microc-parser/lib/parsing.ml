open Lexing

exception Syntax_error of Location.lexeme_pos * string


let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse _scanner _lexbuf = 
  try
    Parser.program _scanner _lexbuf
  with
  | Scanner.Lexing_error (pos, msg) ->
    Printf.fprintf stderr "\n%s: %s\n" (Location.show_lexeme_pos pos) msg;
    exit(-1)
  | Syntax_error (pos, msg) ->
    Printf.fprintf stderr "\n%s: syntax error (%s)\n" (Location.show_lexeme_pos pos) msg;
    exit(-1)
  | Parser.Error ->
    Printf.fprintf stderr "\n%s: syntax error\n" (string_of_position _lexbuf);
    exit(-1)