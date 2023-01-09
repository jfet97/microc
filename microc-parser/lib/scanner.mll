{
  open Parser
  open Printf

  exception Lexing_error of Location.lexeme_pos * string

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  (* map keyword lexemes to corresponding Parser tokens*)
  let keyword_table =
    let table = [
      ("if", IF);
      ("else", ELSE);
      ("for", FOR);
      ("while", WHILE);
      ("do", DO);
      ("return", RETURN);
      ("int", INT_T);
      ("bool", BOOL_T);
      ("char", CHAR_T);
      ("void", VOID_T);
      ("NULL", NULL);
    ] in create_hashtable (List.length table) table

  let char_for_backslash = function
  | 'n' -> '\010'
  | 't' -> '\009'
  | 'b' -> '\008'
  | 'r' -> '\013'
  | 'f' -> '\012'
  | c   -> c
}

let digit_base10 = ['0'-'9']
let digit_base16 = ['0'-'9' 'A'-'F']
let one_to_nine = ['1'-'9']
let one_to_f = ['1'-'9' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z']

let num_base10 = (one_to_nine digit_base10*) | '0'
let num_base16 = ("0x" one_to_f digit_base16) | "0x0"
let id = ('_' | alpha)('_' | alpha | digit_base10)*
let boolean = "true" | "false"

let newline = ['\r' '\n'] | "\r\n"
let whitespace = [' ' '\t']

let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' 'f']

(* Scanner specification *)

rule next_token = parse
| boolean as sbool
  {
    let vbool =
      match sbool with
      | "true" -> true
      | "false" -> false
      | _ -> failwith "the impossible happened"
    in BOOL vbool
  }
| id as word
  {
    try
      let token = Hashtbl.find keyword_table word in
      (* printf "keyword: %s  --- " word;
      printf "position: %s\n" (Location.show_lexeme_pos (Location.to_lexeme_position lexbuf)); *)
      token
    with Not_found ->
      ID word
  }
| num_base10
| num_base16 as snum
  {
    let vnum = int_of_string snum in
    INT vnum
  }

| "'" [^ '\\'] "'"
    { CHAR (Lexing.lexeme_char lexbuf 1) }
| "'" '\\' backslash_escapes "'"
    { CHAR (char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
| "+="
  { ADD_ASSIGN}
| "-="
  { SUB_ASSIGN }
| "*="
  { MUL_ASSIGN }
| "/="
  { DIV_ASSIGN }
| "%="
  { MOD_ASSIGN }
| '='
  { ASSIGN }
| "++"
  { PLUSPLUS }
| "--"
  { MINUSMINUS }
| "&&"
  { AND }
| "||"
  { OR }
| "=="
  { EQUAL }
| "!="
  { NEQ }
| '<'
  { LESS}
| '>'
  { GREATER }
| "<="
  { LEQ }
| ">="
  { GEQ }
| '+'
  { ADD }
| '-'
  { SUB }
| '%'
  { MOD }
| '/'
  { DIV }
| '!'
  { NOT }
| ','
  { COMMA }
| ';'
  { SEMICOLON }
| '*'
  { STAR }
| '&'
  { AMPERSAND }
| '('
  { LEFT_PAREN }
| ')'
  { RIGHT_PAREN }
| '['
  { LEFT_BRACKET }
| ']'
  { RIGHT_BRACKET }
| '{'
  { LEFT_CURLY }
| '}'
  { RIGHT_CURLY }
| "//" 
  {
    (* eat up one-line comments *)
    single_line_comment lexbuf
  } 
| "/*" 
  {
    (* eat up multiline comments *)
    multi_line_comment lexbuf
  } 
| whitespace
  {
    (* eat up whitespaces *)
    next_token lexbuf
  }
| newline
  {
    (* eat up newlines, increase line number *)
    Lexing.new_line lexbuf;
    next_token lexbuf
  }
| _ as c
  {
    let err_msg = sprintf "Unrecognized character: %c --- " c in
    let pos = Location.to_lexeme_position lexbuf in
    raise (Lexing_error (pos, err_msg))
  }
| eof
  { EOF }

and single_line_comment = parse
  | newline 
    { 
      Lexing.new_line lexbuf;
      next_token lexbuf 
    }
  | eof { EOF }
  | _ 
    { 
      single_line_comment lexbuf 
    }

and multi_line_comment = parse
  | "*/" 
    { 
      next_token lexbuf 
    }
  | newline 
    { 
      Lexing.new_line lexbuf;
      multi_line_comment lexbuf 
    }
  | eof 
    { 
      let err_msg = "Lexer: Unexpected EOF in a multiline comment. Please terminate it" in
      let pos = Location.to_lexeme_position lexbuf in
      raise (Lexing_error(pos, err_msg)) 
    }
  | _ 
    { 
      multi_line_comment lexbuf 
    }