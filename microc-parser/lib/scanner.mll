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
      (* ("if", IF);
      ("else", ELSE);
      ("function", FUNCTION);
      ("return", RETURN);
      ("for", FOR);
      ("while", WHILE); *)
      ("int", INT_T);
      ("bool", BOOL_T);
      ("char", CHAR_T);
      ("void", VOID_T);
      (* ("null", NULL); *)
    ] in create_hashtable (List.length table) table 
}

let digit_base10 = ['0'-'9']
let digit_base16 = ['0'-'9' 'A'-'F']
let one_to_nine = ['1'-'9']
let one_to_f = ['1'-'9' 'A'-'F']

let num_base10 = ('-'? one_to_nine digit_base10*) | '0'
let num_base16 = ('-'? "0x" one_to_f digit_base16) | "0x0"
let id = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let boolean = "true" | "false"

(* Scanner specification *)

rule next_token = parse
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
| boolean as sbool
  {
    let vbool =
      match sbool with
      | "true" -> true
      | "false" -> false
      | _ -> failwith "the impossible happened"
    in BOOL vbool
  }
| ';'
  { SEMICOLON }
| '*'
  { STAR }
| '('
  { LEFT_PAREN }
| ')'
  { RIGHT_PAREN }
| '['
  { LEFT_BRACKET }
| ']'
  { RIGHT_BRACKET }
| "//" [^ '\n']*  (* eat up one-line comments *)
| [' ' '\t']  (* eat up whitespaces *)
  {
    next_token lexbuf
  }
| ['\r' '\n'] | "\r\n"
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