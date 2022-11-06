/*
* MicroC Parser specification
*/

%{
     (* Auxiliary definitions *)
     type vardesc_desc = VarDescStar | VarDescParens | VarDescArr of int option;;

    let (|@|) node loc = { Ast.node = node; Ast.loc = loc }
%}

/* Tokens declarations */
%token <int> INT
%token <bool> BOOL
%token <string> ID

%token SEMICOLON
%token INT_T CHAR_T BOOL_T VOID_T

%token STAR
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_CURLY RIGHT_CURLY
%token COMMA


%token EOF
/* Precedence and associativity specification */

%left STAR
%nonassoc LEFT_BRACKET


/* Starting symbol */

%start program

%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */

program:
  | list(topdecl) EOF 
    { Ast.Prog($1) }
  ;

topdecl:
  | vardecl SEMICOLON
    { Ast.Vardec(fst $1, snd $1) |@| (Location.to_code_position $loc) }
  | fundecl
    { Ast.Fundecl($1) |@| (Location.to_code_position $loc) }
  ;

(* TODO: solo un void o un array di void non è okay, un array di puntatori a void si, un puntatore ad un array di void no, un puntatore a void si, un array di puntatori a void si, quando ok usare [] *)
vardecl:
  | typ vardesc
    { 
      let ftvd t vd = 
        match vd with
          | VarDescStar -> Ast.TypP t
          | VarDescParens -> t
          | VarDescArr oi -> Ast.TypA (t, oi)
      in
      let tt = List.fold_left ftvd $1 (snd $2)
      in (tt,  fst $2)
    }
  ;

typ:
  | INT_T
    { Ast.TypI }
  | CHAR_T
    { Ast.TypC }
  | BOOL_T
    { Ast.TypB }
  | VOID_T
    { Ast.TypV }
  ;

vardesc:
  | ID
    { ($1, []) }
  | STAR vardesc
    { (fst $2, VarDescStar :: snd $2) }
  | LEFT_PAREN vardesc RIGHT_PAREN
    { (fst $2, VarDescParens :: snd $2) }
  | vardesc LEFT_BRACKET RIGHT_BRACKET
    { (fst $1, VarDescArr None :: snd $1) }
  | vardesc LEFT_BRACKET INT RIGHT_BRACKET
    { (fst $1, VarDescArr (Some $3) :: snd $1) }
  ;

fundecl:
  | typ ID LEFT_PAREN separated_list(COMMA, vardecl) RIGHT_PAREN block                      
    {
      {
        Ast.typ = $1;
        Ast.fname = $2;
        Ast.formals = $4;
        Ast.body = $6;
      } 
    }
  ;

(* TODO *)
block:
  | LEFT_CURLY RIGHT_CURLY
    { Ast.Block [] |@| (Location.to_code_position $loc) }
  ;