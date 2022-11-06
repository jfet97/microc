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
  | list(topdecl) EOF                           { Ast.Prog($1) }
  ;

(*TODO: non usare i node diretti ma la versione annotata con la posizione, capire come usare le posizioni dalle slide *)
topdecl:
  | vardecl SEMICOLON                           { $1 |@| (Location.to_code_position $loc) }
  (* TODO: Fundecl *)
  ;

(*TODO: non usare i node diretti ma la versione annotata con la posizione, capire come usare le posizioni dalle slide *)
(* TODO: solo un void o un array di void non è okay, un array di puntatori a void si, un puntatore ad un array di void no, un puntatore a void si, un array di puntatori a void si, quando ok usare [] *)
vardecl:
  | typ vardesc                                 { 
                                                  let ftvd t vd = match vd with
                                                                    | VarDescStar -> Ast.TypP t
                                                                    | VarDescParens -> t
                                                                    | VarDescArr oi -> Ast.TypA (t, oi)
                                                  in
                                                  let tt = List.fold_left ftvd $1 (snd $2)
                                                  in Ast.Vardec(tt,  fst $2)
                                                }
  ;

typ:
  | INT_T                                         { Ast.TypI }
  | CHAR_T                                        { Ast.TypC }
  | BOOL_T                                        { Ast.TypB }
  | VOID_T                                        { Ast.TypV }
  ;

vardesc:
  | ID                                          { ($1, []) }
  | STAR vardesc                                { (fst $2, VarDescStar :: snd $2) }
  | LEFT_PAREN vardesc RIGHT_PAREN              { (fst $2, VarDescParens :: snd $2) }
  | vardesc LEFT_BRACKET RIGHT_BRACKET          { (fst $1, VarDescArr None :: snd $1) }
  | vardesc LEFT_BRACKET INT RIGHT_BRACKET      { (fst $1, VarDescArr (Some $3) :: snd $1) }
  ;

(* INT_T STAR LEFT_PAREN STAR ID RIGHT_PAREN LEFT_BRACKET INT RIGHT_BRACKET SEMICOLON *)
(* INT_T STAR STAR ID LEFT_BRACKET INT RIGHT_BRACKET SEMICOLON *)