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
%token <char> CHAR
%token <bool> BOOL
%token NULL
%token <string> ID

%token COMMA SEMICOLON
%token INT_T CHAR_T BOOL_T VOID_T

%token ASSIGN
%token ADD SUB DIV MOD
%token STAR AMPERSAND
%token AND OR NOT
%token EQUAL NEQ LESS GREATER LEQ GEQ
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_CURLY RIGHT_CURLY
%token RETURN
%token WHILE

%token EOF
/* Precedence and associativity specification */

%right ASSIGN
%left OR
%left AND
%left EQUAL NEQ
%nonassoc LESS GREATER LEQ GEQ
%left ADD SUB
%left STAR DIV MOD 
%nonassoc NOT AMPERSAND
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
  | vardecl_sem
    { 
      let (t, id, loc) = $1 in
      Ast.Vardec(t, id) |@| loc
    }
  | fundecl
    { Ast.Fundecl($1) |@| (Location.to_code_position $loc) }
  ;

vardecl_sem:
  | vardecl SEMICOLON
    { (fst $1, snd $1, Location.to_code_position $loc) }
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
      in (tt, fst $2)
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
  | LEFT_CURLY list(block_entry) RIGHT_CURLY
    { 
      let stmtordec_list = List.fold_right (fun o a -> match o with
                                                      | None -> a
                                                      | Some(sod) -> sod :: a) $2 [] in
      Ast.Block(stmtordec_list) |@| (Location.to_code_position $loc)
    }
  ;

block_entry:
  | vardecl_sem
    { 
      let (t, id, loc) = $1 in
      Some(Ast.Dec(t, id) |@| loc)
    }
  | stmt
    {
      Option.map (fun sm -> (Ast.Stmt(sm) |@| sm.Ast.loc)) $1
    }

(* TODO: finire *)
stmt:
  | RETURN option(expr) SEMICOLON
    { Some (Ast.Return($2) |@| Location.to_code_position $loc) }
  | option(expr) SEMICOLON 
    { 
      let loc = Location.to_code_position $loc in
      Option.map (fun ex -> (Ast.Expr(ex) |@| loc)) $1
    }
  | block
    { Some $1 }
  (*| WHILE LEFT_PAREN (* optional(Expr) *) RIGHT_PAREN stmt
    { SOME (AST.While($3, $5) )} *)
  ;

expr:
  | lexpr_access
    { $1 }
  | rexpr
    { $1 }

(* otherwise it's unclear when take the reduction lexpr_access -> lexpr *)
(* e.g. `STAR lexpr` becomes `STAR lexpr_access` vs wait for a possible LEFT_BRACKET, so that the reduction is post-poned *)
(* the precedence of LEFT_BRACKET over STAR does not apply here because the intention is not to reduce *)
(* `STAR lexpr` as a whole, but only the `lexpr` inside it *)
%inline lexpr_access:
  | lexpr
    { 
      let loc =  Location.to_code_position $loc in
      Ast.Access($1) |@| loc
    }
  ;

lexpr:
  | ID
    { 
      let loc =  Location.to_code_position $loc in
      Ast.AccVar($1) |@| loc
    }
  | LEFT_PAREN lexpr RIGHT_PAREN
    { $2 }
  | STAR lexpr_access
    { 
      let loc = Location.to_code_position $loc in
      Ast.AccDeref($2) |@| loc
    }
  | STAR aexpr
    {
      let loc = Location.to_code_position $loc in
      Ast.AccDeref($2) |@| loc   
    }
  | lexpr LEFT_BRACKET expr RIGHT_BRACKET
    {
      let loc = Location.to_code_position $loc in
      Ast.AccIndex($1, $3) |@| loc
    }
  ;

aexpr: 
  | INT
    {
      let loc = Location.to_code_position $loc in 
      Ast.ILiteral($1) |@| loc
    }
  | CHAR
    { 
      let loc = Location.to_code_position $loc in
      Ast.CLiteral($1) |@| loc 
    }
  | BOOL
    { 
      let loc = Location.to_code_position $loc in
      Ast.BLiteral($1) |@| loc 
    }
  | NULL
    { 
      let loc = Location.to_code_position $loc in
      Ast.ILiteral(0) |@| loc 
    }
  | AMPERSAND lexpr
    { 
      let loc = Location.to_code_position $loc in
      Ast.Addr($2) |@| loc
    }
  | LEFT_PAREN rexpr RIGHT_PAREN
    {
      let loc = Location.to_code_position $loc in
      $2.Ast.node |@| loc
    }
  ;

rexpr:
  | aexpr
    { $1 }
  | lexpr ASSIGN expr
    { 
      let loc = Location.to_code_position $loc in
      Ast.Assign($1, $3) |@| loc
    }
  | expr binop expr
    { 
      let loc = Location.to_code_position $loc in
      Ast.BinaryOp($2, $1, $3) |@| loc
    }
  | uop expr
    { 
      let loc = Location.to_code_position $loc in
      Ast.UnaryOp($1, $2) |@| loc
    }
  | ID LEFT_PAREN separated_list(COMMA, expr) RIGHT_PAREN
    {
      let loc = Location.to_code_position $loc in
      Ast.Call($1, $3) |@| loc
    }
  ;

%inline binop:
  | ADD
    { Ast.Add }
  | SUB
    { Ast.Sub }
  | STAR
    { Ast.Mult }
  | MOD
    { Ast.Mod }
  | DIV
    { Ast.Div }
  | AND
    { Ast.And }
  | OR
    { Ast.Or }
  | LESS
    { Ast.Less }
  | GREATER
    { Ast.Greater }
  | LEQ
    { Ast.Leq }
  | GEQ
    { Ast.Geq }
  | EQUAL
    { Ast.Equal }
  | NEQ
    { Ast.Neq }
  ;

%inline uop:
  | NOT
    { Ast.Not }
  | SUB
    { Ast.Neg }