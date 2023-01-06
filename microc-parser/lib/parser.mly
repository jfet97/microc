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

%token COMMA
%token SEMICOLON
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
%token IF ELSE
%token WHILE FOR DO

%token EOF
/* Precedence and associativity specification */

%nonassoc THEN
%nonassoc ELSE

%nonassoc LESS_THAN_COMMA
%left COMMA
%nonassoc MORE_THAN_COMMA
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
  | vardecl_init_list
    { 
      Ast.Vardec($1) |@| Location.to_code_position $loc
    }
  | fundecl
    { Ast.Fundecl(fst $1) |@| (snd $1) }
  ;

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
      (* type, id *)
      in (tt, fst $2)
    }
  ;

assign_expr:
  | ASSIGN expr_comma
    { [$2] }
  | ASSIGN LEFT_CURLY separated_list(COMMA, expr) RIGHT_CURLY
    { $3 }
  ;

vardecl_init_list:
  | typ separated_nonempty_list(COMMA, pair(vardesc, option(assign_expr))) SEMICOLON
    { 
      let ftvd t vd = 
        match vd with
          | VarDescStar -> Ast.TypP t
          | VarDescParens -> t
          | VarDescArr oi -> Ast.TypA (t, oi)
      in
      List.map (fun vinit -> 
        let vdesc = fst vinit in
        let init_exprs = match snd vinit with | None -> [] | Some ae -> ae in
        let tt_temp = List.fold_left ftvd $1 (snd vdesc) in
        (* set array length if there is a non-empty initializer list and the length were not already set *)
        let tt = (
          match tt_temp with 
          | Ast.TypA(t, oi) -> (
            match oi with
              | Some _ -> tt_temp
              | None -> 
                let init_exprs_len = List.length init_exprs in
                if init_exprs_len == 0 then tt_temp
                else Ast.TypA(t, Some(init_exprs_len))
          )
          | _ -> tt_temp
        )
        in (tt, fst vdesc, init_exprs)
      ) $2
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
      (
        {
          Ast.typ = $1;
          Ast.fname = $2;
          Ast.formals = $4;
          Ast.body = $6;
        },
        (Location.to_code_position $loc)
      )
    }
  ;

block:
  | LEFT_CURLY list(block_entry) RIGHT_CURLY
    {
      Ast.Block($2) |@| (Location.to_code_position $loc)
    }
  ;

block_entry:
  | vardecl_init_list
    { 
       Ast.Dec($1) |@| Location.to_code_position $loc
    }
  | stmt
    {
      let sm = $1 in
      Ast.Stmt(sm) |@| sm.Ast.loc
    }

stmt:
  | RETURN option(expr_comma) SEMICOLON
    { 
      let loc = Location.to_code_position $loc in
      Ast.Return($2) |@| loc
    }
  | option(expr_comma) SEMICOLON 
    { 
      let loc = Location.to_code_position $loc in
      match $1 with
        | None -> Ast.Block([]) |@| loc
        | Some(ex) -> Ast.Expr(ex) |@| loc
    }
  | block
    { $1 }
  | WHILE LEFT_PAREN expr_comma RIGHT_PAREN stmt
    { 
      let loc = Location.to_code_position $loc in
      Ast.While($3, $5) |@| loc
    }
  | DO stmt WHILE LEFT_PAREN expr_comma RIGHT_PAREN SEMICOLON
    { 
      (* do while -> while rewriting *)
      let loc = Location.to_code_position $loc in
      let stmt_loc = Location.to_code_position($startpos($2), $endpos($2)) in
      (* let expr_loc = Location.to_code_position($startpos($5), $endpos($5)) in *)
      Ast.Block([Ast.Stmt($2) |@| stmt_loc ; Ast.Stmt(Ast.While($5, $2) |@| loc) |@| loc]) |@| loc
    }
  | FOR LEFT_PAREN option(expr_comma) SEMICOLON option(expr_comma) SEMICOLON option(expr_comma) RIGHT_PAREN stmt
    {
      (* for -> while rewriting *)
      let loc = Location.to_code_position $loc in
      let init_expr_stm_o =
        let loc_e = Location.to_code_position($startpos($3), $endpos($4)) in
        Option.map (fun ex ->  Ast.Stmt(Ast.Expr(ex) |@| loc_e) |@| loc_e) $3
      in
      let update_expr_stm_o =
        let loc_e = Location.to_code_position($startpos($7), $endpos($7)) in
        Option.map (fun ex ->  Ast.Stmt(Ast.Expr(ex) |@| loc_e) |@| loc_e) $7
      in
      let while_stm =
        match update_expr_stm_o with
          | None -> $9
          | Some(uestm) -> Ast.Block([
              Ast.Stmt($9) |@| Location.to_code_position($startpos($9), $endpos($9));
              uestm
            ]) |@| Location.to_code_position($startpos($7), $endpos($9))
      in
      let while_expr = match $5 with
          | None -> Ast.BLiteral(true) |@| Location.to_code_position($startpos($5), $endpos($5))
          | Some(ex) -> ex
      in
      let while_partial = Ast.While(while_expr, while_stm)
      in
      let while_partial_loc = Location.to_code_position($startpos($5), $endpos($9))
      in 
      let for_as_while =
        match init_expr_stm_o with
          | None -> while_partial |@| loc
          | Some(iestm) -> Ast.Block([
            iestm;
            Ast.Stmt(while_partial |@| while_partial_loc) |@| while_partial_loc
          ]) |@| loc
      in
      for_as_while
    }
    (* https://stackoverflow.com/questions/12731922/reforming-the-grammar-to-remove-shift-reduce-conflict-in-if-then-else *)
    | IF LEFT_PAREN expr_comma RIGHT_PAREN stmt %prec THEN
    { 
      let loc = Location.to_code_position $loc in
      Ast.If($3, $5, Ast.Block([]) |@| Location.to_code_position($endpos($5), $endpos($5))) |@| loc
    }
    | IF LEFT_PAREN expr_comma RIGHT_PAREN stmt ELSE stmt
    { 
      let loc = Location.to_code_position $loc in
      Ast.If($3, $5, $7) |@| loc
    }
  ;

expr:
  | lexpr_access
    { $1 }
  | rexpr
    { $1 }
  ;

expr_comma:
  | expr
    { $1 }
  | comma %prec LESS_THAN_COMMA
    { $1 |@| Location.to_code_position $loc }
  ;
 
comma: 
  | expr_comma COMMA expr_comma
    { Ast.Comma([$1; $3])}
  | comma COMMA expr_comma
    { 
      match $1 with
      | Ast.Comma(exprs) -> Ast.Comma($3::exprs)
      | _ -> assert false
    }
;


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
  | lexpr LEFT_BRACKET expr_comma RIGHT_BRACKET
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
      Ast.Null |@| loc 
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
  | lexpr ASSIGN expr_comma
    { 
      let loc = Location.to_code_position $loc in
      Ast.Assign($1, $3) |@| loc
    }
  | expr_comma binop expr_comma
    { 
      let loc = Location.to_code_position $loc in
      Ast.BinaryOp($2, $1, $3) |@| loc
    }
  | uop expr_comma
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