type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
[@@deriving show]

type uop = Neg | Not [@@deriving show]
type identifier = string [@@deriving show]

type 'a annotated_node = { loc : Location.code_pos; [@opaque] node : 'a }
[@@deriving show]

type typ =
  | TypI (* Type int *)
  | TypB (* Type bool *)
  | TypC (* Type char *)
  | TypA of typ * int option (* Array type *)
  | TypP of typ (* Pointer type  *)
  | TypV (* Type void  *)
[@@deriving show]

and expr = expr_node annotated_node

and expr_node =
  | Access of access (* x  or  *p  or  a[e]  *)
  | Assign of access * expr (* x=e  or  *p=e  or  a[e]=e   *)
  | Addr of access (* &x   or  &*p   or  &a[e]  *)
  | ILiteral of int (* Integer literal  *)
  | CLiteral of char (* Char literal    *)
  | BLiteral of bool (* Bool literal    *)
  | UnaryOp of uop * expr (* Unary primitive operator  *)
  | BinaryOp of binop * expr * expr (* Binary primitive operator  *)
  | Call of identifier * expr list (* Function call f(...)    *)
  | Null
[@@deriving show]

and access = access_node annotated_node

and access_node =
  | AccVar of identifier (* Variable access    x  *)
  | AccDeref of expr (* Pointer dereferencing  *p *)
  | AccIndex of access * expr (* Array indexing   a[e] *)
[@@deriving show]

and stmt = stmt_node annotated_node

and stmt_node =
  | If of expr * stmt * stmt (* Conditional    *)
  | While of expr * stmt (* While loop     *)
  | Expr of expr (* Expression statement   e;  *)
  | Return of expr option (* Return statement  *)
  | Block of stmtordec list (* Block: grouping and scope *)
[@@deriving show]

and stmtordec = stmtordec_node annotated_node

and stmtordec_node =
  (* Local variabls declarations plus optional initializers *)
  | Dec of (typ * identifier * expr list) list
  | Stmt of stmt (* A statement *)
[@@deriving show]

type fun_decl = {
  typ : typ;
  fname : string;
  formals : (typ * identifier) list;
  body : stmt;
}
[@@deriving show]

type topdecl = topdecl_node annotated_node

and topdecl_node =
  | Fundecl of fun_decl
  | Vardec of (typ * identifier * expr list) list
[@@deriving show]

type program = Prog of topdecl list [@@deriving show]

(* pretty print *)

let ( |+| ) s1 s2 = String.concat "" [ s1; s2 ]

let rec int_to_spaces i =
  if i <= 0 then "" else if i = 1 then " " else int_to_spaces (i - 1) |+| " "

let sprint_string_indented i s = Printf.sprintf "%s" (int_to_spaces i |+| s)

let rec sprint_program i prog =
  let prog_str =
    match prog with
    | Prog tls ->
        List.fold_left (fun a c -> a |+| sprint_topdecl (i + 2) c) "" tls
  in
  sprint_string_indented i "program\n" |+| prog_str

and sprint_topdecl i tl =
  let tl_str =
    match tl.node with
    | Fundecl fd -> sprint_fundecl (i + 2) fd
    | Vardec inits ->
        let s =
          List.fold_left
            (fun s (typ, id, oinit) -> s |+| sprint_vardec (i + 2) typ id oinit)
            "" inits
        in
        s ^ "\n"
  in
  sprint_string_indented i "topdecl\n" |+| tl_str

and sprint_fundecl i fn =
  sprint_string_indented i "fundecl\n"
  |+| sprint_typ (i + 2) fn.typ
  |+| "\n"
  |+| sprint_string_indented (i + 2) "fname "
  |+| fn.fname |+| "\n"
  |+| sprint_string_indented (i + 2) "formals\n"
  |+| List.fold_left
        (fun a c -> a |+| sprint_vardec (i + 4) (fst c) (snd c) [])
        "" fn.formals
  |+| sprint_string_indented (i + 2) "body\n"
  |+| sprint_stmt (i + 4) fn.body

and sprint_vardec i typ id init_exprs =
  let vardec_s =
    sprint_string_indented i
      ("vardec\n" |+| sprint_typ (i + 2) typ |+| " " |+| id |+| "\n")
  in
  List.fold_left
    (fun s init_expr -> s |+| sprint_expr (i + 2) init_expr |+| "\n")
    vardec_s init_exprs

and sprint_typ i typ =
  let rec aux = function
    | TypI -> "int"
    | TypC -> "char"
    | TypV -> "void"
    | TypB -> "bool"
    | TypP typp -> "*(" |+| aux typp |+| ")"
    | TypA (typa, io) -> (
        let typas = "(" |+| aux typa |+| ")" in
        match io with
        | None -> typas |+| "[]"
        | Some i -> typas |+| "[" |+| string_of_int i |+| "]")
  in
  sprint_string_indented i ("typ " |+| aux typ)

and sprint_stmt i stmt =
  let stmt_str =
    match stmt.node with
    | If (ex, st1, st2) ->
        sprint_string_indented (i + 2) "if\n"
        |+| sprint_expr (i + 4) ex
        |+| sprint_string_indented (i + 4) "then\n"
        |+| sprint_stmt (i + 6) st1
        |+| sprint_string_indented (i + 4) "else\n"
        |+| sprint_stmt (i + 6) st2
    | While (ex, st) ->
        sprint_string_indented (i + 2) "while\n"
        |+| sprint_expr (i + 4) ex
        |+| sprint_stmt (i + 4) st
    | Return exo -> (
        sprint_string_indented (i + 2) "return\n"
        |+| match exo with None -> "" | Some ex -> sprint_expr (i + 4) ex)
    | Expr ex -> sprint_expr (i + 2) ex
    | Block sts ->
        sprint_string_indented (i + 2) "block\n"
        |+| List.fold_left (fun a c -> a |+| sprint_stmtordec (i + 4) c) "" sts
  in
  sprint_string_indented i "stmt\n" |+| stmt_str

and sprint_stmtordec i stmtordec =
  let stmtordec_str =
    match stmtordec.node with
    | Dec inits ->
        let s =
          List.fold_left
            (fun s (typ, id, oinit) -> s |+| sprint_vardec (i + 2) typ id oinit)
            "" inits
        in
        s ^ "\n"
    | Stmt st -> sprint_stmt (i + 2) st
  in
  sprint_string_indented i "stmtordec\n" |+| stmtordec_str

and sprint_expr i expr =
  let expr_str =
    match expr.node with
    | Access ac -> sprint_access (i + 2) ac
    | Assign (ac, ex) ->
        sprint_string_indented (i + 2) "assign\n"
        |+| sprint_access (i + 4) ac
        |+| sprint_expr (i + 4) ex
    | Addr ac ->
        sprint_string_indented (i + 2) "addr\n" |+| sprint_access (i + 4) ac
    | ILiteral i' ->
        sprint_string_indented (i + 2)
          ("iliteral " |+| string_of_int i' |+| "\n")
    | CLiteral c ->
        sprint_string_indented (i + 2) ("cliteral " |+| String.make 1 c |+| "\n")
    | BLiteral b ->
        sprint_string_indented (i + 2)
          ("bliteral " |+| string_of_bool b |+| "\n")
    | UnaryOp (uop, ex) ->
        sprint_string_indented (i + 2) "unaryop\n"
        |+| sprint_uop (i + 4) uop
        |+| sprint_expr (i + 4) ex
    | BinaryOp (binop, ex1, ex2) ->
        sprint_string_indented (i + 2) "binaryop\n"
        |+| sprint_binop (i + 4) binop
        |+| sprint_expr (i + 4) ex1
        |+| sprint_expr (i + 4) ex2
    | Call (id, exs) ->
        sprint_string_indented (i + 2) "call\n"
        |+| sprint_string_indented (i + 4) "id "
        |+| id |+| "\n"
        |+| sprint_string_indented (i + 4) "parameters\n"
        |+| List.fold_left (fun a c -> a |+| sprint_expr (i + 6) c) "" exs
    | Null -> sprint_string_indented (i + 2) "NULL\n"
  in
  sprint_string_indented i "expr\n" |+| expr_str

and sprint_uop i uop =
  let uop_str =
    match uop with
    | Neg -> sprint_string_indented (i + 2) "neg\n"
    | Not -> sprint_string_indented (i + 2) "not\n"
  in
  sprint_string_indented i "uop\n" |+| uop_str

and sprint_binop i binop =
  let binop_str =
    match binop with
    | Add -> sprint_string_indented (i + 2) "add\n"
    | Sub -> sprint_string_indented (i + 2) "sub\n"
    | Mult -> sprint_string_indented (i + 2) "mult\n"
    | Div -> sprint_string_indented (i + 2) "div\n"
    | Mod -> sprint_string_indented (i + 2) "mod\n"
    | Equal -> sprint_string_indented (i + 2) "equal\n"
    | Neq -> sprint_string_indented (i + 2) "neq\n"
    | Less -> sprint_string_indented (i + 2) "less\n"
    | Leq -> sprint_string_indented (i + 2) "leq\n"
    | Greater -> sprint_string_indented (i + 2) "greater\n"
    | Geq -> sprint_string_indented (i + 2) "geq\n"
    | And -> sprint_string_indented (i + 2) "and\n"
    | Or -> sprint_string_indented (i + 2) "or\n"
  in
  sprint_string_indented i "binop\n" |+| binop_str

and sprint_access i ac =
  let ac_str =
    match ac.node with
    | AccVar id -> sprint_string_indented (i + 2) "accvar " |+| id |+| "\n"
    | AccDeref ex ->
        sprint_string_indented (i + 2) "accderef\n" |+| sprint_expr (i + 4) ex
    | AccIndex (ac', ex) ->
        sprint_string_indented (i + 2) "accindex\n"
        |+| sprint_access (i + 4) ac'
        |+| sprint_expr (i + 4) ex
  in
  sprint_string_indented i "access\n" |+| ac_str

let sprint_ast ast = sprint_program 0 ast