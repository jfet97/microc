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

type 'a annotated_node = {
  loc : Location.code_pos; [@opaque]
  node : 'a;
}
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
  | Dec of typ * identifier (* Local variable declaration  *)
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

and topdecl_node = Fundecl of fun_decl | Vardec of typ * identifier
[@@deriving show]

type program = Prog of topdecl list [@@deriving show]



(* pretty print *)


let (|+|) s1 s2 = String.concat "" [s1; s2];;

let rec int_to_spaces i =
  if i <= 0 then ""
  else if i = 1 then " "
  else int_to_spaces(i-1) |+| " "

let print_string_indented i s =
  Printf.printf "%s" ((int_to_spaces i) |+| s)

let rec print_program i prog = 
  print_string_indented i "program\n";
  match prog with
  | Prog(tls) -> List.iter (print_topdecl (i + 2)) tls

and print_topdecl i tl =
  print_string_indented i "topdecl\n";
  match tl.node with
  | Fundecl(fd) -> print_fundecl (i+2) fd
  | Vardec(typ, id) -> print_vardec (i+2) typ id

and print_fundecl i fd =
  print_string_indented i "fundecl\n"

and print_vardec i typ id =
  print_string_indented i "vardec";

;;


let print ast = print_program 0 ast;