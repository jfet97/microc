exception Semantic_error of Location.code_pos * string

open Ast

type ttype =
  | TInt
  | TBool
  | TChar
  | TVoid
  | TNull
  | TPtr of ttype
  | TArray of ttype * int option
  | TFun of ttype * ttype

type 'v env = (string * 'v) list

let remove_node_annotations annotated_node =
  match annotated_node with { loc; node } -> node

let prelude =
  [
    ("+", TFun (TInt, TFun (TInt, TInt)));
    ("-", TFun (TInt, TFun (TInt, TInt)));
    ("*", TFun (TInt, TFun (TInt, TInt)));
    ("/", TFun (TInt, TFun (TInt, TInt)));
    ("%", TFun (TInt, TFun (TInt, TInt)));
    ("<", TFun (TInt, TFun (TInt, TBool)));
    (">", TFun (TInt, TFun (TInt, TBool)));
    ("<=", TFun (TInt, TFun (TInt, TBool)));
    (">=", TFun (TInt, TFun (TInt, TBool)));
    ("||", TFun (TBool, TFun (TBool, TBool)));
    ("&&", TFun (TBool, TFun (TBool, TBool)));
    ("!", TFun (TBool, TBool));
    ("NULL", TNull);
    ("getint", TFun (TVoid, TInt));
    ("print", TFun (TInt, TVoid));
  ]

let rec check_type_equality t1 t2 =
  match (t1, t2) with
  | TArray (ta1, _), TArray (ta2, _) -> check_type_equality ta1 ta2
  | TPtr tp1, TPtr tp2 -> check_type_equality tp1 tp2
  | x, y -> if x = y then true else false

let rec from_ast_type t =
  match t with
  | Ast.TypI -> TInt
  | Ast.TypB -> TBool
  | Ast.TypV -> TVoid
  | Ast.TypC -> TChar
  | Ast.TypP tt -> TPtr (from_ast_type tt)
  | Ast.TypA (bt, oi) -> TArray (from_ast_type bt, oi)

let rec type_of_expression gamma expr =
  let rec type_of_variable gamma expr =
    match remove_node_annotations expr with
    (* get type of variable in case of variable access *)
    | AccVar id -> (
        try match Symbol_table.lookup id gamma with ttype -> ttype
        with _ -> Utils.raise_semantic_error expr.loc "Variable not in scope")
    (* ensure we're dereferencing a pointer *)
    | AccDeref ex -> (
        let typ = type_of_expression gamma ex in
        match typ with
        | TPtr typ -> typ
        | _ -> Utils.raise_semantic_error expr.loc "Dereferencing a non-pointer"
        )
    (* ensure we're indexing into an array and that the index is an integer *)
    | AccIndex (b, idx) -> (
        let base_type = type_of_variable gamma b in
        match base_type with
        | TArray (a_typ, _) ->
            if  check_type_equality (type_of_expression gamma idx) TInt then a_typ
            else Utils.raise_semantic_error expr.loc "Index is not an int"
        | _ -> Utils.raise_semantic_error expr.loc "Indexing a non-array")
  in
  match remove_node_annotations expr with _ -> assert false

let type_check _p = failwith "Not implemented yet"