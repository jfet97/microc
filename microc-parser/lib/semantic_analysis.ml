exception Semantic_error of Location.code_pos * string

open Ast

type ttype =
  | TInt
  | TBool
  | TChar
  | TVoid
  | TPtr of ttype
  | TArray of ttype * int option
  | TFun of ttype * ttype
[@@deriving show]

let rec check_type_equality t1 t2 =
  match (t1, t2) with
  | TArray (ta1, _), TArray (ta2, _) -> check_type_equality ta1 ta2
  | TPtr tp1, TPtr tp2 -> check_type_equality tp1 tp2
  | x, y -> if x = y then true else false

let is_type_array t = match t with TArray (_, _) -> true | _ -> false
let is_type_fun t = match t with TFun (_, _) -> true | _ -> false

let remove_node_annotations annotated_node =
  match annotated_node with { loc; node } -> node

let prelude = [ ("getint", TFun (TVoid, TInt)); ("print", TFun (TInt, TVoid)) ]

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
        try match Symbol_table.lookup id gamma with t -> t
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
            if check_type_equality (type_of_expression gamma idx) TInt then
              a_typ
            else Utils.raise_semantic_error expr.loc "Index is not an int"
        | _ -> Utils.raise_semantic_error expr.loc "Indexing a non-array")
  in
  match remove_node_annotations expr with
  | Access v -> type_of_variable gamma v
  | Assign (lhs, rhs) ->
      let lhs_t = type_of_variable gamma lhs in
      let rhs_t = type_of_expression gamma rhs in
      if is_type_array lhs_t then
        raise (Utils.raise_semantic_error expr.loc "Cannot reassign an array")
      else if check_type_equality lhs_t rhs_t then rhs_t
      else
        Utils.raise_semantic_error expr.loc
          ("Cannot assign " ^ show_ttype rhs_t ^ " to " ^ show_ttype lhs_t)
  | Addr a -> TPtr (type_of_variable gamma a)
  | ILiteral _ -> TInt
  | BLiteral _ -> TBool
  | CLiteral _ -> TChar
  | UnaryOp (op, ex) -> (
      match (op, type_of_expression gamma ex) with
      | Neg, TInt -> TInt
      | Neg, _ ->
          Utils.raise_semantic_error expr.loc
            "The unary - operator must be applied to a number"
      | Ast.Not, TBool -> TBool
      | Ast.Not, _ ->
          Utils.raise_semantic_error expr.loc
            "The unary ! operator must be applied to a boolean")
  | BinaryOp (op, ex1, ex2) -> (
      match
        (op, type_of_expression gamma ex1, type_of_expression gamma ex2)
      with
      | Add, TInt, TInt -> TInt
      | Sub, TInt, TInt -> TInt
      | Mult, TInt, TInt -> TInt
      | Div, TInt, TInt -> TInt
      | Mod, TInt, TInt -> TInt
      | Equal, TInt, TInt -> TInt
      | Neq, TInt, TInt -> TInt
      | Less, TInt, TInt -> TInt
      | Leq, TInt, TInt -> TInt
      | Greater, TInt, TInt -> TInt
      | Geq, TInt, TInt -> TInt
      | And, TBool, TBool -> TBool
      | Or, TBool, TBool -> TBool
      | Add, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Sub, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Mult, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Div, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Mod, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Equal, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Neq, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Less, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Leq, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Greater, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Geq, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | And, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be booleans")
      | Or, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be booleans"))
  | Call (id, args) -> (
      try
        let fun_t = Symbol_table.lookup id gamma in
        if is_type_fun fun_t then
          let rec check_args ft args =
            match (ft, args) with
            (* function with no arguments, no arguments *)
            | TFun (param_t, ret_t), [] ->
                if check_type_equality param_t TVoid then ret_t
                else
                  Utils.raise_semantic_error expr.loc
                    ("Too few arguments for function " ^ id)
            (* function with one argument, an argument *)
            | TFun (param_t, ret_t), arg :: [] ->
                if check_type_equality param_t (type_of_expression gamma arg)
                then
                  (* a function is returned but we've ended the arguments *)
                  if is_type_fun ret_t then
                    Utils.raise_semantic_error expr.loc
                      ("Too few arguments for " ^ id)
                  else ret_t
                else
                  Utils.raise_semantic_error expr.loc
                    ("Wrong argument type when calling" ^ id)
            (* function with one argument, more arguments left *)
            | TFun (param_t, ret_t), arg :: xs ->
                if check_type_equality param_t (type_of_expression gamma arg)
                then check_args ret_t xs
                else
                  Utils.raise_semantic_error expr.loc
                    ("Wrong argument type when calling" ^ id)
            (* some arguments left but ft is no more a function, so it was a previous end-result *)
            | _ ->
                Utils.raise_semantic_error expr.loc
                  ("Too many arguments for" ^ id)
          in
          check_args fun_t args
        else Utils.raise_semantic_error expr.loc (id ^ " is not a function")
      with _ -> Utils.raise_semantic_error expr.loc "Variable not in scope")

let type_check _p = failwith "Not implemented yet"