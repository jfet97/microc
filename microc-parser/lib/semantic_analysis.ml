exception Semantic_error of Location.code_pos * string

open Ast

(* these are the actual types of our entities, functions included, the one from ast were just annotations *)
type ttype =
  | TInt
  | TBool
  | TChar
  | TVoid
  | TPtr of ttype
  | TArray of ttype * int option
  | TFun of ttype * ttype
[@@deriving show]

let rec from_ast_type t =
  match t with
  | Ast.TypI -> TInt
  | Ast.TypB -> TBool
  | Ast.TypV -> TVoid
  | Ast.TypC -> TChar
  | Ast.TypP tt -> TPtr (from_ast_type tt)
  | Ast.TypA (bt, oi) -> TArray (from_ast_type bt, oi)

let rec check_type_equality t1 t2 =
  match (t1, t2) with
  | TFun (tp1, tr1), TFun (tp2, tr2) ->
      check_type_equality tp1 tp2 && check_type_equality tr1 tr2
  | TArray (ta1, Some size1), TArray (ta2, Some size2) ->
      size1 == size2 && check_type_equality ta1 ta2
  | TArray (ta1, _), TArray (ta2, _) -> check_type_equality ta1 ta2
  | TPtr tp1, TPtr tp2 -> check_type_equality tp1 tp2
  | x, y -> if x = y then true else false

let is_type_array t = match t with TArray (_, _) -> true | _ -> false
let is_type_fun t = match t with TFun (_, _) -> true | _ -> false

let remove_node_annotations annotated_node =
  match annotated_node with { loc; node } -> node

let prelude = [ ("getint", TFun (TVoid, TInt)); ("print", TFun (TInt, TVoid)) ]

let rec get_expression_type gamma expr =
  match remove_node_annotations expr with
  | Access v -> get_access_type gamma v
  | Assign (lhs, rhs) ->
      let lhs_t = get_access_type gamma lhs in
      let rhs_t = get_expression_type gamma rhs in
      if is_type_array lhs_t then
        raise (Utils.raise_semantic_error expr.loc "Cannot reassign an array")
      else if is_type_fun lhs_t then
        raise (Utils.raise_semantic_error expr.loc "Cannot reassign a function")
      else if check_type_equality lhs_t rhs_t then rhs_t
      else
        Utils.raise_semantic_error expr.loc
          ("Cannot assign " ^ show_ttype rhs_t ^ " to " ^ show_ttype lhs_t)
  | Addr a -> TPtr (get_access_type gamma a)
  | ILiteral _ -> TInt
  | BLiteral _ -> TBool
  | CLiteral _ -> TChar
  | UnaryOp (op, ex) -> (
      match (op, get_expression_type gamma ex) with
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
        (* TODO: refactor matching o first then expression types*)
        (op, get_expression_type gamma ex1, get_expression_type gamma ex2)
      with
      | Add, TInt, TInt -> TInt
      | Sub, TInt, TInt -> TInt
      | Mult, TInt, TInt -> TInt
      | Div, TInt, TInt -> TInt
      | Mod, TInt, TInt -> TInt
      | Equal, TInt, TInt -> TBool
      | Neq, TInt, TInt -> TBool
      | Less, TInt, TInt -> TBool
      | Leq, TInt, TInt -> TBool
      | Greater, TInt, TInt -> TBool
      | Geq, TInt, TInt -> TBool
      | And, TBool, TBool -> TBool
      | Or, TBool, TBool -> TBool
      | Equal, TBool, TBool -> TBool
      | Equal, TChar, TChar -> TBool
      | Equal, TArray (_, _), TArray (_, _) ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator cannot be applied to arrays")
      | Equal, TFun (_, _), TFun (_, _) ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator cannot be applied to functions")
      | Equal, TPtr tp1, TPtr tp2 when check_type_equality tp1 tp2 -> TBool
      | Neq, TBool, TBool -> TBool
      | Neq, TChar, TChar -> TBool
      | Neq, TArray (_, _), TArray (_, _) ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator cannot be applied to arrays")
      | Neq, TFun (_, _), TFun (_, _) ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator cannot be applied to functions")
      | Neq, TPtr tp1, TPtr tp2 when check_type_equality tp1 tp2 -> TBool
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
           ^ " operator's arguments must be booleans")
      | Equal, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must have the same type")
      | Neq, _, _ ->
          Utils.raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must have the same type"))
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
                if check_type_equality param_t (get_expression_type gamma arg)
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
                if check_type_equality param_t (get_expression_type gamma arg)
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

and get_access_type gamma access =
  match remove_node_annotations access with
  (* get type of variable in case of variable access *)
  | AccVar id -> (
      try match Symbol_table.lookup id gamma with t -> t
      with _ -> Utils.raise_semantic_error access.loc "Variable not in scope")
  (* ensure we're dereferencing a pointer *)
  | AccDeref ex -> (
      let t = get_expression_type gamma ex in
      match t with
      | TPtr typ -> typ
      | _ -> Utils.raise_semantic_error access.loc "Dereferencing a non-pointer"
      )
  (* ensure we're indexing into an array and that the index is an integer *)
  | AccIndex (b, idx) -> (
      let array_t = get_access_type gamma b in
      match array_t with
      | TArray (array_typ, _) ->
          if check_type_equality (get_expression_type gamma idx) TInt then
            array_typ
          else Utils.raise_semantic_error access.loc "Index is not an int"
      | _ -> Utils.raise_semantic_error access.loc "Indexing a non-array")

(* return unit instead of tvoid *)
let rec get_statement_type gamma stmt expected_ret_type =
  match remove_node_annotations stmt with
  | If (guard, then_stmt, else_stmt) ->
      if check_type_equality (get_expression_type gamma guard) TBool then
        let _ = get_statement_type gamma then_stmt expected_ret_type in
        let _ = get_statement_type gamma else_stmt expected_ret_type in
        TVoid
      else Utils.raise_semantic_error guard.loc "The if guard must be a boolean"
  | While (guard, stmt) ->
      if check_type_equality (get_expression_type gamma guard) TBool then
        get_statement_type gamma stmt expected_ret_type
      else
        Utils.raise_semantic_error guard.loc "The while guard must be a boolean"
  | Expr expr ->
      let _ = get_expression_type gamma expr in
      TVoid
  | Return oexpr -> (
      match oexpr with
      | Some expr ->
          let expr_t = get_expression_type gamma expr in
          if check_type_equality expr_t expected_ret_type then TVoid
          else
            Utils.raise_semantic_error expr.loc
              ("Expected "
              ^ show_ttype expected_ret_type
              ^ " return type but found " ^ show_ttype expr_t)
      | None ->
          if check_type_equality TVoid expected_ret_type then TVoid
          else
            Utils.raise_semantic_error stmt.loc
              ("Expected "
              ^ show_ttype expected_ret_type
              ^ " return type but found void"))
  | Block stmt_list ->
      let block_gamma = Symbol_table.begin_block gamma in
      let _ =
        List.iter
          (fun stmt ->
            let _ =
              get_statement_or_declaration_type block_gamma stmt
                expected_ret_type
            in
            ())
          stmt_list
      in
      TVoid

(* return unit instead of tvoid *)
and get_statement_or_declaration_type gamma stmtordec expected_ret_type =
  match remove_node_annotations stmtordec with
  | Dec (typ, id) -> (
      let t = from_ast_type typ in
      match t with
      | TVoid ->
          Utils.raise_semantic_error stmtordec.loc
            "Cannot declare a void variable"
      | TArray (_, None) ->
          Utils.raise_semantic_error stmtordec.loc
            "Cannot declare an array without specifiying its size"
      | TArray (_, Some size) when size <= 0 ->
          Utils.raise_semantic_error stmtordec.loc "Arrays must have size > 0"
      | TArray (ta, _) when check_type_equality ta TVoid ->
          Utils.raise_semantic_error stmtordec.loc
            "Cannot declare an array of void"
      (* NON ESPRIMIBILE COME DICHIARAZIONE NELL'AST*)
      (* | TArray (ta, _) when is_type_fun ta ->
          Utils.raise_semantic_error stmtordec.loc
            "Cannot declare an array of functions" *)
      | TArray (ta, _) when is_type_array ta ->
          Utils.raise_semantic_error stmtordec.loc
            "Cannot declare a multidimensional array"
      | TPtr tp when is_type_array tp ->
          Utils.raise_semantic_error stmtordec.loc
            "Cannot declare a pointer to array"
      | _ ->
          let _ = Symbol_table.add_entry id t gamma in
          TVoid)
  | Stmt stmt -> get_statement_type gamma stmt expected_ret_type

let type_check _p = failwith "Not implemented yet"