exception Semantic_error of Location.code_pos * string

let raise_semantic_error code_position msg =
  raise (Semantic_error (code_position, msg))

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

let rec typecheck_expression gamma expr =
  match remove_node_annotations expr with
  | Access v -> typecheck_access gamma v
  | Assign (lhs, rhs) ->
      let lhs_t = typecheck_access gamma lhs in
      let rhs_t = typecheck_expression gamma rhs in
      if is_type_array lhs_t then
        raise (raise_semantic_error expr.loc "Cannot reassign an array")
      else if is_type_fun lhs_t then
        raise (raise_semantic_error expr.loc "Cannot reassign a function")
      else if check_type_equality lhs_t rhs_t then rhs_t
      else
        raise_semantic_error expr.loc
          ("Cannot assign " ^ show_ttype rhs_t ^ " to " ^ show_ttype lhs_t)
  | Addr a -> TPtr (typecheck_access gamma a)
  | ILiteral _ -> TInt
  | BLiteral _ -> TBool
  | CLiteral _ -> TChar
  | UnaryOp (op, ex) -> (
      match (op, typecheck_expression gamma ex) with
      | Neg, TInt -> TInt
      | Neg, _ ->
          raise_semantic_error expr.loc
            "The unary - operator must be applied to a number"
      | Ast.Not, TBool -> TBool
      | Ast.Not, _ ->
          raise_semantic_error expr.loc
            "The unary ! operator must be applied to a boolean")
  | BinaryOp (op, ex1, ex2) -> (
      match
        (* TODO: refactor matching o first then expression types*)
        (op, typecheck_expression gamma ex1, typecheck_expression gamma ex2)
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
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator cannot be applied to arrays")
      | Equal, TFun (_, _), TFun (_, _) ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator cannot be applied to functions")
      | Equal, TPtr tp1, TPtr tp2 when check_type_equality tp1 tp2 -> TBool
      | Neq, TBool, TBool -> TBool
      | Neq, TChar, TChar -> TBool
      | Neq, TArray (_, _), TArray (_, _) ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator cannot be applied to arrays")
      | Neq, TFun (_, _), TFun (_, _) ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator cannot be applied to functions")
      | Neq, TPtr tp1, TPtr tp2 when check_type_equality tp1 tp2 -> TBool
      | Add, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Sub, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Mult, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Div, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Mod, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Less, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Leq, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Greater, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | Geq, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be numbers")
      | And, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be booleans")
      | Or, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must be booleans")
      | Equal, _, _ ->
          raise_semantic_error expr.loc
            ("The binary " ^ Ast.show_binop op
           ^ " operator's arguments must have the same type")
      | Neq, _, _ ->
          raise_semantic_error expr.loc
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
                  raise_semantic_error expr.loc
                    ("Too few arguments for function " ^ id)
            (* function with one argument, an argument *)
            | TFun (param_t, ret_t), arg :: [] ->
                if check_type_equality param_t (typecheck_expression gamma arg)
                then
                  (* a function is returned but we've ended the arguments *)
                  if is_type_fun ret_t then
                    raise_semantic_error expr.loc ("Too few arguments for " ^ id)
                  else ret_t
                else
                  raise_semantic_error expr.loc
                    ("Wrong argument type when calling" ^ id)
            (* function with one argument, more arguments left *)
            | TFun (param_t, ret_t), arg :: xs ->
                if check_type_equality param_t (typecheck_expression gamma arg)
                then check_args ret_t xs
                else
                  raise_semantic_error expr.loc
                    ("Wrong argument type when calling" ^ id)
            (* some arguments left but ft is no more a function, so it was a previous end-result *)
            | _ -> raise_semantic_error expr.loc ("Too many arguments for" ^ id)
          in
          check_args fun_t args
        else raise_semantic_error expr.loc (id ^ " is not a function")
      with _ -> raise_semantic_error expr.loc "Variable not in scope")

and typecheck_access gamma access =
  match remove_node_annotations access with
  (* get type of variable in case of variable access *)
  | AccVar id -> (
      try match Symbol_table.lookup id gamma with t -> t
      with _ -> raise_semantic_error access.loc "Variable not in scope")
  (* ensure we're dereferencing a pointer *)
  | AccDeref ex -> (
      let t = typecheck_expression gamma ex in
      match t with
      | TPtr typ -> typ
      | _ -> raise_semantic_error access.loc "Dereferencing a non-pointer")
  (* ensure we're indexing into an array and that the index is an integer *)
  | AccIndex (b, idx) -> (
      let array_t = typecheck_access gamma b in
      match array_t with
      | TArray (array_typ, _) ->
          if check_type_equality (typecheck_expression gamma idx) TInt then
            array_typ
          else raise_semantic_error access.loc "Index is not an int"
      | _ -> raise_semantic_error access.loc "Indexing a non-array")

(* return unit instead of tvoid *)
let rec typecheck_statement gamma stmt expected_ret_type is_parent_function =
  match remove_node_annotations stmt with
  | If (guard, then_stmt, else_stmt) ->
      if check_type_equality (typecheck_expression gamma guard) TBool then
        let _ =
          typecheck_statement gamma then_stmt expected_ret_type
            is_parent_function
        in
        let _ =
          typecheck_statement gamma else_stmt expected_ret_type
            is_parent_function
        in
        TVoid
      else raise_semantic_error guard.loc "The if guard must be a boolean"
  | While (guard, stmt) ->
      if check_type_equality (typecheck_expression gamma guard) TBool then
        typecheck_statement gamma stmt expected_ret_type is_parent_function
      else raise_semantic_error guard.loc "The while guard must be a boolean"
  | Expr expr ->
      let _ = typecheck_expression gamma expr in
      TVoid
  | Return oexpr -> (
      match oexpr with
      | Some expr ->
          let expr_t = typecheck_expression gamma expr in
          if check_type_equality expr_t expected_ret_type then TVoid
          else
            raise_semantic_error expr.loc
              ("Expected "
              ^ show_ttype expected_ret_type
              ^ " return type but found " ^ show_ttype expr_t)
      | None ->
          if check_type_equality TVoid expected_ret_type then TVoid
          else
            raise_semantic_error stmt.loc
              ("Expected "
              ^ show_ttype expected_ret_type
              ^ " return type but found void"))
  | Block stmt_list ->
      let block_gamma =
        if is_parent_function then gamma else Symbol_table.begin_block gamma
      in
      let _ =
        List.iter
          (fun stmt ->
            let _ =
              typecheck_statement_or_declaration block_gamma stmt
                expected_ret_type
            in
            ())
          stmt_list
      in
      TVoid

(* return unit instead of tvoid *)
and typecheck_statement_or_declaration gamma stmtordec expected_ret_type =
  match remove_node_annotations stmtordec with
  | Dec (typ, id) -> (
      let t = from_ast_type typ in
      match t with
      | TVoid ->
          raise_semantic_error stmtordec.loc "Cannot declare a void variable"
      | TArray (_, None) ->
          raise_semantic_error stmtordec.loc
            "Cannot declare an array without specifiying its size"
      | TArray (_, Some size) when size <= 0 ->
          raise_semantic_error stmtordec.loc "Arrays must have size > 0"
      | TArray (ta, _) when check_type_equality ta TVoid ->
          raise_semantic_error stmtordec.loc "Cannot declare an array of void"
      (* NON ESPRIMIBILE COME DICHIARAZIONE NELL'AST*)
      (* | TArray (ta, _) when is_type_fun ta ->
          raise_semantic_error stmtordec.loc
            "Cannot declare an array of functions" *)
      | TArray (ta, _) when is_type_array ta ->
          raise_semantic_error stmtordec.loc
            "Cannot declare a multidimensional array"
      | TPtr tp when is_type_array tp ->
          raise_semantic_error stmtordec.loc "Cannot declare a pointer to array"
      | _ ->
          let _ = Symbol_table.add_entry id t gamma in
          TVoid)
  | Stmt stmt -> typecheck_statement gamma stmt expected_ret_type false

let typecheck_topdeclaration gamma topdlecl =
  match remove_node_annotations topdlecl with
  | Fundecl { typ; fname; formals; body } -> (
      let fun_gamma = Symbol_table.begin_block gamma in
      let return_t = from_ast_type typ in
      match return_t with
      | TVoid | TBool | TChar | TInt ->
          (* add params inside function scope *)
          let formals_t =
            List.map (fun (typ, id) -> (from_ast_type typ, id)) formals
          in
          let _ =
            List.iter
              (fun (t, id) ->
                match t with
                | TVoid ->
                    raise_semantic_error topdlecl.loc
                      "Cannot declare a void parameter"
                | TArray (_, Some size) when size <= 0 ->
                    raise_semantic_error topdlecl.loc
                      "Arrays must have size > 0"
                | TArray (ta, _) when check_type_equality ta TVoid ->
                    raise_semantic_error topdlecl.loc
                      "Cannot declare an array of void parameter"
                | TArray (ta, _) when is_type_array ta ->
                    raise_semantic_error topdlecl.loc
                      "Cannot declare a multidimensional array parameter"
                | TPtr tp when is_type_array tp ->
                    raise_semantic_error topdlecl.loc
                      "Cannot declare a pointer to array parameter"
                | _ ->
                    let _ = Symbol_table.add_entry id t fun_gamma in
                    ())
              formals_t
          in

          (* add the function into the global scope *)
          let fun_t =
            List.fold_right
              (fun (param_t, _) acc -> TFun (param_t, acc))
              formals_t return_t
          in
          let _ = Symbol_table.add_entry fname fun_t gamma in
          (* pass in the expected return type: all inners return statements should be complient *)
          (* delayed computation to firstly add all the functions to the global scope *)
          fun () ->
            let _ = typecheck_statement fun_gamma body return_t true in
            TVoid
      | _ ->
          raise_semantic_error topdlecl.loc
            "A function can only return void, int, bool, char")
  | Vardec (typ, id) -> (
      let t = from_ast_type typ in
      match t with
      | TVoid ->
          raise_semantic_error topdlecl.loc "Cannot declare a void variable"
      | TArray (_, None) ->
          raise_semantic_error topdlecl.loc
            "Cannot declare an array without specifiying its size"
      | TArray (_, Some size) when size <= 0 ->
          raise_semantic_error topdlecl.loc "Arrays must have size > 0"
      | TArray (ta, _) when check_type_equality ta TVoid ->
          raise_semantic_error topdlecl.loc "Cannot declare an array of void"
      | TArray (ta, _) when is_type_array ta ->
          raise_semantic_error topdlecl.loc
            "Cannot declare a multidimensional array"
      | TPtr tp when is_type_array tp ->
          raise_semantic_error topdlecl.loc "Cannot declare a pointer to array"
      | _ ->
          let _ = Symbol_table.add_entry id t gamma in
          fun () -> TVoid)

let type_check p =
  match p with
  | Prog program ->
      let global = Symbol_table.begin_block Symbol_table.empty_table in
      let _ = Symbol_table.add_entry "getint" (TFun (TVoid, TInt)) global in
      let _ = Symbol_table.add_entry "print" (TFun (TInt, TVoid)) global in
      let delayed_bodies_check =
        List.map
          (fun topdecl -> typecheck_topdeclaration global topdecl)
          program
      in
      let _ = List.iter (fun d -> d () |> ignore) delayed_bodies_check in
      p
