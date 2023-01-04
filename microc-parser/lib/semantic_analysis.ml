exception Semantic_error of Location.code_pos * string

let raise_semantic_error code_position msg =
  raise (Semantic_error (code_position, msg))

open Ast

(* these are the actual types of our entities, functions included, the ones from ast were just annotations *)
type ttype =
  | TInt
  | TBool
  | TChar
  | TVoid
  | TNull
  | TPtr of ttype
  | TArray of ttype * int option
  (* currying: argument, return *)
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
  | TPtr _, TNull | TNull, TPtr _ -> true
  | x, y -> if x = y then true else false

let is_type_array t = match t with TArray (_, _) -> true | _ -> false
let is_type_fun t = match t with TFun (_, _) -> true | _ -> false

let remove_node_annotations annotated_node =
  match annotated_node with { loc; node } -> node

let st_lookup_rethrow id gamma code_position msg =
  try Symbol_table.lookup id gamma
  with _ -> raise_semantic_error code_position msg

let st_add_entry_rethrow id t st code_position =
  try Symbol_table.add_entry id t st
  with _ ->
    raise_semantic_error code_position ("Cannot redeclare symbol " ^ id)

let check_variable_type t loc =
  match t with
  | TVoid -> raise_semantic_error loc "Cannot declare a void variable"
  | TArray (_, None) ->
      raise_semantic_error loc
        "Cannot declare an array without specifiying its size"
  | TArray (_, Some size) when size <= 0 ->
      raise_semantic_error loc "Arrays must have size > 0"
  | TArray (ta, _) when check_type_equality ta TVoid ->
      raise_semantic_error loc "Cannot declare an array of void"
  (* NOT EXPRESSIBLE AS DECLARATION IN THE AST *)
  (* | TArray (ta, _) when is_type_fun ta ->
      raise_semantic_error loc
        "Cannot declare an array of functions" *)
  | TArray (ta, _) when is_type_array ta ->
      raise_semantic_error loc "Cannot declare a multidimensional array"
  | TPtr tp when is_type_array tp ->
      raise_semantic_error loc "Cannot declare a pointer to array"
  | _ -> t

let check_parameter_type t loc =
  match t with
  | TVoid -> raise_semantic_error loc "Cannot declare a void parameter"
  | TArray (_, Some size) when size <= 0 ->
      raise_semantic_error loc "Arrays must have size > 0"
  | TArray (ta, _) when check_type_equality ta TVoid ->
      raise_semantic_error loc "Cannot declare an array of void parameter"
  | TArray (ta, _) when is_type_array ta ->
      raise_semantic_error loc
        "Cannot declare a multidimensional array parameter"
  | TPtr tp when is_type_array tp ->
      raise_semantic_error loc "Cannot declare a pointer to array parameter"
  | _ -> t

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
  | ILiteral i ->
      if i >= 2147483648 then raise_semantic_error expr.loc "Integer overflow"
      else if i <= -2147483649 then
        raise_semantic_error expr.loc "Integer underflow"
      else TInt
  | BLiteral _ -> TBool
  | CLiteral _ -> TChar
  | Null -> TNull
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
      let ex1_t = typecheck_expression gamma ex1 in
      let ex2_t = typecheck_expression gamma ex2 in
      match op with
      | Add | Sub | Mult | Div | Mod -> (
          match (ex1_t, ex2_t) with
          | TInt, TInt -> TInt
          | _, _ ->
              raise_semantic_error expr.loc
                ("The binary " ^ Ast.show_binop op
               ^ " operator's arguments must be numbers"))
      | Less | Leq | Greater | Geq -> (
          match (ex1_t, ex2_t) with
          | TInt, TInt -> TBool
          | _, _ ->
              raise_semantic_error expr.loc
                ("The binary " ^ Ast.show_binop op
               ^ " operator's arguments must be numbers"))
      | And | Or -> (
          match (ex1_t, ex2_t) with
          | TBool, TBool -> TBool
          | _, _ ->
              raise_semantic_error expr.loc
                ("The binary " ^ Ast.show_binop op
               ^ " operator's arguments must be booleans"))
      | Equal | Neq -> (
          match (ex1_t, ex2_t) with
          | TInt, TInt -> TBool
          | TBool, TBool -> TBool
          | TChar, TChar -> TBool
          | TPtr tp1, TPtr tp2 when check_type_equality tp1 tp2 -> TBool
          | TArray (_, _), TArray (_, _) ->
              raise_semantic_error expr.loc
                ("The binary " ^ Ast.show_binop op
               ^ " operator cannot be applied to arrays")
          | TFun (_, _), TFun (_, _) ->
              raise_semantic_error expr.loc
                ("The binary " ^ Ast.show_binop op
               ^ " operator cannot be applied to functions")
          | _, _ ->
              raise_semantic_error expr.loc
                ("The binary " ^ Ast.show_binop op
               ^ " operator's arguments must have the same type")))
  | Call (id, args) ->
      (* let _ = Symbol_table.print_keys gamma in *)
      let fun_t = st_lookup_rethrow id gamma expr.loc "Function not in scope" in
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
                  ("Wrong argument type when calling " ^ id)
          (* function with one argument, more arguments left *)
          | TFun (param_t, ret_t), arg :: xs ->
              if check_type_equality param_t (typecheck_expression gamma arg)
              then check_args ret_t xs
              else
                raise_semantic_error expr.loc
                  ("Wrong argument type when calling " ^ id)
          (* some arguments left but ft is no more a function, so it was a previous end-result *)
          | _ -> raise_semantic_error expr.loc ("Too many arguments for" ^ id)
        in
        check_args fun_t args
      else raise_semantic_error expr.loc (id ^ " is not a function")

and typecheck_access gamma access =
  match remove_node_annotations access with
  (* get type of variable in case of variable access *)
  | AccVar id -> st_lookup_rethrow id gamma access.loc "Variable not in scope"
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
let rec typecheck_statement gamma stmt expected_ret_type is_function_block =
  match remove_node_annotations stmt with
  | If (guard, then_stmt, else_stmt) ->
      if check_type_equality (typecheck_expression gamma guard) TBool then
        let does_then_ret =
          typecheck_statement gamma then_stmt expected_ret_type false
        in
        let does_else_ret =
          typecheck_statement gamma else_stmt expected_ret_type false
        in
        (* an if clause returns iff both branchess return *)
        does_then_ret && does_else_ret
      else raise_semantic_error guard.loc "The if guard must be a boolean"
  | While (guard, stmt) ->
      if check_type_equality (typecheck_expression gamma guard) TBool then
        let does_body_ret =
          typecheck_statement gamma stmt expected_ret_type false
        in
        (* even if the body of the while returns, the guard could be immediately false *)
        false && does_body_ret
      else raise_semantic_error guard.loc "The guard must be a boolean"
  | Expr expr ->
      let _ = typecheck_expression gamma expr in
      (* an expression by itself does not return *)
      false
  | Return oexpr -> (
      match oexpr with
      | Some expr ->
          let expr_t = typecheck_expression gamma expr in
          (* a return expression obviously returns *)
          if check_type_equality expr_t expected_ret_type then true
          else
            raise_semantic_error expr.loc
              ("Expected "
              ^ show_ttype expected_ret_type
              ^ " return type but found " ^ show_ttype expr_t)
      | None ->
          (* a return expression obviously returns *)
          if check_type_equality TVoid expected_ret_type then true
          else
            raise_semantic_error stmt.loc
              ("Expected "
              ^ show_ttype expected_ret_type
              ^ " return type but found void"))
  | Block stmt_list ->
      let block_gamma =
        if is_function_block then gamma else Symbol_table.begin_block gamma
      in
      let there_is_ret =
        List.fold_left
          (fun ret_until_here stmt ->
            if ret_until_here then
              (* the previous stmt has returned, so the current one and the next ones will never be executed *)
              raise_semantic_error stmt.loc "Dead code detected"
            else
              typecheck_statement_or_declaration block_gamma stmt
                expected_ret_type)
          false stmt_list
      in
      (* a block returns if the last statement returns, whatever it is *)
      there_is_ret

(* return unit instead of tvoid *)
and typecheck_statement_or_declaration gamma stmtordec expected_ret_type =
  match remove_node_annotations stmtordec with
  | Dec (typ, id) ->
      let t = check_variable_type (from_ast_type typ) stmtordec.loc in
      let _ = st_add_entry_rethrow id t gamma stmtordec.loc in
      (* a declaration does not return *)
      false
      (* a Stmt returns iff the inner stmt returns *)
  | Stmt stmt -> typecheck_statement gamma stmt expected_ret_type false

let typecheck_topdeclaration gamma topdecl =
  match remove_node_annotations topdecl with
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
                let t = check_parameter_type t topdecl.loc in
                let _ = st_add_entry_rethrow id t fun_gamma topdecl.loc in
                ())
              formals_t
          in
          (* add the function into the global scope *)
          let fun_t =
            List.fold_right
              (fun (param_t, _) acc -> TFun (param_t, acc))
              (if List.length formals_t == 0 then [ (TVoid, "") ]
              else formals_t)
              return_t
          in
          let _ = st_add_entry_rethrow fname fun_t gamma topdecl.loc in
          (* pass in the expected return type: all inners return statements should be compliant *)
          (* delayed computation to firstly add all the functions to the global scope *)
          fun () ->
            let _ = typecheck_statement fun_gamma body return_t true in
            ()
      | _ ->
          raise_semantic_error topdecl.loc
            (* currently the language itself does not accept functions that do not return  void, int, bool, char *)
            "A function can only return void, int, bool, char")
  | Vardec (typ, id) ->
      let t = check_variable_type (from_ast_type typ) topdecl.loc in
      let _ = st_add_entry_rethrow id t gamma topdecl.loc in
      fun () -> ()

let type_check p =
  match p with
  | Prog program ->
      let global = Symbol_table.begin_block Symbol_table.empty_table in
      let _ = Symbol_table.add_entry "getint" (TFun (TVoid, TInt)) global in
      let _ = Symbol_table.add_entry "print" (TFun (TInt, TVoid)) global in

      (* check and store functions' declarations in the global environment *)
      let delayed_bodies_check =
        List.map
          (fun topdecl -> typecheck_topdeclaration global topdecl)
          program
      in
      let _ =
        st_lookup_rethrow "main" global Location.dummy_code_pos
          "Missing main function"
      in
      (* check functions' bodies *)
      let _ = List.iter (fun d -> d () |> ignore) delayed_bodies_check in
      p
