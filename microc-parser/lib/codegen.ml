open Ast
module L = Llvm

(* The LLVM global context *)
let mc_context = L.global_context ()

(* --------------- types --------------------- *)

(* LLVM versions of microc types *)
let int_ll = L.i32_type mc_context
let bool_ll = L.i1_type mc_context
let char_ll = L.i8_type mc_context
let void_ll = L.void_type mc_context

let rec from_ast_type = function
  | TypI -> int_ll
  | TypB -> bool_ll
  | TypC -> char_ll
  | TypA (typ, i) -> (
      let tp = from_ast_type typ in
      match i with None -> L.pointer_type tp | Some i -> L.array_type tp i)
  | TypP typ -> (
      let tp = from_ast_type typ in
      match L.classify_type tp with
      (* LLVM does not support void pointers *)
      | L.TypeKind.Void -> L.pointer_type int_ll
      | _ -> L.pointer_type tp)
  | TypV -> void_ll

(* --------------- generic utils --------------------- *)
let remove_node_annotation annotated_node =
  match annotated_node with { loc = _; node } -> node

let global_counter = ref 0

let next_global_counter_value () =
  global_counter := !global_counter + 1;
  string_of_int !global_counter

let reset_global_counter_value () = global_counter := 0

(* --------------- LLVM instructions builders --------------------- *)
let build_bop op ll1 ll2 =
  let prelude_bop =
    [
      (Add, L.build_add);
      (Mult, L.build_mul);
      (Sub, L.build_sub);
      (Div, L.build_sdiv);
      (Mod, L.build_srem);
      (Less, L.build_icmp L.Icmp.Slt);
      (Leq, L.build_icmp L.Icmp.Sle);
      (Greater, L.build_icmp L.Icmp.Sgt);
      (Geq, L.build_icmp L.Icmp.Sge);
      (Equal, L.build_icmp L.Icmp.Eq);
      (Neq, L.build_icmp L.Icmp.Ne);
      (And, L.build_and);
      (Or, L.build_or);
    ]
  in
  snd
    (List.find (fun o -> fst o == op) prelude_bop)
    ll1 ll2
    ("r" ^ next_global_counter_value ())

let build_uop op ll =
  let prelude_uop = [ (Neg, L.build_neg); (Not, L.build_not) ] in
  snd
    (List.find (fun o -> fst o == op) prelude_uop)
    ll
    ("r" ^ next_global_counter_value ())

let build_load var ibuilder =
  L.build_load var ("r" ^ next_global_counter_value ()) ibuilder

let build_append_block name fun_def_ll =
  L.append_block mc_context (name ^ next_global_counter_value ()) fun_def_ll

let build_store var value ibuilder =
  let value =
    if L.is_undef value then
      L.const_pointer_null (L.element_type (L.type_of var))
    else value
  in
  L.build_store value var ibuilder

let build_call f params ibuilder =
  let ret_typekind =
    (* f is a pointer to a function that return something *)
    L.classify_type (L.return_type (L.element_type (L.type_of f)))
  in
  match ret_typekind with
  | L.TypeKind.Void -> L.build_call f (Array.of_list params) "" ibuilder
  | _ ->
      L.build_call f (Array.of_list params)
        ("r" ^ next_global_counter_value ())
        ibuilder

let build_in_bounds_gep base_ll indexes ibuilder =
  L.build_in_bounds_gep base_ll indexes
    ("gep" ^ next_global_counter_value ())
    ibuilder

let build_alloca typ_ll id ibuilder = L.build_alloca typ_ll id ibuilder

(* --------------- LLVM utils --------------------- *)
(*
let debug_typekind typekind =
  let tks =
    [
      (L.TypeKind.Array, (1, "Array"));
      (L.TypeKind.BFloat, (2, "BFloat"));
      (L.TypeKind.Double, (3, "Double"));
      (L.TypeKind.Float, (4, "Float"));
      (L.TypeKind.Fp128, (5, "Fp128"));
      (L.TypeKind.Function, (6, "Function"));
      (L.TypeKind.Half, (7, "Half"));
      (L.TypeKind.Integer, (8, "Integer"));
      (L.TypeKind.Label, (9, "Label"));
      (L.TypeKind.Metadata, (10, "Metadata"));
      (L.TypeKind.Pointer, (11, "Pointer"));
      (L.TypeKind.Ppc_fp128, (12, "Ppc_fp128"));
      (L.TypeKind.ScalableVector, (13, "ScalableVector"));
      (L.TypeKind.Struct, (14, "Struct"));
      (L.TypeKind.Token, (15, "Token"));
      (L.TypeKind.Vector, (16, "Vector"));
      (L.TypeKind.Void, (17, "Void"));
      (L.TypeKind.X86_amx, (18, "X86_amx"));
      (L.TypeKind.X86_mmx, (19, "X86_mmx"));
      (L.TypeKind.X86fp80, (20, "X86fp80"));
    ]
  in
  List.assoc typekind tks
*)

(* given an address of something, return that something *)
let get_value_at_addr ibuilder addr =
  match L.classify_type (L.element_type (L.type_of addr)) with
  (* it's already a reference to a block of contiguous elements, there is no need to load anything *)
  | L.TypeKind.Array -> addr
  (* load from memory *)
  | _ -> build_load addr ibuilder

(* call 'add ibuilder' if the current block doesn't have a terminal *)
let add_terminal_to_block ibuilder add =
  match L.block_terminator (L.insertion_block ibuilder) with
  | Some _ -> ()
  | None -> add ibuilder |> ignore

let evaluate_const_expr expr typ_ll =
  let prelude_const_bop =
    [
      (Add, L.const_add);
      (Mult, L.const_mul);
      (Sub, L.const_sub);
      (Div, L.const_sdiv);
      (Mod, L.const_srem);
      (Less, L.const_icmp L.Icmp.Slt);
      (Leq, L.const_icmp L.Icmp.Sle);
      (Greater, L.const_icmp L.Icmp.Sgt);
      (Geq, L.const_icmp L.Icmp.Sge);
      (Equal, L.const_icmp L.Icmp.Eq);
      (Neq, L.const_icmp L.Icmp.Ne);
      (And, L.const_and);
      (Or, L.const_or);
    ]
  in
  let prelude_const_uop = [ (Not, L.const_not); (Neg, L.const_neg) ] in
  let rec aux expr =
    match remove_node_annotation expr with
    | ILiteral i -> L.const_int int_ll i
    | BLiteral b -> L.const_int bool_ll (if b then 1 else 0)
    | CLiteral c -> L.const_int char_ll (int_of_char c)
    | Null -> L.const_null typ_ll
    | UnaryOp (op, expr) -> List.assoc op prelude_const_uop (aux expr)
    | BinaryOp (bop, expr1, expr2) ->
        List.assoc bop prelude_const_bop (aux expr1) (aux expr2)
    | _ -> failwith "non-constant expression as initializer for global variable"
  in
  aux expr

(* --------------- code generation --------------------- *)

(* in general, should_ret_value indicates if we want the value or the address of something and
   it is true for read operations, false for write operations *)
let rec codegen_expression gamma ibuilder expr should_ret_value =
  match remove_node_annotation expr with
  | ILiteral i -> L.const_int int_ll i
  | BLiteral b -> L.const_int bool_ll (if b then 1 else 0)
  | CLiteral c -> L.const_int char_ll (int_of_char c)
  | Null -> L.undef void_ll
  | Access acc -> codegen_access gamma ibuilder acc should_ret_value
  (* &a -> i want the address of a *)
  | Addr acc -> codegen_access gamma ibuilder acc false
  | UnaryOp (uop, op) ->
      let value = codegen_expression gamma ibuilder op true in
      build_uop uop value ibuilder
  | BinaryOp (bop, op1, op2) ->
      let value1 = codegen_expression gamma ibuilder op1 true in
      let value2 = codegen_expression gamma ibuilder op2 true in
      build_bop bop value1 value2 ibuilder
  | Call (f, params) ->
      let f_ll = Symbol_table.lookup f gamma in
      let params_ll =
        List.map
          (fun param_ll ->
            let param = codegen_expression gamma ibuilder param_ll true in
            if
              L.classify_type (L.element_type (L.type_of param))
              == L.TypeKind.Array
            then
              L.build_bitcast param
                (* param is a pointer to an array, cast it into a pointer to the data STORED into the array *)
                (L.pointer_type
                   (L.element_type (L.element_type (L.type_of param))))
                (next_global_counter_value ())
                ibuilder
            else param)
          params
      in
      build_call f_ll params_ll ibuilder
  | Assign (acc, expr) ->
      (* i want the address of the access, the value of the expression *)
      let acc_ll = codegen_access gamma ibuilder acc false in
      let expr_ll = codegen_expression gamma ibuilder expr true in
      let _ = build_store acc_ll expr_ll ibuilder in
      expr_ll
  | Comma exprs ->
      List.fold_left
        (fun _ expr -> codegen_expression gamma ibuilder expr should_ret_value)
        (* L.undef void_ll is just a spurious initial llvalue because exprs is at least of length 2 *)
        (L.undef void_ll)
        exprs

and codegen_access gamma ibuilder acc should_ret_value =
  match remove_node_annotation acc with
  | AccVar id ->
      let addr = Symbol_table.lookup id gamma in
      (* are we interested on the value of the symbol id (to read from it) or in its address (to write into it)? *)
      if should_ret_value then get_value_at_addr ibuilder addr else addr
  | AccIndex (base, index) ->
      (*
        true/false does not matter if base is an array because they are specially handled by get_value_at_addr,
        it matters if base would be a pointer from a decayed array parameter and should be true
        because we want the pointer to the elements of the array, not the address in memory of the decayed pointer
      *)
      let base_ll = codegen_access gamma ibuilder base true in
      let index_ll = codegen_expression gamma ibuilder index true in
      (* base_ll could be a pointer to an array or just a pointer from a decayed array parameter *)
      let base_ll_t = L.element_type (L.type_of base_ll) in
      let addr =
        match L.classify_type base_ll_t with
        | L.TypeKind.Array ->
            build_in_bounds_gep base_ll
              [| L.const_int int_ll 0; index_ll |]
              ibuilder
        | _ ->
            (* if a parameter was declared as an array, it has just decayed into a pointer *)
            build_in_bounds_gep base_ll [| index_ll |] ibuilder
      in
      (* are we interested on the value of a[i] (to read from it) or in its address (to write into it)? *)
      if should_ret_value then get_value_at_addr ibuilder addr else addr
  | AccDeref expr ->
      let ptr = codegen_expression gamma ibuilder expr true in
      (* are we interested on the value of *p (to read from it) or in its address (to write into it)? *)
      if should_ret_value then get_value_at_addr ibuilder ptr else ptr

let rec codegen_stmt fun_def_ll gamma ibuilder stmt =
  match remove_node_annotation stmt with
  | Block stmt_list ->
      let block_gamma = Symbol_table.begin_block gamma in
      let _ =
        List.iter
          (fun stmtordec ->
            codegen_stmtordec fun_def_ll block_gamma ibuilder stmtordec
            |> ignore)
          stmt_list
      in
      ibuilder
  | Expr expr ->
      let _ = codegen_expression gamma ibuilder expr false in
      ibuilder
  | Return oexpr ->
      let _ =
        match oexpr with
        | None -> L.build_ret_void ibuilder
        | Some expr ->
            L.build_ret (codegen_expression gamma ibuilder expr true) ibuilder
      in
      ibuilder
  | If (cond, then_stmt, else_stmt) ->
      (* build the condition *)
      let cond_ll = codegen_expression gamma ibuilder cond true in
      (* create empty blocks for then, else and the last merge *)
      let then_block = build_append_block "then" fun_def_ll in
      let else_block = build_append_block "else" fun_def_ll in
      let merge_block = build_append_block "merge" fun_def_ll in
      let _ =
        (* generate code for the then statement, and insert it into the then block *)
        (* add br to the merge block if the then block does not return *)
        add_terminal_to_block
          (codegen_stmt fun_def_ll gamma
             (L.builder_at_end mc_context then_block)
             then_stmt)
          (L.build_br merge_block)
      in
      (* generate code for the else statement, and insert it into the else block *)
      (* add br to the merge block if the else block does not return *)
      let _ =
        add_terminal_to_block
          (codegen_stmt fun_def_ll gamma
             (L.builder_at_end mc_context else_block)
             else_stmt)
          (L.build_br merge_block)
      in
      (* handle the result of the test of the condition *)
      let _ = L.build_cond_br cond_ll then_block else_block ibuilder in
      (* move the ibuilder at the end of the merge block *)
      let _ = L.position_at_end merge_block ibuilder in
      ibuilder
  | While (cond, stmt) ->
      let condition_block = build_append_block "cond" fun_def_ll in
      let body_block = build_append_block "body" fun_def_ll in
      let merge_block = build_append_block "merge" fun_def_ll in

      (* set a jump to the condition *)
      let _ = L.build_br condition_block ibuilder in

      (* generate code for the body statement, and insert it into the body block *)
      (* add br to the condition block if the body block does not return *)
      let _ =
        add_terminal_to_block
          (codegen_stmt fun_def_ll gamma
             (L.builder_at_end mc_context body_block)
             stmt)
          (L.build_br condition_block)
      in

      (* build the expression and the condition inside the condition block *)
      let merge_block_builder = L.builder_at_end mc_context condition_block in
      (* test the condition *)
      let cond_ll = codegen_expression gamma merge_block_builder cond true in
      let _ =
        (* handle the result of the previous test by setting the jumps to do
           body of the while if false, out of the while if true *)
        L.build_cond_br cond_ll body_block merge_block merge_block_builder
      in

      (* move the ibuilder at the end of the while *)
      let _ = L.position_at_end merge_block ibuilder in
      ibuilder

and codegen_stmtordec fun_def_ll gamma ibuilder stmtordec =
  match remove_node_annotation stmtordec with
  | Dec inits ->
      let _ =
        List.iter
          (fun (typ, id, init_exprs) ->
            let typ_ll = from_ast_type typ in
            let var = build_alloca typ_ll id ibuilder in
            let _ =
              (* handle init list *)
              match init_exprs with
              (* empty init list *)
              | [] -> ()
              (* an init list with just one element *)
              | expr :: [] ->
                  let expr_ll = codegen_expression gamma ibuilder expr true in
                  let addr =
                    match typ with
                    | TypA _ ->
                        build_in_bounds_gep var
                          [| L.const_int int_ll 0; Llvm.const_int int_ll 0 |]
                          ibuilder
                    (* it could be 'T a = T_val' or 'T a = {T_val}' *)
                    | _ -> var
                  in
                  build_store addr expr_ll ibuilder |> ignore
              (* an init list with more than one element *)
              | exprs ->
                  let exprs_ll =
                    List.map
                      (fun expr -> codegen_expression gamma ibuilder expr true)
                      exprs
                  in
                  let _ =
                    (* build a store for each element *)
                    List.fold_left
                      (fun i expr_ll ->
                        let addr =
                          build_in_bounds_gep var
                            [| L.const_int int_ll 0; Llvm.const_int int_ll i |]
                            ibuilder
                        in
                        let _ = build_store addr expr_ll ibuilder in
                        i + 1)
                      0 exprs_ll
                  in
                  ()
            in
            let _ = Symbol_table.add_entry id var gamma in
            ())
          inits
      in
      ibuilder
  | Stmt stmt -> codegen_stmt fun_def_ll gamma ibuilder stmt

let codegen_fundecl gamma { typ; fname; formals; body } llmodule =
  (* LLVM return type *)
  let fun_ret_typ_ll = from_ast_type typ in
  (* LLVM formals' type *)
  let formals_typ_ll =
    List.map
      (fun (typ, id) ->
        match typ with
        | TypA (el_typ, _) -> (from_ast_type (TypP el_typ), id)
        | _ -> (from_ast_type typ, id))
      formals
  in
  (* LLVM function type *)
  let fun_typ_ll =
    L.function_type fun_ret_typ_ll (Array.of_list (List.map fst formals_typ_ll))
  in
  (* the LLVM function *)
  let fun_def_ll = L.define_function fname fun_typ_ll llmodule in
  (* add it to the global scope*)
  let _ = Symbol_table.add_entry fname fun_def_ll gamma in

  (* delay code generation of the body *)
  fun () ->
    (* instance of the instructions builder for this function *)
    let ibuilder = L.builder_at_end mc_context (L.entry_block fun_def_ll) in
    (* the function scope *)
    let fun_gamma = Symbol_table.begin_block gamma in
    let _ =
      (* insert parameters into it *)
      List.fold_left
        (fun i (typ_ll, id) ->
          let param_addr_ll = build_alloca typ_ll id ibuilder in
          let i_param_ll = L.param fun_def_ll i in
          let _ = build_store param_addr_ll i_param_ll ibuilder in
          let _ = Symbol_table.add_entry id param_addr_ll fun_gamma in
          i + 1)
        0 formals_typ_ll
    in
    (* reset the global counter to 0 to start the names for instructions builders from 0 inside the body *)
    let _ = reset_global_counter_value () in
    (* a return block could be missing *)
    add_terminal_to_block (codegen_stmt fun_def_ll fun_gamma ibuilder body)
      (fun ibuilder ->
        match typ with
        | TypV -> L.build_ret_void ibuilder
        | _ -> L.build_ret (L.undef fun_ret_typ_ll) ibuilder)

let codegen_topdecl global topdecl llmodule =
  match remove_node_annotation topdecl with
  | Fundecl f -> codegen_fundecl global f llmodule
  (* only costants and constants expression are supported for globals *)
  | Vardec inits ->
      let _ =
        List.iter
          (fun (typ, id, init_exprs) ->
            let typ_ll = from_ast_type typ in
            let llvalue =
              L.define_global id
                (* handle init list *)
                (match init_exprs with
                (* empty init list *)
                | [] -> L.const_null typ_ll
                (* an init list with just one element *)
                | expr :: [] -> (
                    match typ with
                    | TypA _ ->
                        let el_typ_ll = L.element_type typ_ll in
                        L.const_array el_typ_ll
                          [| evaluate_const_expr expr el_typ_ll |]
                    (* it could be 'T a = T_val' or 'T a = {T_val}' *)
                    | _ -> evaluate_const_expr expr typ_ll)
                | exprs ->
                    (* an init list with more than one element *)
                    let el_typ_ll = L.element_type typ_ll in
                    L.const_array el_typ_ll
                      (Array.of_list
                         (List.map
                            (fun expr -> evaluate_const_expr expr el_typ_ll)
                            exprs)))
                llmodule
            in
            let _ = Symbol_table.add_entry id llvalue global in
            ())
          inits
      in
      fun () -> ()

(* Declare a function prototype in the current module *)
let declare_function llvm_module global_scope fun_t name =
  let fun_decl = L.declare_function name fun_t llvm_module in
  Symbol_table.add_entry name fun_decl global_scope

(* Declare in the current module the print prototype *)
let print_ll llvm_module global_scope =
  let print_t = L.function_type void_ll [| int_ll |] in
  declare_function llvm_module global_scope print_t "print"

(* Declare in the current module the getint prototype *)
let getint_ll llvm_module global_scope =
  let getint_t = L.function_type int_ll [||] in
  declare_function llvm_module global_scope getint_t "getint"

let to_llvm_module p =
  match p with
  | Prog program ->
      let global = Symbol_table.begin_block Symbol_table.empty_table in
      let llmodule = L.create_module mc_context "mc" in
      let _ = print_ll llmodule global in
      let _ = getint_ll llmodule global in
      let deferred_bodies =
        List.map
          (fun topdecl -> codegen_topdecl global topdecl llmodule)
          program
      in
      let _ = List.iter (fun f -> f ()) deferred_bodies in
      llmodule
