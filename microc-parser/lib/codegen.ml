open Ast
open Symbol_table
module L = Llvm

let remove_node_annotations annotated_node =
  match annotated_node with { loc; node } -> node

(* The LLVM global context *)
let llcontext = L.global_context ()

(* Some useful LLVM IR type to use in the code generation *)
let int_ll = L.i32_type llcontext
let bool_ll = L.i1_type llcontext
let char_ll = L.i8_type llcontext
let void_ll = L.void_type llcontext

(* Translate into a LLVM IR one*)
let rec from_ast_type = function
  | TypI -> int_ll
  | TypB -> bool_ll
  | TypC -> char_ll
  | TypA (typ, i) -> (
      let tp = from_ast_type typ in
      match i with None -> L.pointer_type tp | Some i -> L.array_type tp i)
  | TypP typ ->
      let tp = from_ast_type typ in
      L.pointer_type tp
  | TypV -> void_ll

(* A table mapping a binary operator in the LLVM function that implemets it and its name *)

let target_register_counter = ref 0

let next_target_label () =
  target_register_counter := !target_register_counter + 1;
  "r" ^ string_of_int !target_register_counter

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
    ll1 ll2 (next_target_label ())

let build_uop op ll =
  let prelude_uop = [ (Neg, L.build_neg); (Not, L.build_not) ] in
  snd (List.find (fun o -> fst o == op) prelude_uop) ll (next_target_label ())

let build_load var ibuilder = L.build_load var (next_target_label ()) ibuilder
let build_store var value ibuilder = L.build_store value var ibuilder

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

let build_call f params ibuilder =
  let ret_typekind =
    (* f is a pointer to a function that return something *)
    L.classify_type (L.return_type (L.return_type (L.type_of f)))
  in
  (* let _ =
       Printf.printf "\nreturn kind %s\n" (snd (debug_typekind ret_typekind))
     in *)
  match ret_typekind with
  | L.TypeKind.Void -> L.build_call f (Array.of_list params) "" ibuilder
  | _ -> L.build_call f (Array.of_list params) (next_target_label ()) ibuilder

let get_value_at_addr ibuilder addr =
  match L.classify_type (L.element_type (L.type_of addr)) with
  (* it's a reference, no need to load it *)
  | L.TypeKind.Array -> addr
  (* load from memory *)
  | _ -> build_load addr ibuilder

let to_null_if_llvalue_undef v =
  if L.is_undef v then L.const_pointer_null (L.pointer_type (L.type_of v))
  else v

(* call f ibuilder if the current block doesn't have a terminal *)
let add_terminal ibuilder add =
  match L.block_terminator (L.insertion_block ibuilder) with
  | Some _ -> ()
  | None -> add ibuilder |> ignore

let evaluate_const_expr expr =
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
    match remove_node_annotations expr with
    | ILiteral i -> L.const_int int_ll i
    | BLiteral b -> L.const_int bool_ll (if b then 1 else 0)
    | CLiteral c -> L.const_int char_ll (int_of_char c)
    | UnaryOp (op, expr) -> List.assoc op prelude_const_uop (aux expr)
    | BinaryOp (bop, expr1, expr2) ->
        List.assoc bop prelude_const_bop (aux expr1) (aux expr2)
    | _ -> failwith "non-constant expression as initializer for global variable"
  in
  aux expr

(* in general should_ret_value = true for res (read operations), false for les (write operations ) *)
let rec codegen_expression gamma ibuilder expr should_ret_value =
  match remove_node_annotations expr with
  | ILiteral i -> L.const_int int_ll i
  | BLiteral b -> L.const_int bool_ll (if b then 1 else 0)
  | CLiteral c -> L.const_int char_ll (int_of_char c)
  | Null -> L.const_pointer_null (L.pointer_type void_ll)
  | Access acc -> codegen_access gamma ibuilder acc should_ret_value
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
          (fun param -> codegen_expression gamma ibuilder param true)
          params
      in
      build_call f_ll params_ll ibuilder
  | Assign (acc, expr) ->
      let acc_ll = codegen_access gamma ibuilder acc false
      and expr_ll = codegen_expression gamma ibuilder expr true in
      build_store acc_ll expr_ll ibuilder

and codegen_access gamma ibuilder acc should_ret_value =
  match remove_node_annotations acc with
  | AccVar id ->
      let addr = Symbol_table.lookup id gamma in
      if should_ret_value then get_value_at_addr ibuilder addr else addr
  | AccIndex (base, index) ->
      let base_ll = codegen_access gamma ibuilder base true in
      let index_ll = codegen_expression gamma ibuilder index true in
      let addr =
        L.build_in_bounds_gep base_ll
          [| L.const_int int_ll 0; index_ll |]
          (next_target_label ()) ibuilder
      in
      if should_ret_value then get_value_at_addr ibuilder addr else addr
  | AccDeref expr ->
      let ptr = codegen_expression gamma ibuilder expr true in
      (* the value of the pointer is the address of another var *)
      if should_ret_value then get_value_at_addr ibuilder ptr else ptr

(* Declare in the current module the print prototype *)
let print_ll llvm_module global_scope =
  let print_t = L.function_type void_ll [| int_ll |] in
  let decl = L.declare_function "print" print_t llvm_module in
  Symbol_table.add_entry "print" decl global_scope

(* Declare in the current module the getint prototype *)
let getint_ll llvm_module global_scope =
  let getint_t = L.function_type int_ll [||] in
  let decl = L.declare_function "getint" getint_t llvm_module in
  Symbol_table.add_entry "getint" decl global_scope

let rec codegen_stmt fun_def_ll gamma ibuilder stmt =
  match remove_node_annotations stmt with
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
  | _ -> failwith "to implement"

and codegen_stmtordec fun_def_ll gamma ibuilder stmtordec =
  match remove_node_annotations stmtordec with
  | Dec inits ->
      let _ =
        List.iter
          (fun (typ, id, init_exprs) ->
            let typ_ll = from_ast_type typ in
            let var = L.build_alloca typ_ll id ibuilder in
            let _ =
              match init_exprs with
              | [] -> ( match typ with TypA _ -> () | _ -> ())
              | expr :: [] ->
                  let expr_ll = codegen_expression gamma ibuilder expr true in
                  let addr =
                    match typ with
                    | TypA _ ->
                        L.build_in_bounds_gep var
                          [| L.const_int int_ll 0; Llvm.const_int int_ll 0 |]
                          (next_target_label ()) ibuilder
                    | _ -> var
                  in
                  build_store addr expr_ll ibuilder |> ignore
              | exprs ->
                  let exprs_ll =
                    List.map
                      (fun expr -> codegen_expression gamma ibuilder expr true)
                      exprs
                  in
                  let _ =
                    List.fold_left
                      (fun i expr_ll ->
                        let addr =
                          L.build_in_bounds_gep var
                            [| L.const_int int_ll 0; Llvm.const_int int_ll i |]
                            (next_target_label ()) ibuilder
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
  let fun_ret_typ_ll = from_ast_type typ in
  let formals_typ_ll =
    List.map (fun (typ, id) -> (from_ast_type typ, id)) formals
  in
  let fun_typ_ll =
    L.function_type fun_ret_typ_ll (Array.of_list (List.map fst formals_typ_ll))
  in
  let fun_def_ll = L.define_function fname fun_typ_ll llmodule in
  let _ = Symbol_table.add_entry fname fun_def_ll gamma in
  fun () ->
    let ibuilder = L.builder_at_end llcontext (L.entry_block fun_def_ll) in
    let fun_gamma = Symbol_table.begin_block gamma in
    let _ =
      List.fold_left
        (fun i (typ_ll, id) ->
          let param_addr_ll = L.build_alloca typ_ll id ibuilder in
          let i_param_ll = L.param fun_def_ll i in
          let _ = build_store param_addr_ll i_param_ll ibuilder in
          let _ = Symbol_table.add_entry id param_addr_ll fun_gamma in
          i + 1)
        0 formals_typ_ll
    in
    let _ = target_register_counter := 0 in
    let _ =
      add_terminal (codegen_stmt fun_def_ll fun_gamma ibuilder body) (fun ib ->
          match typ with
          | TypV -> L.build_ret_void ib
          | t -> L.build_ret (L.undef fun_ret_typ_ll) ib)
    in
    ()

let codegen_topdecl global topdecl llmodule =
  match remove_node_annotations topdecl with
  | Fundecl f -> codegen_fundecl global f llmodule
  | Vardec inits ->
      let _ =
        List.iter
          (fun (typ, id, init_exprs) ->
            let typ_ll = from_ast_type typ in
            let llvalue =
              L.define_global id
                (match init_exprs with
                | [] -> L.const_null typ_ll
                | expr :: [] -> (
                    match typ with
                    | TypA _ ->
                        L.const_array (L.element_type typ_ll)
                          [| evaluate_const_expr expr |]
                    | _ -> evaluate_const_expr expr)
                | exprs ->
                    L.const_array (L.element_type typ_ll)
                      (Array.of_list (List.map evaluate_const_expr exprs)))
                llmodule
            in
            let _ = Symbol_table.add_entry id llvalue global in
            ())
          inits
      in
      fun () -> ()

let to_llvm_module p =
  match p with
  | Prog program ->
      let global = Symbol_table.begin_block Symbol_table.empty_table in
      let llmodule = L.create_module llcontext "mc" in
      let _ = print_ll llmodule global in
      let _ = getint_ll llmodule global in
      let deferred_bodies =
        List.map
          (fun topdecl -> codegen_topdecl global topdecl llmodule)
          program
      in
      let _ = List.iter (fun f -> f ()) deferred_bodies in
      llmodule
