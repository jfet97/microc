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

(* Translate a microc type into a LLVM IR one*)
let rec from_type = function
  | TypI -> int_ll
  | TypB -> bool_ll
  | TypC -> char_ll
  | TypA (typ, i) -> (
      let tp = from_type typ in
      match i with None -> L.pointer_type tp | Some i -> L.array_type tp i)
  | TypP typ ->
      let tp = from_type typ in
      L.pointer_type tp
  | TypV -> void_ll

(* A table mapping a binary operator in the LLVM function that implemets it and its name *)

let target_register_counter = ref 0

let next_target_label () =
  target_register_counter := !target_register_counter + 1;
  "r" ^ string_of_int !target_register_counter

let build_op op =
  let prelude =
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
  (List.find (fun o -> o == op) prelude, next_target_label ())

let build_load var ibuilder = L.build_load var (next_target_label ()) ibuilder

(* Codegen for expr node *)
let rec codegen_expr gamma ibuilder e =
  match remove_node_annotations e with
  | Access le -> (
      let var = codegen_le gamma ibuilder le in
      match L.classify_type (L.element_type (L.type_of var)) with
      (* it's a reference, no need to load it *)
      | Array -> var
      (* load from memory *)
      | _ -> build_load var ibuilder)
  | _ -> codegen_re gamma ibuilder e

and codegen_ae gamma ibuilder e =
  match remove_node_annotations e with
  | Addr a -> codegen_le gamma ibuilder a
  | ILiteral i -> L.const_int int_ll i
  | BLiteral b -> L.const_int bool_ll (if b then 1 else 0)
  | CLiteral c -> L.const_int char_ll (int_of_char c)
  | _ -> codegen_re gamma ibuilder e

and codegen_le gamma ibuilder e =
  match remove_node_annotations e with
  | AccVar id ->
      (* just return the address of id *) Symbol_table.lookup id gamma
  | AccDeref e -> (
      match remove_node_annotations e with
      (* *a loads the value of a that is an address *)
      | Access _ -> codegen_expr gamma ibuilder e
      | _ -> codegen_ae gamma ibuilder e)
  | AccIndex (le, e) -> (
      (* just return the address of le[e] *)
      let index = codegen_expr gamma ibuilder e in
      let base = codegen_le gamma ibuilder le in
      match L.classify_type (L.element_type (L.type_of base)) with
      | Array ->
          L.build_in_bounds_gep base
            [| Llvm.const_int int_ll 0; index |]
            (next_target_label ()) ibuilder
      | _ -> failwith "unreachable: indexing non array is not supported")

and codegen_re gamma ibuilder e =
  match remove_node_annotations e with
  | Assign (le, e) ->
      let le_ll = codegen_le gamma ibuilder le in
      let e_ll = codegen_expr gamma ibuilder e in
      let _ = L.build_store e_ll le_ll ibuilder in
      build_load le_ll ibuilder
  | _ -> codegen_ae gamma ibuilder e

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

let to_llvm_module _ = failwith "Not implemented yet"