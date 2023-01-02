exception DuplicateEntry of Ast.identifier
exception NotFoundEntry of Ast.identifier

type identifier = Ast.identifier
type 'a table = SymbolTable of (identifier, 'a) Hashtbl.t List.t

let empty_table = SymbolTable []

let begin_block st =
  match st with SymbolTable l -> SymbolTable (List.cons (Hashtbl.create 1) l)

(* useless because of immutability *)
let end_block st = match st with SymbolTable l -> SymbolTable (List.tl l)

let add_entry i t st =
  match st with
  | SymbolTable l -> (
      match Hashtbl.find_opt (List.hd l) i with
      | Some _ -> raise (DuplicateEntry i)
      | None ->
          let current_scope = List.hd l in
          let _ = Hashtbl.add current_scope i t in
          st)

let lookup i st =
  let rec aux l i =
    match l with
    | [] -> raise (NotFoundEntry i)
    | ht :: tail -> (
        match Hashtbl.find_opt ht i with Some t -> t | None -> aux tail i)
  in
  match st with SymbolTable l -> aux l i

let of_alist l =
  SymbolTable
    [
      List.fold_left
        (fun ht (i, t) ->
          let _ = Hashtbl.add ht i t in
          ht)
        (Hashtbl.create (List.length l))
        l;
    ]

let print_keys st =
  let _ = Printf.printf "\nSTART PRINTING ST:" in
  let aux ht = Hashtbl.iter (fun id _ -> Printf.printf "%s " id) ht in
  match st with
  | SymbolTable l ->
      let _ =
        List.iter
          (fun ht ->
            aux ht;
            Printf.printf "\n")
          l
      in
      let _ = Printf.printf "END PRINTING ST\n" in
      ()
