exception DuplicateEntry of Ast.identifier
exception NotFoundEntry of Ast.identifier

type identifier = Ast.identifier
type 'a table = SymbolTable of (identifier, 'a) Hashtbl.t List.t

let empty_table = SymbolTable []

let begin_block st =
  match st with SymbolTable l -> SymbolTable (List.cons (Hashtbl.create 1) l)

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
