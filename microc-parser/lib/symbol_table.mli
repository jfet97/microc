exception DuplicateEntry of Ast.identifier
exception NotFoundEntry of Ast.identifier

type 'a table 
val empty_table : 'a table 
val begin_block : 'a table -> 'a table 
val end_block : 'a table -> 'a table
val add_entry : Ast.identifier -> 'a -> 'a table -> 'a table 
val lookup : Ast.identifier -> 'a table -> 'a
val of_alist : (Ast.identifier * 'a) list -> 'a table 