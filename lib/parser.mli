val parse_expression : string -> (Ast.expr, string) result
(** Parse a source string into an AST. *)

val parse_file : string -> (Ast.expr, string) result
(** Parse the contents of a file into an AST. *)
