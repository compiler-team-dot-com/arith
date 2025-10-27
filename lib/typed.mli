(** Raised when type checking fails. *)
exception Type_error of string

(** Opaque handle to a runtime value paired with its inferred type. *)
type eval_result

(** Render an evaluation result to a human-readable form. *)
val string_of_eval_result : eval_result -> string

(** Type-check and evaluate an expression, returning its type and value. *)
val evaluate :
  Ast.expr ->
  (Ast.typ * eval_result, string) result

(** Type-check and pretty-print an expression, returning its type and surface form. *)
val pretty :
  Ast.expr ->
  (Ast.typ * string, string) result
