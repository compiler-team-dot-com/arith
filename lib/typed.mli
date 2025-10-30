exception Type_error of string
(** Raised when type checking fails. *)

type eval_result
(** Opaque handle to a runtime value paired with its inferred type. *)

val string_of_eval_result : eval_result -> string
(** Render an evaluation result to a human-readable form. *)

val evaluate : Ast.expr -> (Ast.typ * eval_result, string) result
(** Type-check and evaluate an expression, returning its type and value. *)

val pretty : Ast.expr -> (Ast.typ * string, string) result
(** Type-check and pretty-print an expression, returning its type and surface
    form. *)
