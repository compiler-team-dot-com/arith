type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | And
  | Or

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Lambda of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | BinOp of binop * expr * expr

let binop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "="
  | And -> "&&"
  | Or -> "||"

let rec to_string = function
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Var v -> v
  | Lambda (arg, body) -> "fun " ^ arg ^ " -> " ^ to_string body
  | App (fn, arg) -> "(" ^ to_string fn ^ " " ^ to_string arg ^ ")"
  | Let (name, value, body) ->
      "let " ^ name ^ " = " ^ to_string value ^ " in " ^ to_string body
  | If (cond, t_branch, f_branch) ->
      "if " ^ to_string cond ^ " then " ^ to_string t_branch ^ " else "
      ^ to_string f_branch
  | BinOp (op, lhs, rhs) ->
      "(" ^ to_string lhs ^ " " ^ binop_to_string op ^ " " ^ to_string rhs
      ^ ")"
