type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | And
  | Or

type typ =
  | TInt
  | TBool
  | TArrow of typ * typ

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Lambda of string * typ * expr
  | App of expr * expr
  | Let of string * typ option * expr * expr
  | If of expr * expr * expr
  | BinOp of binop * expr * expr

val typ_to_string : typ -> string
val to_string : expr -> string
