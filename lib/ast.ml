type binop = Add | Sub | Mul | Div | Eq | And | Or
type typ = TInt | TBool | TArrow of typ * typ

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Lambda of string * typ * expr
  | App of expr * expr
  | Let of string * typ option * expr * expr
  | If of expr * expr * expr
  | BinOp of binop * expr * expr

let rec typ_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (lhs, rhs) ->
      let lhs_str =
        match lhs with
        | TArrow _ -> "(" ^ typ_to_string lhs ^ ")"
        | _ -> typ_to_string lhs
      in
      lhs_str ^ " -> " ^ typ_to_string rhs

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
  | Lambda (arg, annot, body) ->
      "fun (" ^ arg ^ " : " ^ typ_to_string annot ^ ") -> " ^ to_string body
  | App (fn, arg) -> "(" ^ to_string fn ^ " " ^ to_string arg ^ ")"
  | Let (name, annot, value, body) ->
      let annot_str =
        match annot with None -> "" | Some t -> " : " ^ typ_to_string t
      in
      "let " ^ name ^ annot_str ^ " = " ^ to_string value ^ " in "
      ^ to_string body
  | If (cond, t_branch, f_branch) ->
      "if " ^ to_string cond ^ " then " ^ to_string t_branch ^ " else "
      ^ to_string f_branch
  | BinOp (op, lhs, rhs) ->
      "(" ^ to_string lhs ^ " " ^ binop_to_string op ^ " " ^ to_string rhs ^ ")"
