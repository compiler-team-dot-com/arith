module type Symantics = sig
  type 'a repr

  val int : int -> int repr
  val bool : bool -> bool repr

  val add : int repr -> int repr -> int repr
  val sub : int repr -> int repr -> int repr
  val mul : int repr -> int repr -> int repr
  val div : int repr -> int repr -> int repr

  val eq : 'a repr -> 'a repr -> bool repr
  val and_ : bool repr -> bool repr -> bool repr
  val or_ : bool repr -> bool repr -> bool repr

  val if_ : bool repr -> 'a repr -> 'a repr -> 'a repr

  val lam : ('a repr -> 'b repr) -> ('a -> 'b) repr
  val app : ('a -> 'b) repr -> 'a repr -> 'b repr

  val let_ : 'a repr -> ('a repr -> 'b repr) -> 'b repr
end

module Eval : Symantics with type 'a repr = 'a = struct
  type 'a repr = 'a

  let int x = x
  let bool b = b

  let add = ( + )
  let sub = ( - )
  let mul = ( * )
  let div = ( / )

  let eq x y = x = y
  let and_ x y = x && y
  let or_ x y = x || y

  let if_ c t e = if c then t else e

  let lam f = f
  let app f x = f x

  let let_ x f = f x
end

module Pretty : Symantics with type 'a repr = string = struct
  type 'a repr = string

  let fresh =
    let counter = ref 0 in
    fun prefix ->
      let id = !counter in
      incr counter;
      prefix ^ string_of_int id

  let int x = string_of_int x
  let bool b = string_of_bool b

  let binary op lhs rhs = "(" ^ lhs ^ " " ^ op ^ " " ^ rhs ^ ")"

  let add = binary "+"
  let sub = binary "-"
  let mul = binary "*"
  let div = binary "/"

  let eq = binary "="
  let and_ = binary "&&"
  let or_ = binary "||"

  let if_ c t e = "(if " ^ c ^ " then " ^ t ^ " else " ^ e ^ ")"

  let lam body =
    let v = fresh "x" in
    "(fun " ^ v ^ " -> " ^ body v ^ ")"

  let app f x = "(" ^ f ^ " " ^ x ^ ")"

  let let_ value body =
    let v = fresh "v" in
    "(let " ^ v ^ " = " ^ value ^ " in " ^ body v ^ ")"
end

module Reify : Symantics with type 'a repr = Ast.expr = struct
  type 'a repr = Ast.expr

  let fresh =
    let counter = ref 0 in
    fun prefix ->
      let id = !counter in
      incr counter;
      prefix ^ string_of_int id

  let int x = Ast.Int x
  let bool b = Ast.Bool b

  let add l r = Ast.BinOp (Ast.Add, l, r)
  let sub l r = Ast.BinOp (Ast.Sub, l, r)
  let mul l r = Ast.BinOp (Ast.Mul, l, r)
  let div l r = Ast.BinOp (Ast.Div, l, r)

  let eq l r = Ast.BinOp (Ast.Eq, l, r)
  let and_ l r = Ast.BinOp (Ast.And, l, r)
  let or_ l r = Ast.BinOp (Ast.Or, l, r)

  let if_ c t e = Ast.If (c, t, e)

  let lam body =
    let v = fresh "x" in
    let body_expr = body (Ast.Var v) in
    Ast.Lambda (v, body_expr)

  let app f x = Ast.App (f, x)

  let let_ value body =
    let v = fresh "v" in
    let body_expr = body (Ast.Var v) in
    Ast.Let (v, value, body_expr)
end

module Example (S : Symantics) = struct
  open S

  let identity = lam (fun x -> x)

  let program =
    let_ identity (fun id ->
        let_ (int 42) (fun answer -> app id answer))

  let conditional =
    if_ (bool true) (int 1) (int 0)

  let arithmetic =
    add (int 10) (mul (int 2) (int 5))
end
