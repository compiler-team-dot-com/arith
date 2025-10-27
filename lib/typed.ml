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

exception Type_error of string

type _ ty =
  | TInt : int ty
  | TBool : bool ty
  | TArrow : 'a ty * 'b ty -> ('a -> 'b) ty

type (_, _) eq = Refl : ('a, 'a) eq

let rec equal_ty : type a b. a ty -> b ty -> (a, b) eq option =
  fun lhs rhs ->
    match (lhs, rhs) with
    | TInt, TInt -> Some Refl
    | TBool, TBool -> Some Refl
    | TArrow (l1, r1), TArrow (l2, r2) -> (
        match equal_ty l1 l2 with
        | Some Refl -> (
            match equal_ty r1 r2 with
            | Some Refl -> Some Refl
            | None -> None)
        | None -> None)
    | _ -> None

type packed_ty = Pack_ty : 'a ty -> packed_ty

let rec typ_of_ty : type a. a ty -> Ast.typ = function
  | TInt -> Ast.TInt
  | TBool -> Ast.TBool
  | TArrow (l, r) -> Ast.TArrow (typ_of_ty l, typ_of_ty r)

let rec ty_of_typ = function
  | Ast.TInt -> Pack_ty TInt
  | Ast.TBool -> Pack_ty TBool
  | Ast.TArrow (lhs, rhs) ->
      let (Pack_ty lhs') = ty_of_typ lhs in
      let (Pack_ty rhs') = ty_of_typ rhs in
      Pack_ty (TArrow (lhs', rhs'))

let rec typ_equal lhs rhs =
  match (lhs, rhs) with
  | Ast.TInt, Ast.TInt -> true
  | Ast.TBool, Ast.TBool -> true
  | Ast.TArrow (l1, r1), Ast.TArrow (l2, r2) ->
      typ_equal l1 l2 && typ_equal r1 r2
  | _ -> false

(** Typed syntax tree produced by [Typecheck] before elaboration.
    Each node carries its inferred type alongside the original expression. *)
module Annotated = struct
  type t = { typ : Ast.typ; node : node }

  and node =
    | Int of int
    | Bool of bool
    | Var of string
    | Lambda of string * Ast.typ * t
    | App of t * t
    | Let of string * Ast.typ * t * t
    | If of t * t * t
    | BinOp of Ast.binop * t * t
end

module Typecheck : sig
  val check : Ast.expr -> Annotated.t
end = struct

  (** Typing environment mapping variables to their types. *)
  type type_env = (string * Ast.typ) list

  let lookup (env : type_env) name =
    match List.assoc_opt name env with
    | Some typ -> typ
    | None -> raise (Type_error (Printf.sprintf "Unbound variable %s" name))

  let ensure condition msg =
    if condition then () else raise (Type_error msg)

  let expect typ expected msg = ensure (typ_equal typ expected) msg

  let ensure_same lhs rhs msg = ensure (typ_equal lhs rhs) msg

  (** Recursively type-checks [expr] under [env], producing an [Annotated] node
      whose subtrees are already checked and paired with their inferred types. *)
  let rec annotate (env : type_env) expr : Annotated.t =
    match expr with
    | Ast.Int n -> { typ = Ast.TInt; node = Int n }
    | Ast.Bool b -> { typ = Ast.TBool; node = Bool b }
    | Ast.Var name ->
        let typ = lookup env name in
        { typ; node = Var name }
    | Ast.Lambda (name, param_typ, body) ->
        let env' = (name, param_typ) :: env in
        let body_ann = annotate env' body in
        {
          typ = Ast.TArrow (param_typ, body_ann.typ);
          node = Lambda (name, param_typ, body_ann);
        }
    | Ast.App (fn_expr, arg_expr) ->
        let fn_ann = annotate env fn_expr in
        let arg_ann = annotate env arg_expr in
        let result_typ =
          match fn_ann.typ with
          | Ast.TArrow (param_typ, ret_typ) ->
              expect arg_ann.typ param_typ
                (Printf.sprintf
                   "Function expected argument of type %s but got %s"
                   (Ast.typ_to_string param_typ)
                   (Ast.typ_to_string arg_ann.typ));
              ret_typ
          | _ ->
              raise
                (Type_error
                   (Printf.sprintf "Expected a function, got %s"
                      (Ast.typ_to_string fn_ann.typ)))
        in
        { typ = result_typ; node = App (fn_ann, arg_ann) }
    | Ast.Let (name, annotation, value_expr, body_expr) ->
        let value_ann = annotate (env : type_env) value_expr in
        let binding_typ =
          match annotation with
          | None -> value_ann.typ
          | Some annot ->
              expect value_ann.typ annot
                (Printf.sprintf
                   "Let-binding annotated as %s but value has type %s"
                   (Ast.typ_to_string annot)
                   (Ast.typ_to_string value_ann.typ));
              annot
        in
        let env' = (name, binding_typ) :: env in
        let body_ann = annotate env' body_expr in
        { typ = body_ann.typ; node = Let (name, binding_typ, value_ann, body_ann) }
    | Ast.If (cond_expr, t_branch, f_branch) ->
        let cond_ann = annotate (env : type_env) cond_expr in
        expect cond_ann.typ Ast.TBool "Condition of if must be bool";
        let t_ann = annotate env t_branch in
        let f_ann = annotate env f_branch in
        ensure_same t_ann.typ f_ann.typ "Branches of if must have same type";
        { typ = t_ann.typ; node = If (cond_ann, t_ann, f_ann) }
    | Ast.BinOp (op, lhs_expr, rhs_expr) ->
        let lhs_ann = annotate (env : type_env) lhs_expr in
        let rhs_ann = annotate env rhs_expr in
        begin
          match op with
          | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div ->
              expect lhs_ann.typ Ast.TInt "Arithmetic requires int operands";
              expect rhs_ann.typ Ast.TInt "Arithmetic requires int operands";
              { typ = Ast.TInt; node = BinOp (op, lhs_ann, rhs_ann) }
          | Ast.And | Ast.Or ->
              expect lhs_ann.typ Ast.TBool "Boolean op requires bool operands";
              expect rhs_ann.typ Ast.TBool "Boolean op requires bool operands";
              { typ = Ast.TBool; node = BinOp (op, lhs_ann, rhs_ann) }
          | Ast.Eq ->
              ensure_same lhs_ann.typ rhs_ann.typ
                "Equality operands must share a type";
              begin
                match lhs_ann.typ with
                | Ast.TInt | Ast.TBool ->
                    { typ = Ast.TBool; node = BinOp (op, lhs_ann, rhs_ann) }
                | _ ->
                    raise
                      (Type_error
                         "Equality is only supported for ints and booleans")
              end
        end

  let check expr = annotate [] expr
end

module Elaborate (S : Symantics) = struct
  (** Existential package pairing an inferred type witness with the
      interpreter representation produced for that type. *)
  type packed = Pack : 'a ty * 'a S.repr -> packed

  type env =
    | Empty
    | Bind : string * 'a ty * 'a S.repr * env -> env

  let rec lookup env name =
    match env with
    | Empty ->
        raise (Type_error (Printf.sprintf "Unbound variable %s" name))
    | Bind (n, ty, value, rest) ->
        if String.equal n name then Pack (ty, value) else lookup rest name

  let expect_ty expected actual msg =
    match equal_ty expected actual with
    | Some Refl -> ()
    | None -> raise (Type_error msg)

  (* [coerce expected actual value msg] reuses [value] when the type witnesses
     agree, otherwise raises a [Type_error] with [msg]. It is used to placate
     the OCaml type checker when unpacking existential witnesses. *)
  let coerce : type a b. a ty -> b ty -> b S.repr -> string -> a S.repr =
   fun expected actual repr msg ->
    match equal_ty expected actual with
    | Some Refl -> repr
    | None -> raise (Type_error msg)

  let rec eval env (expr : Annotated.t) : packed =
    match expr.node with
    | Int n -> Pack (TInt, S.int n)
    | Bool b -> Pack (TBool, S.bool b)
    | Var name ->
        let Pack (ty, value) = lookup env name in
        Pack (ty, value)
    | Lambda (name, param_typ, body) ->
        let (Pack_ty param_wit) = ty_of_typ param_typ in
        let (Pack_ty body_wit) = ty_of_typ body.typ in
        let lam_repr =
          S.lam (fun arg ->
              let env' = Bind (name, param_wit, arg, env) in
              let Pack (body_ty, body_repr) = eval env' body in
              coerce body_wit body_ty body_repr
                "Lambda body type changed after initial check")
        in
        Pack (TArrow (param_wit, body_wit), lam_repr)
    | App (fn_expr, arg_expr) ->
        let Pack (fn_ty, fn_repr) = eval env fn_expr in
        let Pack (arg_ty, arg_repr) = eval env arg_expr in
        begin
          match fn_ty with
          | TArrow (param_ty, result_ty) ->
              let coerced_arg =
                coerce param_ty arg_ty arg_repr "Function argument type mismatch"
              in
              Pack (result_ty, S.app fn_repr coerced_arg)
          | _ ->
              raise
                (Type_error
                   (Printf.sprintf "Expected a function, got %s"
                      (Ast.typ_to_string (typ_of_ty fn_ty))))
        end
    | Let (name, binding_typ, value_expr, body_expr) ->
        let (Pack_ty binding_wit) = ty_of_typ binding_typ in
        let Pack (value_ty, value_repr) = eval env value_expr in
        let coerced_value =
          coerce binding_wit value_ty value_repr "Let-binding type mismatch"
        in
        let env' = Bind (name, binding_wit, coerced_value, env) in
        eval env' body_expr
    | If (cond_expr, t_branch, f_branch) ->
        let Pack (cond_ty, cond_repr) = eval env cond_expr in
        let cond_val =
          coerce TBool cond_ty cond_repr "If condition must be bool"
        in
        let Pack (t_ty, t_repr) = eval env t_branch in
        let Pack (f_ty, f_repr) = eval env f_branch in
        begin
          match equal_ty t_ty f_ty with
          | Some Refl -> Pack (t_ty, S.if_ cond_val t_repr f_repr)
          | None -> raise (Type_error "If branches have different types")
        end
    | BinOp (op, lhs_expr, rhs_expr) ->
        let Pack (lhs_ty, lhs_repr) = eval env lhs_expr in
        let Pack (rhs_ty, rhs_repr) = eval env rhs_expr in
        begin
          match op with
          | Ast.Add ->
              let lhs_val =
                coerce TInt lhs_ty lhs_repr "Add expects integers"
              in
              let rhs_val =
                coerce TInt rhs_ty rhs_repr "Add expects integers"
              in
              Pack (TInt, S.add lhs_val rhs_val)
          | Ast.Sub ->
              let lhs_val =
                coerce TInt lhs_ty lhs_repr "Sub expects integers"
              in
              let rhs_val =
                coerce TInt rhs_ty rhs_repr "Sub expects integers"
              in
              Pack (TInt, S.sub lhs_val rhs_val)
          | Ast.Mul ->
              let lhs_val =
                coerce TInt lhs_ty lhs_repr "Mul expects integers"
              in
              let rhs_val =
                coerce TInt rhs_ty rhs_repr "Mul expects integers"
              in
              Pack (TInt, S.mul lhs_val rhs_val)
          | Ast.Div ->
              let lhs_val =
                coerce TInt lhs_ty lhs_repr "Div expects integers"
              in
              let rhs_val =
                coerce TInt rhs_ty rhs_repr "Div expects integers"
              in
              Pack (TInt, S.div lhs_val rhs_val)
          | Ast.And ->
              let lhs_val =
                coerce TBool lhs_ty lhs_repr "And expects booleans"
              in
              let rhs_val =
                coerce TBool rhs_ty rhs_repr "And expects booleans"
              in
              Pack (TBool, S.and_ lhs_val rhs_val)
          | Ast.Or ->
              let lhs_val =
                coerce TBool lhs_ty lhs_repr "Or expects booleans"
              in
              let rhs_val =
                coerce TBool rhs_ty rhs_repr "Or expects booleans"
              in
              Pack (TBool, S.or_ lhs_val rhs_val)
          | Ast.Eq ->
              begin
                match lhs_ty with
                | TInt ->
                    let lhs_val =
                      coerce TInt lhs_ty lhs_repr
                        "Equality expects integer operands"
                    in
                    let rhs_val =
                      coerce TInt rhs_ty rhs_repr
                        "Equality expects integer operands"
                    in
                    Pack (TBool, S.eq lhs_val rhs_val)
                | TBool ->
                    let lhs_val =
                      coerce TBool lhs_ty lhs_repr
                        "Equality expects boolean operands"
                    in
                    let rhs_val =
                      coerce TBool rhs_ty rhs_repr
                        "Equality expects boolean operands"
                    in
                    Pack (TBool, S.eq lhs_val rhs_val)
                | _ ->
                    raise
                      (Type_error
                         "Equality is only supported for ints and booleans")
              end
        end

  (** Type-checks [expr] and produces both its inferred type and the
      corresponding representation for the interpreter [S]. *)
  let run expr =
    let annotated = Typecheck.check expr in
    (annotated.typ, eval Empty annotated)
end

type eval_result = Eval_result : 'a ty * 'a -> eval_result

(** Type-checks and evaluates an expression using the [Eval] interpreter.
    Returns the inferred type and runtime value when successful. *)
let evaluate expr =
  let module E = Elaborate (Eval) in
  try
    let typ, packed = E.run expr in
    match packed with
    | E.Pack (ty, value) -> Ok (typ, Eval_result (ty, value))
  with Type_error msg -> Error msg

(** Type-checks and pretty-prints an expression using the [Pretty] interpreter.
    Returns the inferred type and human-readable form when successful. *)
let pretty expr =
  let module P = Elaborate (Pretty) in
  try
    let typ, packed = P.run expr in
    match packed with
    | P.Pack (_, repr) -> Ok (typ, repr)
  with Type_error msg -> Error msg

(** Runs only the type checker, returning the annotated AST when successful. *)
let type_check expr =
  try Ok (Typecheck.check expr) with Type_error msg -> Error msg
