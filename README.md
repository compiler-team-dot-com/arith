# Arith

## Theory

Arith implements a simply typed lambda calculus with integers, booleans, conditionals, let-bindings, and first-class functions. Programs are written with explicit type annotations on lambda parameters (and optional annotations on let-bindings) to keep typing rules straightforward. Parsing is handled with ocamllex and Menhir.

The interpreter relies on the tagless-final style: instead of materialising a typed syntax tree, we define a polymorphic interface (`Symantics`) whose operations mirror the typing rules. Concrete interpreters—such as an evaluator or a pretty-printer—instantiate this interface by choosing a representation for `'a repr`. The host language (OCaml) then ensures that any expression constructed through the interface is well typed; ill-typed programs cannot be expressed.

To connect parsed syntax to the tagless interpreters we perform an explicit type checking/elaboration step. First, the parser produces an untyped AST (`Ast.expr`). Next, the type checker walks that tree, enforcing the simply typed rules and producing an annotated tree (`Typed.Annotated.t`) whose nodes carry their inferred types. Finally, an elaborator consumes the annotated tree and, for any chosen `Symantics`, produces the corresponding tagless-final representation. This bridge is where we use GADTs to track type witnesses: the checker returns an `eval_result` or pretty-printed string only when the expression passes type checking, and the witness travels with the value so downstream interpreters know which representation to produce.

## Implementation

The project is organised into a few modules:

- `lib/ast.ml` defines the untyped syntax tree and helpers for pretty-printing expressions.
- `lib/lexer.mll` and `lib/parser.mly` implement the tokenizer and Menhir grammar. Lambda parameters must be annotated (`fun (x : int) -> ...`), and let bindings may optionally carry annotations. The grammar enforces usual precedence for application and arithmetic operators.
- `lib/typed.ml` houses the tagless-final infrastructure. It declares the `Symantics` interface, concrete interpreters (`Eval`, `Pretty`), the type witness GADT, the `Annotated` tree produced by the checker, the type checker/elaborator, and public helpers (`evaluate`, `pretty`, `string_of_eval_result`). An `.mli` file exports only what callers need.
- `lib/arith.ml` exposes parsing utilities (`parse_expression`, `parse_file`, `expr_to_string`) and reexports `Typed` to applications.
- `bin/main.ml` provides a CLI that parses input, displays the AST, runs the typed evaluator, and shows a few sample programs.
- `test/parser_driver.ml` reads `.lambda` fixtures from `test/programs/` and ensures they parse, type-check, and evaluate; this is wired into `dune build @runtest`.

The key flow is: source text → lexer/parser → untyped AST → type checker → annotated tree → elaborator → tagless representation → interpreter (evaluation or pretty printing).

## Usage

### CLI

```
$ dune exec arith_cli "let id = fun (x : int) -> x in id 42"
```
The CLI prints the parsed AST, the inferred type and value, and a typed pretty-print. With no argument it uses a default sample and showcases several built-in examples.

### Library

From OCaml code you can parse and evaluate expressions programmatically:

```ocaml
let expr = Arith.parse_expression "let id = fun (x : int) -> x in id 42" in
match Arith.Typed.evaluate expr with
| Ok (typ, value) ->
    Printf.printf "Type: %s\n" (Arith.Ast.typ_to_string typ);
    Printf.printf "Value: %s\n" (Arith.Typed.string_of_eval_result value)
| Error msg -> Printf.eprintf "Type error: %s\n" msg
```
You can also call `Arith.Typed.pretty` to obtain the type-directed pretty-print of a checked program.

### Tests

Run `dune build @runtest` to parse/type-check/evaluate the fixture programs under `test/programs/`.
