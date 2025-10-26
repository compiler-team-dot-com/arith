open Arith

let default_program = "let id = fun x -> x in id 42"

let parse_and_print source =
  match parse_expression source with
  | ast ->
      Printf.printf "Parsed expression: %s\n" (expr_to_string ast)
  | exception Parse_error msg ->
      prerr_endline ("Parse error: " ^ msg);
      exit 1

let show_typed_examples () =
  let module EvalExample = Typed.Example (Typed.Eval) in
  let module PrettyExample = Typed.Example (Typed.Pretty) in
  let module ReifyExample = Typed.Example (Typed.Reify) in
  Printf.printf "Typed program (eval): %d\n" EvalExample.program;
  Printf.printf "Typed program (pretty): %s\n" PrettyExample.program;
  Printf.printf "Typed program (AST): %s\n"
    (expr_to_string ReifyExample.program);
  Printf.printf "Typed conditional (eval): %d\n" EvalExample.conditional;
  Printf.printf "Typed arithmetic (pretty): %s\n" PrettyExample.arithmetic

let () =
  let program =
    if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else
      default_program
  in
  parse_and_print program;
  show_typed_examples ()
