open Arith

let default_program = "let id = fun (x : int) -> x in id 42"

let parse_and_print source =
  match parse_expression source with
  | ast ->
      Printf.printf "Parsed expression: %s\n" (expr_to_string ast)
      ;
      (match Typed.evaluate ast with
      | Ok (typ, value) ->
          Printf.printf "Type: %s\n" (Ast.typ_to_string typ);
          Printf.printf "Eval: %s\n" (Typed.string_of_eval_result value);
          (match Typed.pretty ast with
          | Ok (_, pretty) ->
              Printf.printf "Pretty (typed): %s\n" pretty
          | Error msg ->
              Printf.printf "Pretty error: %s\n" msg)
      | Error msg -> Printf.printf "Type error: %s\n" msg)
  | exception Parse_error msg ->
      prerr_endline ("Parse error: " ^ msg);
      exit 1

let show_typed_examples () =
  let module EvalExample = Typed.Example (Typed.Eval) in
  let module PrettyExample = Typed.Example (Typed.Pretty) in
  Printf.printf "Typed program (eval): %d\n" EvalExample.program;
  Printf.printf "Typed program (pretty): %s\n" PrettyExample.program;
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
