open Arith

let default_program = "let id = fun x -> x in id 42"

let parse_and_print source =
  match parse_expression source with
  | ast ->
      Printf.printf "Parsed expression: %s\n" (expr_to_string ast)
  | exception Parse_error msg ->
      prerr_endline ("Parse error: " ^ msg);
      exit 1

let () =
  let program =
    if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else
      default_program
  in
  parse_and_print program
