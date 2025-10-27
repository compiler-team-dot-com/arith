open Arith

let run_file path =
  match parse_file path with
  | expr ->
      Printf.printf "Parsed %s => %s\n" path (expr_to_string expr);
      (match Typed.evaluate expr with
      | Ok (typ, value) ->
          Printf.printf "  Type: %s\n" (Ast.typ_to_string typ);
          Printf.printf "  Eval: %s\n" (Typed.string_of_eval_result value)
      | Error msg -> Printf.printf "  Type error: %s\n" msg)
  | exception Parse_error msg ->
      prerr_endline (Printf.sprintf "Error in %s: %s" path msg);
      exit 1

let () =
  if Array.length Sys.argv <= 1 then (
    prerr_endline "Usage: parser_driver <files>";
    exit 2);
  for idx = 1 to Array.length Sys.argv - 1 do
    run_file Sys.argv.(idx)
  done
