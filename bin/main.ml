open Arith

let default_program = "let id = fun (x : int) -> x in id 42"

let parse_and_print source =
  match parse_expression source with
  | ast ->
      Printf.printf "Parsed expression: %s\n" (Ast.to_string ast)
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
  let samples =
    [
      ( "identity"
      , "let id = fun (x : int) -> x in id 42" );
      ( "conditional", "if true then 1 else 0" );
      ( "arithmetic", "10 + (2 * 5)" );
    ]
  in
  List.iter
    (fun (label, program) ->
      Printf.printf "Example (%s): %s\n" label program;
      match parse_expression program with
      | exception Parse_error msg ->
          Printf.printf "  Parse error: %s\n" msg
      | ast -> (
          match Typed.evaluate ast with
          | Ok (typ, value) ->
              Printf.printf "  Type: %s\n" (Ast.typ_to_string typ);
              Printf.printf "  Eval: %s\n"
                (Typed.string_of_eval_result value);
              (match Typed.pretty ast with
              | Ok (_, pretty) -> Printf.printf "  Pretty: %s\n" pretty
              | Error msg -> Printf.printf "  Pretty error: %s\n" msg)
          | Error msg -> Printf.printf "  Type error: %s\n" msg))
    samples

let () =
  let program =
    if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else
      default_program
  in
  parse_and_print program;
  print_endline "";
  show_typed_examples ()
