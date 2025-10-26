open Arith

let run_file path =
  match parse_file path with
  | expr -> Printf.printf "Parsed %s => %s\n" path (expr_to_string expr)
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
