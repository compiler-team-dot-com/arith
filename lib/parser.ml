exception Parse_error of string

let pp_position lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %d, column %d"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_expression source =
  let lexbuf = Lexing.from_string source in
  try Grammar.program Lexer.token lexbuf with
  | Lexer.Error msg -> raise (Parse_error msg)
  | Grammar.Error ->
      let loc = pp_position lexbuf in
      raise (Parse_error ("Parse error at " ^ loc))

let parse_file filename =
  let channel = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      let contents = really_input_string channel length in
      parse_expression contents)
