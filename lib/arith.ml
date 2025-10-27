module Ast = Ast
module Typed = Typed

exception Parse_error of string

let add a b = a + b

let subtract a b = a - b

let pp_position lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %d, column %d"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_expression source =
  let lexbuf = Lexing.from_string source in
  try Parser.program Lexer.token lexbuf with
  | Lexer.Error msg -> raise (Parse_error msg)
  | Parser.Error ->
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

let expr_to_string = Ast.to_string

let string_of_eval_result (Typed.Eval_result (ty, value)) =
  let aux : type a. a Typed.ty -> a -> string =
   fun ty value ->
    match ty with
    | Typed.TInt -> string_of_int value
    | Typed.TBool -> string_of_bool value
    | Typed.TArrow _ -> "<fun>"
  in
  aux ty value
