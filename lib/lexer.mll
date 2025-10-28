{
 open Grammar

 exception Error of string

 let keyword = function
   | "fun" -> FUN
   | "let" -> LET
   | "in" -> IN
   | "if" -> IF
   | "then" -> THEN
   | "else" -> ELSE
   | "true" -> TRUE
   | "false" -> FALSE
   | "int" -> TYPE_INT
   | "bool" -> TYPE_BOOL
   | ident -> IDENT ident
}

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let ident_start = lowercase | uppercase | '_'
let ident_char = ident_start | digit | '\''

rule token =
  parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | "(*" { comment lexbuf; token lexbuf }
  | "->" { ARROW }
  | "&&" { AND }
  | "||" { OR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ':' { COLON }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '=' { EQUAL }
  | digit+ as literal { INT (int_of_string literal) }
  | ident_start ident_char* as id { keyword id }
  | eof { EOF }
  | _ as ch {
      let msg = Printf.sprintf "Unexpected character: %c" ch in
      raise (Error msg)
    }

and comment =
  parse
  | "*)" { () }
  | eof { raise (Error "Unterminated comment") }
  | _ { comment lexbuf }
