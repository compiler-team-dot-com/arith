%{
open Ast
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token FUN LET IN IF THEN ELSE
%token ARROW
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%token EQUAL
%token AND OR
%token EOF

%left OR
%left AND
%left EQUAL
%left PLUS MINUS
%left TIMES DIV
%left APP

%start <Ast.expr> program

%%

program:
  | expr EOF { $1 }

expr:
  | FUN IDENT ARROW expr { Lambda ($2, $4) }
  | LET IDENT EQUAL expr IN expr { Let ($2, $4, $6) }
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | expr PLUS expr { BinOp (Add, $1, $3) }
  | expr MINUS expr { BinOp (Sub, $1, $3) }
  | expr TIMES expr { BinOp (Mul, $1, $3) }
  | expr DIV expr { BinOp (Div, $1, $3) }
  | expr EQUAL expr { BinOp (Eq, $1, $3) }
  | expr AND expr { BinOp (And, $1, $3) }
  | expr OR expr { BinOp (Or, $1, $3) }
  | expr atom %prec APP { App ($1, $2) }
  | atom { $1 }

atom:
  | INT { Int $1 }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | IDENT { Var $1 }
  | LPAREN expr RPAREN { $2 }
