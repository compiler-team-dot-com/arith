%{
open Ast
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token FUN LET IN IF THEN ELSE
%token ARROW
%token LPAREN RPAREN
%token COLON COMMA
%token PLUS MINUS TIMES DIV
%token EQUAL
%token AND OR
%token TYPE_INT TYPE_BOOL
%token FST SND
%token EOF

%right ARROW

%start <Ast.expr> program
%type <Ast.typ> typ
%type <Ast.expr> expr disj conj equality sum prod application atom

%%

program:
  | expr EOF { $1 }

expr:
  | FUN LPAREN IDENT COLON typ RPAREN ARROW expr { Lambda ($3, $5, $8) }
  | LET IDENT let_annotation EQUAL expr IN expr { Let ($2, $3, $5, $7) }
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | disj { $1 }

disj:
  | disj OR conj { BinOp (Or, $1, $3) }
  | conj { $1 }

conj:
  | conj AND equality { BinOp (And, $1, $3) }
  | equality { $1 }

equality:
  | sum EQUAL sum { BinOp (Eq, $1, $3) }
  | sum { $1 }

sum:
  | sum PLUS prod { BinOp (Add, $1, $3) }
  | sum MINUS prod { BinOp (Sub, $1, $3) }
  | prod { $1 }

prod:
  | prod TIMES application { BinOp (Mul, $1, $3) }
  | prod DIV application { BinOp (Div, $1, $3) }
  | application { $1 }

application:
  | application atom { App ($1, $2) }
  | atom { $1 }

let_annotation:
  | COLON typ { Some $2 }
  | /* empty */ { None }

atom:
  | INT { Int $1 }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | IDENT { Var $1 }
  | LPAREN expr COMMA expr RPAREN { Pair ($2, $4) }
  | LPAREN expr RPAREN { $2 }

typ:
  | TYPE_INT { TInt }
  | TYPE_BOOL { TBool }
  | typ TIMES typ { TPair ($1, $3) }
  | typ ARROW typ { TArrow ($1, $3) }
  | LPAREN typ RPAREN { $2 }
