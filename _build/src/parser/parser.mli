exception Error

type token = 
  | WHILE
  | USER_OP of (string)
  | TRUE
  | TOP
  | TILDE
  | SYMBOL of (string)
  | STRING_CONST of (string)
  | SEMI
  | RSUPER_ASSIGN
  | RPAREN
  | REPEAT
  | RBRAX
  | RBRACK
  | RBRACE
  | RASSIGN
  | QUESTION
  | PLUS
  | OUTER_PROD
  | OR2
  | OR
  | NULL
  | NS_GET_INT
  | NS_GET
  | NEXT
  | NEWLINE
  | NE
  | NAN
  | NA
  | MULT
  | MOD
  | MINUS
  | MATRIX_MULT
  | MATCH
  | LT
  | LSUPER_ASSIGN
  | LPAREN
  | LE
  | LBRAX
  | LBRACK
  | LBRACE
  | LASSIGN
  | KRON_PROD
  | INT_DIV
  | INT_CONST of (int)
  | INFINITY
  | IN
  | IF
  | GT
  | GE
  | FUNCTION
  | FOR
  | FLOAT_CONST of (float)
  | FALSE
  | EQ_ASSIGN
  | EQ
  | END_OF_INPUT
  | ELSE
  | DOLLAR
  | DIV
  | COMPLEX_CONST of (float)
  | COMMA
  | COLON
  | CARAT
  | BREAK
  | BANG
  | AT
  | AND2
  | AND


val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Rast.program)