exception Error

type token = 
  | UNSAT
  | UNDERSCORE
  | SYMBOL of (string)
  | STRING_CONST of (string)
  | SORT_INT
  | SORT_FLOAT
  | SORT_BOOL
  | SORT_BITVEC
  | SORT_ARRAY
  | SET_OPTION
  | SET_LOGIC
  | SET_INFO
  | SAT
  | RPAREN
  | PUSH
  | POP
  | NEWLINE
  | MODEL
  | LPAREN
  | LOGIC_QFUF
  | LOGIC_QFNRA
  | LOGIC_QFNIRA
  | LOGIC_QFNIA
  | LOGIC_QFLRA
  | LOGIC_QFLIRA
  | LOGIC_QFLIA
  | LOGIC_ALL
  | LET
  | KEYWORD of (string)
  | INT_CONST of (int)
  | GET_VALUE
  | GET_UNSAT_CORE
  | GET_PROOF
  | GET_OPTION
  | GET_MODEL
  | GET_INFO
  | GET_ASSIGNMENT
  | GET_ASSERTS
  | FORALL
  | FLOAT_CONST of (float)
  | EXIT
  | EXISTS
  | END_OF_INPUT
  | DEFINE_SORT
  | DEFINE_FUN
  | DECLARE_SORT
  | DECLARE_FUN
  | CHECK_SAT
  | BANG
  | ASSERT
  | AS


val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Smtsyntax.smtprog)