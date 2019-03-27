
(* Based on https://smtlib.github.io/jSMTLIB/SMTLIBTutorial.pdf *)

{

  open Smtparser

  let incr_line_count : Lexing.lexbuf -> unit =
    fun lexbuf ->
      let
        pos = lexbuf.Lexing.lex_curr_p
      in
        lexbuf.Lexing.lex_curr_p <- {
          pos with
            Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
            Lexing.pos_bol = pos.Lexing.pos_cnum;
        }

  let strip_string_quotes : string -> string =
    fun str -> String.sub str 1 (String.length str - 2)

  let strip_string_colon : string -> string =
    fun str -> String.sub str 1 (String.length str - 1)
}

let numeral =
    '0'
  | ['1'-'9'] ['0'-'9']*

let decimal =
  numeral '.' ['0'-'9']+

let alpha =
  ['a'-'z' 'A'-'Z']+

let alphanum =
  ['0'-'9' 'a'-'z' 'A'-'Z']+

let binary =
  '#' 'b' ['0' '1']+

let hex =
  '#' 'x' ['0'-'9' 'a'-'f' 'A'-'F']+

let whitespace = 
  [' ' '\t' '\x0c']

let newline =
    '\n'
  | '\r'
  | '\r' '\n'

let esc =
    '\\' ['a' 'b' 't' 'n' 'f' 'r' 'v' '\\' '"' '\'' '`']

let string =
    '"' (esc | [^ '\\' '"'])* '"'

let comment =
  ';' [^ '\n' '\r']*

let parts = 
   ['~' '!' '@' '$' '%' '^' '&' '*' '_' '+' '=' '<' '>' '.' '?' '/' '-']

let simple_symbol =
  (alpha | parts) (alphanum | parts)*

let quoted_symbol =
  '|' (whitespace)* [^ '\n' '\t' '\r']* '|'

let symbol =
    simple_symbol
  | quoted_symbol

let keyword =
  ':' (alphanum | parts)+


rule tokenize context = parse
  | "(" { LPAREN }
  | ")" { RPAREN }
  (*
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  *)
  | "_" { UNDERSCORE }

  | "ALL"     { LOGIC_ALL }
  | "QF_UF"   { LOGIC_QFUF }
  | "QF_LIA"  { LOGIC_QFLIA }
  | "QF_LRA"  { LOGIC_QFLRA }
  | "QF_NIA"  { LOGIC_QFNIA }
  | "QF_NRA"  { LOGIC_QFNRA }
  | "QF_LIRA" { LOGIC_QFLIRA }
  | "QF_NIRA" { LOGIC_QFNIRA }

  | "forall" { FORALL }
  | "exists" { EXISTS }
  | "let"    { LET }
  | "as"     { AS }

  | "BitVec" { SORT_BITVEC }
  | "Array"  { SORT_ARRAY }
  | "Real"   { SORT_FLOAT }
  | "Int"    { SORT_INT }
  | "Bool"   { SORT_BOOL }

  | "set-logic"    { SET_LOGIC }
  | "declare-fun"  { DECLARE_FUN }
  | "define-fun"   { DEFINE_FUN }
  | "declare-sort" { DECLARE_SORT }
  | "define-sort"  { DEFINE_SORT }

  | "assert"         { ASSERT }
  | "get-assertions" { GET_ASSERTS }
  | "check-sat"      { CHECK_SAT }
  | "get-model"      { GET_MODEL }
  | "get-proof"      { GET_PROOF }
  | "get-unsat-core" { GET_UNSAT_CORE }
  | "get-assignment" { GET_ASSIGNMENT }
  | "get-value"      { GET_VALUE }

  | "get-option" { GET_OPTION }
  | "set-option" { SET_OPTION }
  | "get-info"   { GET_INFO }
  | "set-info"   { SET_INFO }

  | "push"  { PUSH }
  | "pop"   { POP }
  | "exit"  { EXIT }
  | "unsat" { UNSAT }
  | "sat"   { SAT }
  | "model" { MODEL }

  | binary   { INT_CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | hex      { INT_CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | numeral  { INT_CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | decimal  { FLOAT_CONST (float_of_string (Lexing.lexeme lexbuf)) }
  | string   { STRING_CONST (strip_string_quotes (Lexing.lexeme lexbuf)) }

  | symbol  { SYMBOL (Lexing.lexeme lexbuf) }
  | keyword { KEYWORD (strip_string_colon (Lexing.lexeme lexbuf) ) }

  | comment    { tokenize context lexbuf }
  | whitespace { tokenize context lexbuf }
  | newline    { incr_line_count lexbuf; tokenize context lexbuf }
  | eof        { END_OF_INPUT }


