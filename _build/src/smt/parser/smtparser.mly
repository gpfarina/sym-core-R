
%{
  open Smtsyntax
%}

%token         END_OF_INPUT
%token         NEWLINE
%token<int>    INT_CONST
%token<float>  FLOAT_CONST
%token<string> STRING_CONST
%token<string> SYMBOL
%token<string> KEYWORD
%token         UNDERSCORE


%token LPAREN RPAREN
(*
%token LBRACE RBRACE LBRACK RBRACK
*)

%token LOGIC_ALL
%token LOGIC_QFUF
%token LOGIC_QFLIA LOGIC_QFLRA
%token LOGIC_QFNIA LOGIC_QFNRA
%token LOGIC_QFLIRA LOGIC_QFNIRA

%token AS FORALL EXISTS LET BANG

%token SORT_INT SORT_FLOAT SORT_BOOL SORT_BITVEC SORT_ARRAY

%token SET_LOGIC
%token DECLARE_FUN DEFINE_FUN DECLARE_SORT DEFINE_SORT
%token ASSERT GET_ASSERTS CHECK_SAT GET_MODEL GET_PROOF GET_UNSAT_CORE
%token GET_ASSIGNMENT GET_VALUE
%token PUSH POP
%token GET_OPTION SET_OPTION
%token GET_INFO SET_INFO
%token EXIT SAT UNSAT MODEL


%start prog
%type <Smtsyntax.smtprog> prog

%%

logic:
  | LOGIC_ALL    { SmtLogALL }
  | LOGIC_QFUF   { SmtLogQFUF }
  | LOGIC_QFLIA  { SmtLogQFLIA }
  | LOGIC_QFLRA  { SmtLogQFLRA }
  | LOGIC_QFNIA  { SmtLogQFNIA }
  | LOGIC_QFNRA  { SmtLogQFNRA }
  | LOGIC_QFLIRA { SmtLogQFLIRA }
  | LOGIC_QFNIRA { SmtLogQFNIRA }

numq:
  |                { [] }
  | INT_CONST numq { $1 :: $2 }

nums:
  | INT_CONST      { [$1] }
  | INT_CONST numq { $1 :: $2 }

symbq:
  |              { [] }
  | SYMBOL symbq { $1 :: $2 }

symbs:
  | SYMBOL       { [$1] }
  | SYMBOL symbq { $1 :: $2 }

sort:
  | SORT_INT   { SmtSortInt }
  | SORT_FLOAT { SmtSortFloat }
  | SORT_BOOL  { SmtSortBool }
  | LPAREN UNDERSCORE SORT_BITVEC INT_CONST RPAREN { SmtSortBitVec ($4) }
  | UNDERSCORE SORT_BITVEC INT_CONST               { SmtSortBitVec ($3) }
  | LPAREN SORT_ARRAY sorts sort RPAREN            { SmtSortArray ($3, $4) }
  | SORT_ARRAY sorts sort                          { SmtSortArray ($2, $3) }
  | LPAREN SYMBOL sorts RPAREN                     { SmtSortApp ($2, $3) }
  | SYMBOL sorts                                   { SmtSortApp ($1, $2) }

sortq:
  |            { [] }
  | sort sortq { $1 :: $2 }

sorts:
  | sort       { [$1] }
  | sort sortq { $1 :: $2 }

ident:
  | SYMBOL                               { SmtVar ($1) }
  | LPAREN UNDERSCORE SYMBOL SYMBOL      { SmtIndVarVar ($3, $4) }
  | LPAREN UNDERSCORE SYMBOL nums RPAREN { SmtIndVarInt ($3, $4) }
  | LPAREN AS SYMBOL SYMBOL RPAREN       { SmtQualVarVar ($3, $4) }
  | LPAREN AS SYMBOL sort RPAREN         { SmtQualVarSort ($3, $4) }

symbsort:
  | LPAREN SYMBOL sort RPAREN { ($2, $3) }

symbsortq:
  |                    { [] }
  | symbsort symbsortq { $1 :: $2 }

symbsorts:
  | symbsort           { [$1] }
  | symbsort symbsorts { $1 :: $2 }

symbexpr:
  | LPAREN SYMBOL expr RPAREN { ($2, $3) }

symbexprq:
  |                    { [] }
  | symbexpr symbexprq { $1 :: $2 }

symbexprs:
  | symbexpr           { [$1] }
  | symbexpr symbexprs { $1 :: $2 }

expr:
  | INT_CONST     { SmtConst (string_of_int $1) }
  | FLOAT_CONST   { SmtConst (string_of_float $1) }
  | STRING_CONST  { SmtConst ("\"" ^ $1 ^ "\"") }
  | SYMBOL        { SmtVar ($1) }
  | ident         { $1 }
  | LPAREN SYMBOL exprs RPAREN { SmtFunApp ($2, $3) }
  | LPAREN FORALL LPAREN symbsorts RPAREN expr RPAREN { SmtForAll ($4, $6) }
  | LPAREN EXISTS LPAREN symbsorts RPAREN expr RPAREN { SmtExists ($4, $6) }
  | LPAREN LET LPAREN symbexprs RPAREN expr RPAREN    { SmtLet ($4, $6) }
  | LPAREN BANG expr KEYWORD SYMBOL RPAREN { $3 } (* Ignored *)

exprq:
  |            { [] }
  | expr exprq { $1 :: $2 }

exprs:
  | expr       { [$1] }
  | expr exprq { $1 :: $2 }

cmd:
  | LPAREN SET_LOGIC logic RPAREN { SmtSetLogic ($3) }

  | LPAREN DECLARE_FUN SYMBOL LPAREN sortq RPAREN sort RPAREN         { SmtDeclFun ($3, $5, $7) }
  | LPAREN DEFINE_FUN SYMBOL LPAREN symbsortq RPAREN sort expr RPAREN { SmtDefFun ($3, $5, $7, $8) }
  | LPAREN DECLARE_SORT SYMBOL INT_CONST RPAREN                       { SmtDeclSort ($3, $4) }
  | LPAREN DEFINE_SORT SYMBOL LPAREN symbs RPAREN sort RPAREN         { SmtDefSort ($3, $5, $7) }

  | LPAREN ASSERT expr RPAREN { SmtAssert ($3) }
  | LPAREN GET_ASSERTS RPAREN { SmtGetAsserts }

  | LPAREN CHECK_SAT RPAREN       { SmtCheckSat }
  | LPAREN GET_MODEL RPAREN       { SmtGetModel }
  | LPAREN GET_PROOF RPAREN       { SmtGetProof }
  | LPAREN GET_UNSAT_CORE RPAREN  { SmtGetUnsatCore }
  | LPAREN GET_ASSIGNMENT RPAREN  { SmtGetAssignment }
  | LPAREN GET_VALUE exprs RPAREN { SmtGetValue ($3) }

  | LPAREN PUSH RPAREN           { SmtPush (1) }
  | LPAREN PUSH INT_CONST RPAREN { SmtPush ($3) }
  | LPAREN POP RPAREN            { SmtPop (1) }
  | LPAREN POP INT_CONST RPAREN  { SmtPop ($3) }

  (* Hacking around with <symbol> instead of <attr-value> *)
  | LPAREN GET_OPTION KEYWORD LPAREN        { SmtGetOption ($3) }
  | LPAREN SET_OPTION KEYWORD SYMBOL RPAREN { SmtSetOption ($3, $4) }
  | LPAREN GET_INFO KEYWORD LPAREN          { SmtGetInfo ($3) }
  | LPAREN SET_INFO KEYWORD SYMBOL RPAREN   { SmtSetInfo ($3, $4) }

  | LPAREN EXIT RPAREN  { SmtExit }
  | LPAREN SAT RPAREN   { SmtSat }
  | SAT                 { SmtSat }
  | LPAREN UNSAT RPAREN { SmtUnsat }
  | UNSAT               { SmtUnsat }

  | LPAREN MODEL cmdq RPAREN  { SmtModel ($3) }

cmdq:
  |          { [] }
  | cmd cmdq { $1 :: $2 }

cmds:
  | cmd      { [$1] }
  | cmd cmdq { $1 :: $2 }

prog:
    END_OF_INPUT { [] }
  | cmd          { [$1] }
  | cmd prog     { $1 :: $2 }
  ;


