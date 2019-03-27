(*
  lexer.mll
  Defines a lexer for R based on R's internal lexer:
    https://github.com/wch/r-source/tree/trunk/src/main/gram.y

  For use with ocamllex. Pairs with parser.mly via menhir
  The R lexer is quite complicated, and the lexing process is interleaved
  with the parsing process. This is a translation into a more traditional
  parser-generator, but some of the same complexities with respect to treatment
  of newlines and if-statements is present. Given how different the code for our
  lexer is, it's entirely possible that there are still some subtle inconsistencies.
  That said, this parser parses most of the R base library.
*)

{
  open Parser

  (* Increments the line counter for error reporting purposes. *)
  let incr_line_count : Lexing.lexbuf -> unit =
    fun lexbuf ->
      let
        pos = lexbuf.Lexing.lex_curr_p
      in
        lexbuf.Lexing.lex_curr_p <- {
          pos with
            Lexing.pos_lnum = pos.Lexing.pos_lnum + 1; (* lnum is line number *)
            Lexing.pos_bol = pos.Lexing.pos_cnum; (* bol is beginning of line *)
        }

  let filter_numeric : string -> string =
    fun str ->
      let
        len = String.length str
      in
        if len = 0
          then ""
          else if String.contains "iL" (String.get str (len - 1))
            then String.sub str 0 (len - 1)
            else str

    (* Makes a string from a token to pretty-print the token stream. *)
    let string_of_token : Parser.token -> string =
        function
        | TOP -> "TOP"
        | END_OF_INPUT -> failwith "eof" (* "END_OF_INPUT" *)
        | WHILE -> "WHILE"
        | USER_OP s -> "USER_OP (" ^ s ^ ")"
        | TRUE -> "TRUE"
        | TILDE -> "TILDE"
        | SYMBOL s -> "SYMBOL (" ^ s ^ ")"
        | STRING_CONST s -> "STRING_CONST (" ^ s ^ ")"
        | SEMI -> "SEMI"
        | RSUPER_ASSIGN -> "RSUPER_ASSIGN"
        | RPAREN -> "RPAREN"
        | REPEAT -> "REPEAT"
        | RBRACK -> "RBRACK"
        | RBRAX  -> "RBRAX"
        | RBRACE -> "RBRACE"
        | RASSIGN -> "RASSIGN"
        | QUESTION -> "QUESTION"
        | PLUS -> "PLUS"
        | OUTER_PROD -> "OUTER_PROD"
        | OR2 -> "OR2"
        | OR -> "OR"
        | NULL -> "NULL"
        | NS_GET_INT -> "NS_GET_INT"
        | NS_GET -> "NS_GET"
        | NEXT -> "NEXT"
        | NEWLINE -> "NEWLINE"
        | NE -> "NE"
        | NAN -> "NAN"
        | NA -> "NA"
        | MULT -> "MULT"
        | MOD -> "MOD"
        | MINUS -> "MINUS"
        | MATRIX_MULT -> "MATRIX_MULT"
        | MATCH -> "MATCH"
        | LT -> "LT"
        | LSUPER_ASSIGN -> "LSUPER_ASSIGN"
        | LPAREN -> "LPAREN"
        | LE -> "LE"
        | LBRACK -> "LBRACK"
        | LBRAX  -> "LBRAX"
        | LBRACE -> "LBRACE"
        | LASSIGN -> "LASSIGN"
        | KRON_PROD -> "KRON_PROD"
        | INT_DIV -> "INT_DIV"
        | INT_CONST i -> "INT_CONST (" ^ (string_of_int i) ^ ")"
        | INFINITY -> "INFINITY"
        | IN -> "IN"
        | IF -> "IF"
        | GT -> "GT"
        | GE -> "GE"
        | FUNCTION -> "FUNCTION"
        | FOR -> "FOR"
        | FLOAT_CONST f -> "FLOAT_CONST (" ^ (string_of_float f) ^ ")"
        | FALSE -> "FALSE"
        | EQ_ASSIGN -> "EQ_ASSIGN"
        | EQ -> "EQ"
        | ELSE -> "ELSE"
        | DOLLAR -> "DOLLAR"
        | DIV -> "DIV"
        | COMPLEX_CONST f -> "COMPLEX_CONST (" ^ (string_of_float f) ^ ")"
        | COMMA -> "COMMA"
        | COLON -> "COLON"
        | CARAT -> "CARAT"
        | BREAK -> "BREAK"
        | BANG -> "BANG"
        | AT -> "AT"
        | AND2 -> "AND2"
        | AND -> "AND"

    (* Whether or not the current token will ignore line numbers when it's on top of the
      context. For a more complete explanation of how this works, see the comment for step(). *)
    let nl_ignore : Parser.token -> bool =
        function
        | TOP               -> false
        | WHILE             -> true
        | USER_OP _         -> true
        | TRUE              -> false (* a value *)
        | TILDE             -> true
        | SYMBOL _          -> false (* a value *)
        | STRING_CONST _    -> false (* a value *)
        | SEMI              -> false (* TODO ? *)
        | RSUPER_ASSIGN     -> true
        | RPAREN            -> true
        | REPEAT            -> true
        | RBRACK            -> true
        | RBRAX             -> true
        | RBRACE            -> false (* braces is a block of newline-separated exprs*)
        | RASSIGN           -> true
        | QUESTION          -> true
        | PLUS              -> true
        | OUTER_PROD        -> true
        | OR2               -> true
        | OR                -> true
        | NULL              -> false (* a value *)
        | NS_GET_INT        -> false (* not sure why *)
        | NS_GET            -> false (* ^ *)
        | NEXT              -> false (* nothing expected after next *)
        | NEWLINE           -> true
        | NE                -> true
        | NAN               -> false (* a value *)
        | NA                -> false (* a value *)
        | MULT              -> true
        | MOD               -> true
        | MINUS             -> true
        | MATRIX_MULT       -> true
        | MATCH             -> true
        | LT                -> true
        | LSUPER_ASSIGN     -> true
        | LPAREN            -> true
        | LE                -> true
        | LBRACK            -> true
        | LBRAX             -> true
        | LBRACE            -> false (* nothing expected after lbrace *)
        | LASSIGN           -> true
        | KRON_PROD         -> true
        | INT_DIV           -> true
        | INT_CONST _       -> false (* a value *)
        | INFINITY          -> false (* a value *)
        | IN                -> true  (* for expects expr after in *)
        | IF                -> true  (* expect (cond) expr *)
        | GT                -> true
        | GE                -> true
        | FUNCTION          -> true (* expect (args) expr *)
        | FOR               -> true (* expect (var in expr) expr *)
        | FLOAT_CONST _     -> false (* a value *)
        | FALSE             -> false (* a value *)
        | EQ_ASSIGN         -> true
        | EQ                -> true
        | END_OF_INPUT      -> false
        | ELSE              -> true (* expect expr *)
        | DOLLAR            -> true
        | DIV               -> true
        | COMPLEX_CONST _   -> false (* a value *)
        | COMMA             -> false (* doesn't matter *)
        | COLON             -> true
        | CARAT             -> true
        | BREAK             -> false
        | BANG              -> true
        | AT                -> true (* TODO ? *)
        | AND2              -> true
        | AND               -> true

    (* What tokens affect the context - many tokens do not affect the context and are
    pushed zero times. Binops affect the context once. Something like an if (where both the
    condition and the expression can be placed with newlines) will be pushed twice. *)
    let to_push: Parser.token -> Parser.token list =
        function
        | WHILE             -> [WHILE; WHILE] (* expect (cond) expr *)
        | TRUE              -> []
        | SYMBOL _          -> []
        | STRING_CONST _    -> []
        | SEMI              -> []
        | RPAREN            -> [] (* ends LPAREN, not pushed *)
        | REPEAT            -> [REPEAT] (* expect expr *)
        | RBRACK            -> [] (* ends LBRACK *)
        | RBRAX             -> [] (* ends LBRAX, not pushed *)
        | LBRAX             -> [LBRACK; LBRACK] (* because there's no RBRAX *)
        | RBRACE            -> [] (* ends LBRACE, but not pushed *)
        | NULL              -> []
        | NS_GET_INT        -> []
        | NS_GET            -> []
        | NEXT              -> []
        | NEWLINE           -> [NEWLINE]
        | NAN               -> []
        | NA                -> []
        | LPAREN            -> [LPAREN] (* expect expr *)
        | LBRACE            -> [LBRACE] (* don't expect *)
        | INT_CONST _       -> []
        | INFINITY          -> []
        | IN                -> [] (* expect expr, but it doesn't matter because this is always in parens *)
        | IF                -> [IF; IF] (* expect (cond) body (else body2?) *)
        | FUNCTION          -> [FUNCTION; FUNCTION] (* expect (args) expr *)
        | FOR               -> [FOR; FOR] (* expect (var in expr) expr *)
        | FLOAT_CONST _     -> []
        | FALSE             -> []
        | END_OF_INPUT      -> []
        | ELSE              -> [ELSE] (* expect expr *)
        | COMPLEX_CONST _   -> []
        | COMMA             -> []
        | BREAK             -> []
        | x                 -> [x] (* Binops *)

    (* Which tokens match each other. A token will only be popped from the context when we encounter
      its match. This means that (ex.) LPAREN stays on the context until the lexer encounters an 
      RPAREN. *)
    let token_match: Parser.token -> Parser.token -> bool =
        fun t1 t2 ->
            begin
            match t1 with
            | WHILE             -> true (* things that expect expr, which can begin with any token *)
            | USER_OP _         -> true
            | TILDE             -> true
            | RSUPER_ASSIGN     -> true
            | RPAREN            -> (match t2 with
                                    | LPAREN    -> true
                                    | _         -> false)
            | REPEAT            -> true
            | RBRACK            -> (match t2 with
                                    | LBRACK    -> true
                                    | _         -> false)
            | RBRAX             -> (match t2 with
                                    | LBRAX     -> true
                                    | _         -> false)
            | RBRACE            -> (match t2 with
                                    | LBRACE    -> true
                                    | _         -> false)
            | RASSIGN           -> true
            | NEWLINE           -> true
            | QUESTION          -> true
            | PLUS              -> true
            | OUTER_PROD        -> true
            | OR2               -> true
            | OR                -> true
            | NE                -> true
            | MULT              -> true
            | MOD               -> true
            | MINUS             -> true
            | MATRIX_MULT       -> true
            | MATCH             -> true
            | LT                -> true
            | LSUPER_ASSIGN     -> true
            | LPAREN            -> (match t2 with
                                    | RPAREN    -> true
                                    | _         -> false)
            | LE                -> true
            | LBRACK            -> (match t2 with
                                    | RBRACK    -> true
                                    | _         -> false)
            | LBRAX             -> (match t2 with
                                    | RBRAX     -> true
                                    | _         -> false)
            | LBRACE            -> (match t2 with
                                    | RBRACE    -> true
                                    | _         -> false)
            | LASSIGN           -> true
            | KRON_PROD         -> true
            | INT_DIV           -> true
            | IF                -> true (*  *)
            | GT                -> true
            | GE                -> true
            | FUNCTION          -> true (* expect (args) expr *)
            | FOR               -> true (* expect (var in expr) expr *)
            | EQ_ASSIGN         -> true
            | EQ                -> true
            | ELSE              -> true (* expect expr *)
            | DOLLAR            -> true
            | DIV               -> true
            | COLON             -> true
            | CARAT             -> true
            | BANG              -> true
            | AT                -> true (* TODO ? *)
            | AND2              -> true
            | AND               -> true
            | _                 -> false
            end

(* Get the top token of the context. If the context is empty, there's a fake token called
  TOP which isn't used by the parser, but serves to describe the behavior of the empty context. *)
let get_top: Parser.token list ref -> Parser.token =
    fun context_ref ->
        match !context_ref with
        | hd::tl -> hd
        | []     -> TOP

(* Replaces the top of the context with tok. *)
let replace_top: Parser.token list ref -> Parser.token -> unit =
    fun context_ref tok ->
    match !context_ref with
    | hd::tl -> context_ref := tok::tl
    | []     -> context_ref := [tok]

(* Does the closest context clue below IFs tell us to ignore newlines before an else or not? 
  The way that if-statements are parsed in R is confusing. I'm going to call LPAREN, RBRACK, etc. 
  'delimiters', and when the top of the context is a delimiter, this is a 'delimited context'.
  When the R lexer is not inside a delimited context, newlines before elses are not ignored.
  This behavior is because of how R code is written to a command line. If you say 'if TRUE then 0\n'
  in a command-line interpreter, the interpreter will evaluate this line immediately since it is a
  valid statement. If the next thing you type is 'else 1', that's a syntax error because else by itself
  is not a valid expression.
  But in a delimited context it's easy for R to tell when you meant an if with an else or not.
  The parser will only parse an if-statement without an else if it gets to the righthand delimiter 
  without encountering an else. This works the same way with nested ifs, where they behave differently 
  outside a delimited context. The way that this is implemented here is that there's a special regex
  that matches any blank space followed by a newline followed by whitespace followed by else. When lexing
  this regex, we check the context to see whether we should produce a NEWLINE token before the ELSE. 
  A seemingly simpler way to do this is just to say, IF's will push themselves three times onto the 
  context when the context is delimited. This does not succeed because if there is not an else after
  the if, the newlines that separate the if-statement from the statement after it should not be ignored.*)
let rec is_if_context: Parser.token list -> bool =
    fun context ->
    match context with
    | LPAREN::tl -> true
    | LBRACK::tl -> true
    | LBRAX::tl  -> true
    | LBRACE::tl -> true
    | IF::tl     -> is_if_context tl
    | _          -> false

(* When the lexer matches a lexeme that contains newlines, its position and column reporting
 gets messed up. In errors, it reports a column number that matches the total length of the lexeme
 and a line number which effectively ignores the newlines in the matched lexeme. This function
 updates the lexer position to the correct information. This is used for handling elses with newlines,
 where the regex matches all the newlines before the else. *)
let update_position =
    fun lexeme lexbuf ->
    for i = 0 to (String.length lexeme) - 1 do
        (* If we're at a newline, then call new_line to reset the column number and
        increment the line number *)
        if lexeme.[i] = '\n' then (*TODO what about \r\n newlines? *)
            (* Lexing.new_line lexbuf *)
            incr_line_count lexbuf
        (* Otherwise, increment the column number since this is a character *)
        (* The column number does not need to change? Lexer calculates column based on
        the cnum (global to the file) and the bol? *)
        else () (* lexbuf.Lexing.lex_curr_p.pos_cnum <- lexbuf.Lexing.lex_curr_p.pos_cnum + 1 *)
    done

(* Print the context for debugging *)
let string_of_context: Parser.token list ref -> string =
    fun context_ref ->
    let context_strs = List.map string_of_token !context_ref in
    "[" ^ (String.concat ", " context_strs) ^ "]"

(* Step is the function that maintains the lexing context during lexing. The context determines
  whether newlines should be ignored. In R, newlines separate statements from each other, so
  3\n+4 is actually two statements. The expression 3, and the expression unary_plus 4. Notably
  3+\n4 parses to 3+4 because the plus token affects the context to ignore newlines. R ignores newlines
  when it expects something to complete the expression. In 3+, it knows that there must be an expression
  to complete the + statement, so it ignores newlines until it finds one. Essentially, whether or not the
  context ignores newlines represents whether or not the current expression is incomplete.
  The general algorithm for this is when we see a token, if it affects the context,
  put it on the stack, then use its behavior until a match is found, at which point it is
  removed from the stack, and we go back to the newline behavior of the character under it.
  Tokens that do not have a match do not go onto the context stack.*)
    let step : Parser.token -> (Parser.token list) ref -> unit =
        fun tok context_ref ->
            let top = get_top context_ref in
            (* let _ = Printf.printf "CONTEXT: %s\n" (string_of_context context_ref) in *)
            (* let _ = Printf.printf "TOKEN: %s\n" (string_of_token tok) in *)
            (* If the top token of the context matches the current token, remove it:
                it has found its match. *)
            let _ = if (token_match top tok) then
                context_ref := (List.tl !context_ref)
                else () in
            (* Push context alterations from token onto the stack *)
            let x = to_push tok in
            context_ref := x @ !context_ref

(* When parsing a string, the lexeme is everything matched by
  the regex, including the quotes. We actually want to make an R string using
  the characters inside the quotes. *)
let strip_string_quotes : string -> string =
  fun str -> String.sub str 1 (String.length str - 2)
}

let hex_digit =
  ['0'-'9' 'a'-'f' 'A'-'F']

let hex =
  '0' ['x' 'X'] hex_digit+

let digit =
  ['0'-'9']

let int =
  digit+ 'L'?

let exp =
  ['e' 'E'] ['+' '-']? int

let float =
    digit+ '.' digit* exp?
  | digit+ exp?
  | '.' digit+ exp?

(* R parses 1+3i as a plus operation involving a double (1.0) and a complex (0,3.0), then
  implicitly casts the double to a complex (1.0,0) and does the addition. Since SimpleR does
  not do any kind of implicit conversion, this makes complex numbers hard to deal with, since 
  you would have to explicitly cast the double to a complex. *)
let complex =
    int 'i'
  | float 'i'

let oct_esc =
    '\\' ['0'-'3'] ['0'-'7'] ['0'-'7']
  | '\\' ['0'-'7'] ['0'-'7']
  | '\\' ['0'-'7']

let hex_esc =
  '\\' hex+

let uni_esc =
    '\\' 'u' hex hex hex hex
  | '\\' 'u' '{' hex hex hex hex '}'

let esc =
    '\\' ['a' 'b' 't' 'n' 'f' 'r' 'v' '\\' '"' '\'' '`']
  | oct_esc
  | hex_esc
  | uni_esc

(* escaped characters, or any character which is not
 an escape or end string delimiter,
 all enclosed within the delimiters *)
let string =
    '"' (esc | [^ '\\' '"'])* '"'
  | '\'' (esc | [^ '\\' '\''])* '\''

let alpha =
    ['a'-'z' 'A'-'Z']

let ident =
    '.' (alpha | '_' | '.') (alpha | digit | '_' | '.')*
  | alpha (alpha | digit | '_' | '.')*
  | '.' (* I guess *)

(* non-syntactic variable names *)
let nsident =
  '`' (esc | [^ '\\' '`'])+ '`'

(* Technically % [^ '\n']+ % *)
let user_op =
  '%' [^ '%' '\n'] '%'

let newline =
    '\n'
  | '\r'
  | '\r' '\n'

let comment =
  '#' [^ '\n']*

(* spaces, tabs, form feeds *)
(* TODO: others? *)
let whitespace =
  [' ' '\t' '\x0c']


(* For matching elses that occur after an if inside an if-context *)
let nlelse = whitespace* "else"
(* Any number of lines with no semantic meaning.
 Require at least one, since we're interested in handling else's that
 can be on another line inside an if-context. *)
let nothing = (whitespace* comment? newline)+
let ifelse = nothing nlelse

(* Parsing *)
rule tokenize context = parse


  (* Delimiters *)
  | "("         { step LPAREN context; LPAREN }
  | ")"         { step RPAREN context; RPAREN }

  | "["         { step LBRACK context; LBRACK }
  | "]"         { step RBRACK context; RBRACK }

  | "[["        { step LBRACK context; step LBRACK context; LBRAX }
  (* | "]]"        { step RBRAX context; RBRAX } *)

  | "{"         { step LBRACE context; LBRACE }
  | "}"         { step RBRACE context; RBRACE }

  (* Operators (cf 3.1.4) *)
  | "-"         { step MINUS context; MINUS }
  | "+"         { step PLUS context; PLUS }
  | "!"         { step BANG context; BANG }
  | "~"         { step TILDE context; TILDE }
  | "?"         { step QUESTION context; QUESTION }
  | ":"         { step COLON context; COLON }
  | "*"         { step MULT context; MULT }
  | "/"         { step DIV context; DIV }
  | "^"         { step CARAT context; CARAT }
  | "%%"        { step MOD context; MOD }
  | "%/%"       { step INT_DIV context; INT_DIV }
  | "%*%"       { step MATRIX_MULT context; MATRIX_MULT }
  | "%o%"       { step OUTER_PROD context; OUTER_PROD }
  | "%x%"       { step KRON_PROD context; KRON_PROD }
  | "%in%"      { step MATCH context; MATCH }
  | "<"         { step LT context; LT }
  | ">"         { step GT context; GT }
  | "=="        { step EQ context; EQ }
  | "!="        { step NE context; NE }
  | ">="        { step GE context; GE }
  | "<="        { step LE context; LE }
  | "&&"        { step AND context; AND }
  | "&"         { step AND2 context; AND2 }
  | "||"        { step OR context; OR }
  | "|"         { step OR2 context; OR2 }
  | "<-"        { step LASSIGN context; LASSIGN }
  | "->"        { step RASSIGN context; RASSIGN }
  | "$"         { step DOLLAR context; DOLLAR }

  (* Additional operators (cf 10.4.2) *)
  | "::"        { step NS_GET context; NS_GET }
  | ":::"       { step NS_GET_INT context; NS_GET_INT }
  | "@"         { step AT context; AT }
  | "<<-"       { step LSUPER_ASSIGN context; LSUPER_ASSIGN }
  | "->>"       { step RSUPER_ASSIGN context; RSUPER_ASSIGN }
  | "="         { step EQ_ASSIGN context; EQ_ASSIGN }

  (* Were not listed but likely relevant *)
  | ";"         { step SEMI context; SEMI }
  | ":="        { step EQ_ASSIGN context; EQ_ASSIGN }
  | "..."       { step (SYMBOL "") context; SYMBOL (Lexing.lexeme lexbuf) }

  (* Dumb Special Cases *)
  | ifelse      { update_position (Lexing.lexeme lexbuf) lexbuf; (* Fix the lexer's position *)
        (* if we're in an if context, then ifelse is treated exactly like a normal ELSE *)
        if is_if_context !context then
            let _ = step ELSE context in
            ELSE
        (* if we're not in an if context, fail since elses cannot follow NLs *)
        else
            failwith "Bad else!" }

  (* Keywords *)
  | "function"  { step FUNCTION context; FUNCTION }
  | "if"        { step IF context; IF }
  | "else"      { step ELSE context; ELSE }
  | "for"       { step FOR context; FOR }
  | "in"        { step IN context; IN }
  | "while"     { step WHILE context; WHILE }
  | "repeat"    { step REPEAT context; REPEAT }
  | "next"      { step NEXT context; NEXT }
  | "break"     { step BREAK context; BREAK }

  (* Native values *)
  | "NULL"      { step NULL context; NULL }
  | "NA"        { step NA context; NA }
  | "Inf"       { step INFINITY context; INFINITY }
  | "NaN"       { step NAN context; NAN }
  | "TRUE"      { step TRUE context; TRUE }
  | "FALSE"     { step FALSE context; FALSE }

  (* To be skipped *)
  | comment     { tokenize context lexbuf }
  | whitespace  { tokenize context lexbuf }

  (* Valued tokens *)
  | ident       { step (SYMBOL (Lexing.lexeme lexbuf)) context;
                    SYMBOL (Lexing.lexeme lexbuf) }
  (* For non-syntactic idents, the `s should be removed *)
  | nsident     { let lexstr = Lexing.lexeme lexbuf in
                    let str = String.sub lexstr 1 (String.length lexstr - 2) in
                    step (SYMBOL str) context;
                    SYMBOL str }
  | user_op     { step (USER_OP (Lexing.lexeme lexbuf)) context;
                    USER_OP (Lexing.lexeme lexbuf) }
  | string      { step (STRING_CONST (strip_string_quotes (Lexing.lexeme lexbuf))) context;
                    STRING_CONST (strip_string_quotes (Lexing.lexeme lexbuf)) }
  | hex         { step (INT_CONST 0) context;
                    INT_CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | int         { step (INT_CONST 0) context;
                    INT_CONST (int_of_string (filter_numeric (Lexing.lexeme lexbuf))) }
  | float       { step (FLOAT_CONST 0.) context;
                    FLOAT_CONST (float_of_string (filter_numeric (Lexing.lexeme lexbuf))) }
  | complex     { step (COMPLEX_CONST 0.) context;
                    COMPLEX_CONST (float_of_string (filter_numeric (Lexing.lexeme lexbuf))) }

  (* Only output newlines if the top token of the context allows them *)
  | newline     { incr_line_count lexbuf;
        if nl_ignore (get_top context) then tokenize context lexbuf else NEWLINE }
  | ','         { step COMMA context; COMMA }

  (* Everybody's favorite thing that's technically not a char sometimes *)
  | eof         { END_OF_INPUT }

