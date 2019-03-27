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

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState176
  | MenhirState174
  | MenhirState170
  | MenhirState168
  | MenhirState156
  | MenhirState153
  | MenhirState150
  | MenhirState147
  | MenhirState143
  | MenhirState138
  | MenhirState135
  | MenhirState125
  | MenhirState121
  | MenhirState119
  | MenhirState117
  | MenhirState115
  | MenhirState113
  | MenhirState111
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState99
  | MenhirState97
  | MenhirState95
  | MenhirState90
  | MenhirState88
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState57
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState40
  | MenhirState36
  | MenhirState32
  | MenhirState31
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState21
  | MenhirState20
  | MenhirState19
  | MenhirState4
  | MenhirState2
  | MenhirState1
  | MenhirState0

  
  module A = Rast
  open A
let _eRR =
  Error

let rec _menhir_run125 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NULL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | STRING_CONST _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | SYMBOL _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | COMMA | RBRACK | RPAREN ->
        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_goto_sublist : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit A.arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                                ( A.ListSub (_1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | RBRACK ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
                let _v : (unit A.expr) =                                ( A.ListProj (_1, _3) ) in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                                ( A.FuncCall (_1, _3)) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit A.arg) =                                 ( A.EmptyArg ) in
    _menhir_goto_sub _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | EQ_ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BANG ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | BREAK ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | COMPLEX_CONST _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FLOAT_CONST _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | FOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FUNCTION ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INFINITY ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INT_CONST _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | LBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NA ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NAN ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEXT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NULL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | QUESTION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | REPEAT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | STRING_CONST _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | SYMBOL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | TILDE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | COMMA | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (unit A.arg) =                                 ( A.IdentAssignEmpty {A.default_ident with name=_1} ) in
            _menhir_goto_sub _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | NS_GET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
    | NS_GET_INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
    | AND | AND2 | AT | CARAT | COLON | COMMA | DIV | DOLLAR | EQ | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LBRACK | LBRAX | LE | LPAREN | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACK | RPAREN | RSUPER_ASSIGN | TILDE | USER_OP _ ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | EQ_ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BANG ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | BREAK ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | COMPLEX_CONST _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FLOAT_CONST _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | FOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FUNCTION ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | INFINITY ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | INT_CONST _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | LBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NA ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NAN ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NEXT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NULL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | QUESTION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | REPEAT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | STRING_CONST _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | SYMBOL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | TILDE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | COMMA | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (unit A.arg) =                                 ( A.StringAssignEmpty _1 ) in
            _menhir_goto_sub _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | NS_GET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
    | NS_GET_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
    | AND | AND2 | AT | CARAT | COLON | COMMA | DIV | DOLLAR | EQ | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LBRACK | LBRAX | LE | LPAREN | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACK | RPAREN | RSUPER_ASSIGN | TILDE | USER_OP _ ->
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | EQ_ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BANG ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BREAK ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | COMPLEX_CONST _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | FLOAT_CONST _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | FOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | FUNCTION ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INFINITY ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INT_CONST _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | LBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NA ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NAN ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NEXT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NULL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | QUESTION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | REPEAT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | STRING_CONST _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | SYMBOL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | TILDE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | COMMA | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _v : (unit A.arg) =                                 ( A.NullAssignEmpty ) in
            _menhir_goto_sub _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | AND | AND2 | AT | CARAT | COLON | COMMA | DIV | DOLLAR | EQ | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LBRACK | LBRAX | LE | LPAREN | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACK | RPAREN | RSUPER_ASSIGN | TILDE | USER_OP _ ->
        _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_expr_or_assign : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _3), _, _5), _, _7) = _menhir_stack in
        let _v : (unit A.expr) =                               ( A.For (({A.default_ident with name=_3}, _5), _7) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _v : (unit A.expr) =                                   ( A.Bop(A.Assign, _1, _3) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (unit A.expr) =                  ( _1 ) in
        _menhir_goto_expr_or_assign _menhir_env _menhir_stack _menhir_s _v
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _3), _, _5) = _menhir_stack in
        let _v : (unit A.expr) =                                ( A.FuncDec (_3, _5)) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BANG ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | BREAK ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | COMPLEX_CONST _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | FLOAT_CONST _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | FUNCTION ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | INFINITY ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | INT_CONST _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | LBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NA ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NAN ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NEXT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NULL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | PLUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | QUESTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | REPEAT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | STRING_CONST _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | SYMBOL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | TILDE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
        | AND | AND2 | AT | CARAT | COLON | COMMA | DIV | DOLLAR | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LBRACK | LBRAX | LE | LPAREN | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, _2), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                               ( A.If (_2, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, _2), _, _3), _, _5) = _menhir_stack in
        let _v : (unit A.expr) =                               ( A.IfElse (_2, _3, _5) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _v : (unit A.expr list) =                                     ( _1 @ [_3] ) in
        _menhir_goto_exprlist _menhir_env _menhir_stack _menhir_s _v
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _v : (unit A.expr list) =                                     ( _1 @ [_3] ) in
        _menhir_goto_exprlist _menhir_env _menhir_stack _menhir_s _v
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr list) =                                     ( [_1] ) in
        _menhir_goto_exprlist _menhir_env _menhir_stack _menhir_s _v
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (unit A.expr) =                                  ( _2 ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
        let _v : (unit A.expr) =                               ( A.Repeat _2 ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, _2), _, _3) = _menhir_stack in
        let _v : (unit A.expr) =                               ( A.While (_2, _3) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState176 | MenhirState174 | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NEWLINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BANG ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | BREAK ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | COMPLEX_CONST _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | END_OF_INPUT ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | FLOAT_CONST _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | FUNCTION ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | INFINITY ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | INT_CONST _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | LBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | NA ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | NAN ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | NEWLINE ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | NEXT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | NULL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | PLUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | QUESTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | REPEAT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | STRING_CONST _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | SYMBOL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | TILDE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BANG ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | BREAK ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | COMPLEX_CONST _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | END_OF_INPUT ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | FLOAT_CONST _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | FUNCTION ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | INFINITY ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | INT_CONST _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | LBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NA ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NAN ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NEWLINE ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NEXT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NULL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | PLUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | QUESTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | REPEAT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | STRING_CONST _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | SYMBOL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | TILDE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sub : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit A.arg) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.arg list) =                       ( _1 @ [_3] ) in
        _menhir_goto_sublist _menhir_env _menhir_stack _menhir_s _v
    | MenhirState49 | MenhirState59 | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (unit A.arg list) =                       ( [_1] ) in
        _menhir_goto_sublist _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run117 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run119 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119

and _menhir_run121 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_run113 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113

and _menhir_run115 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NULL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | STRING_CONST _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | SYMBOL _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | COMMA | RPAREN ->
        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NULL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | STRING_CONST _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | SYMBOL _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | COMMA | RBRACK ->
        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NULL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | STRING_CONST _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | SYMBOL _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | COMMA | RBRACK ->
        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run109 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING_CONST _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                              ( A.ListProj (_1, [A.ExprArg (A.StringConst _3)]) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                              ( A.ListProj (_1, [A.ExprArg (A.Ident {A.default_ident with name=_3})]) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run101 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING_CONST _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                              ( A.Bop (A.ObjAttr, _1, (A.StringConst _3)) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                              ( A.Bop (A.ObjAttr, _1, (A.Ident {A.default_ident with name=_3})) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce13 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
    let _v : (unit A.expr) =            ( A.Ident { A.default_ident with name = _1 } ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING_CONST _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                                          ( A.Bop (A.GetPackageInt, (A.Ident {A.default_ident with name=_1}), (A.StringConst _3)) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                                          ( A.Bop (A.GetPackageInt, (A.Ident {A.default_ident with name=_1}), (A.Ident {A.default_ident with name=_3})) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING_CONST _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                                          ( A.Bop (A.GetPackage, (A.Ident {A.default_ident with name=_1}), (A.StringConst _3)) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                                          ( A.Bop (A.GetPackage, (A.Ident {A.default_ident with name=_1}), (A.Ident {A.default_ident with name=_3})) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
    let _v : (unit A.expr) =                   ( A.StringConst _1 ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING_CONST _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                                          ( A.Bop (A.GetPackageInt, (A.StringConst _1), (A.StringConst _3)) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                                          ( A.Bop (A.GetPackageInt, (A.StringConst _1), (A.Ident {A.default_ident with name=_3}))) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING_CONST _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                                          ( A.Bop (A.GetPackage, (A.StringConst _1), (A.StringConst _3)) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit A.expr) =                                          ( A.Bop (A.GetPackage, (A.StringConst _1), (A.Ident {A.default_ident with name=_3})) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce12 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : (unit A.expr) =                   ( A.Null ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_exprlist : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit A.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NEWLINE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BANG ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | BREAK ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | COMPLEX_CONST _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | FLOAT_CONST _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | FOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | FUNCTION ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | INFINITY ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | INT_CONST _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | LBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | NA ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | NAN ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | NEXT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | NULL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | QUESTION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | REPEAT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | STRING_CONST _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | SYMBOL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | TILDE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | NEWLINE | RBRACE | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (unit A.expr list) =                                     ( _1 ) in
            _menhir_goto_exprlist _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
        let _v : (unit A.expr) =                                ( A.Block _2 ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BANG ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | BREAK ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | COMPLEX_CONST _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | FLOAT_CONST _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | FOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | FUNCTION ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | INFINITY ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | INT_CONST _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | LBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NA ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NAN ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NEXT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NULL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | QUESTION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | REPEAT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | STRING_CONST _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | SYMBOL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | TILDE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NEWLINE | RBRACE | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (unit A.expr list) =                                     ( _1 ) in
            _menhir_goto_exprlist _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_goto_formlist : _menhir_env -> 'ttv_tail -> (unit A.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | SYMBOL _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | EQ_ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BANG ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | BREAK ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | COMPLEX_CONST _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | FLOAT_CONST _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | FUNCTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | IF ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | INFINITY ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | INT_CONST _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | LBRACE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | LPAREN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | MINUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | NA ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | NAN ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | NEXT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | NULL ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | PLUS ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | QUESTION ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | REPEAT ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | STRING_CONST _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | SYMBOL _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | TILDE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | TRUE ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
            | COMMA | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _1), _3) = _menhir_stack in
                let _v : (unit A.param list) =                                          ( _1 @ [A.Param {A.default_ident with name=_3}] ) in
                _menhir_goto_formlist _menhir_env _menhir_stack _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BANG ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | BREAK ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | COMPLEX_CONST _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | FLOAT_CONST _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | FOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | FUNCTION ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | INFINITY ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | INT_CONST _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | LBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LPAREN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | NA ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | NAN ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | NEXT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | NULL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | QUESTION ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | REPEAT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | STRING_CONST _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | SYMBOL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | TILDE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Rast.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (unit Rast.program) =                                 ( _2 ) in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit Rast.program) =                                 ( _1 :: _3 ) in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit Rast.program) =                                 ( _1 :: _3 ) in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Obj.magic _1
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit A.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Uop (A.Not, _2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _2), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.FuncCall (A.Ident { A.default_ident with name=_2 }, A.ExprArg _1 :: [A.ExprArg _3]) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.arg) =                                 ( A.IdentAssign ({A.default_ident with name=_1}, _3) ) in
            _menhir_goto_sub _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Form, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ | EQ_ASSIGN | GE | GT | LASSIGN | LE | LSUPER_ASSIGN | LT | MINUS | NE | NEWLINE | OR | OR2 | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Plus, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.OuterProd, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.arg) =                                 ( A.StringAssign (_1, _3) ) in
            _menhir_goto_sub _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.SuperAssign, _3, _1) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Or, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Neq, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | LASSIGN | LE | LSUPER_ASSIGN | LT | MINUS | MULT | NE | NEWLINE | OR | OR2 | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Mult, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Mod, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _3) = _menhir_stack in
            let _v : (unit A.arg) =                                 ( A.NullAssign _3 ) in
            _menhir_goto_sub _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Assign, _3, _1) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.OrVec, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ | EQ_ASSIGN | GE | GT | LASSIGN | LE | LSUPER_ASSIGN | LT | MINUS | NE | NEWLINE | OR | OR2 | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Minus, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.MatrixMult, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COLON | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Range, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COLON | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Pow, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Match, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.KronProd, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.IntDiv, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | LASSIGN | LE | LSUPER_ASSIGN | LT | MINUS | MULT | NE | NEWLINE | OR | OR2 | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Div, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Lt, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Le, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Gt, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Ge, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Eq, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.And, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | AND | AND2 | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | OR | OR2 | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.AndVec, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | NEWLINE | QUESTION | RBRACE | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Help, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | NEWLINE | QUESTION | RBRACE | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.SuperAssign, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | NEWLINE | QUESTION | RBRACE | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Bop (A.Assign, _1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 | MenhirState59 | MenhirState73 | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (unit A.arg) =                                 ( A.ExprArg _1 ) in
            _menhir_goto_sub _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BANG ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | BREAK ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | COMPLEX_CONST _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | FLOAT_CONST _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | FUNCTION ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | INFINITY ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | INT_CONST _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | LBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | NA ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | NAN ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | NEXT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | NULL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | PLUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | QUESTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | REPEAT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | STRING_CONST _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | SYMBOL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | TILDE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState170 | MenhirState176 | MenhirState174 | MenhirState168 | MenhirState19 | MenhirState27 | MenhirState28 | MenhirState156 | MenhirState153 | MenhirState32 | MenhirState150 | MenhirState143 | MenhirState138 | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | EQ_ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BANG ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | BREAK ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | COMPLEX_CONST _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | FLOAT_CONST _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | FUNCTION ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | INFINITY ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | INT_CONST _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | LBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LPAREN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | MINUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | NA ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | NAN ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | NEXT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | NULL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | PLUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | QUESTION ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | REPEAT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | STRING_CONST _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | SYMBOL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | TILDE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | NEWLINE | RBRACE | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (unit A.expr) =                  ( _1 ) in
            _menhir_goto_expr_or_assign _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _1), _, _3) = _menhir_stack in
            let _v : (unit A.param list) =                                          ( [A.DefaultParam ({A.default_ident with name=_1}, _3)] ) in
            _menhir_goto_formlist _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _1), _3), _, _5) = _menhir_stack in
            let _v : (unit A.param list) =                                          ( _1 @ [A.DefaultParam ({A.default_ident with name=_3}, _5)] ) in
            _menhir_goto_formlist _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COLON | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Uop (A.UMinus, _2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AND | AND2 | COLON | COMMA | DIV | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LE | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Uop (A.UPlus, _2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | NEWLINE | QUESTION | RBRACE | RBRACK | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Uop (A.UHelp, _2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA | ELSE | EQ_ASSIGN | LASSIGN | LSUPER_ASSIGN | NEWLINE | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (unit A.expr) =                             ( A.Uop (A.UForm, _2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | AND2 ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | AT ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | CARAT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | DOLLAR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | INT_DIV ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | KRON_PROD ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LASSIGN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRAX ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LSUPER_ASSIGN ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MATCH ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MATRIX_MULT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | OR2 ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OUTER_PROD ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack)
        | RASSIGN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (unit A.expr) =                        ( _2 ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState31 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BANG ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | BREAK ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | COMPLEX_CONST _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | FLOAT_CONST _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | FUNCTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | IF ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | INFINITY ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | INT_CONST _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | LBRACE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | LPAREN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | MINUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | NA ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | NAN ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | NEXT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | NULL ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | PLUS ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | QUESTION ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | REPEAT ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | STRING_CONST _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | SYMBOL _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | TILDE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | TRUE ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
            | MenhirState1 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BANG ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | BREAK ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | COMPLEX_CONST _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | FLOAT_CONST _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | FUNCTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | IF ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | INFINITY ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | INT_CONST _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | LBRACE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | LPAREN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | MINUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | NA ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | NAN ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | NEXT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | NULL ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | PLUS ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | QUESTION ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | REPEAT ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | STRING_CONST _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | SYMBOL _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | TILDE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | TRUE ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
            | _ ->
                _menhir_fail ())
        | RSUPER_ASSIGN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TILDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | USER_OP _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit A.expr) =                   ( A.BoolConst true ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | NS_GET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
    | NS_GET_INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
    | AND | AND2 | AT | CARAT | COLON | COMMA | DIV | DOLLAR | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LBRACK | LBRAX | LE | LPAREN | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | NS_GET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
    | NS_GET_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
    | AND | AND2 | AT | CARAT | COLON | COMMA | DIV | DOLLAR | ELSE | EQ | EQ_ASSIGN | GE | GT | INT_DIV | KRON_PROD | LASSIGN | LBRACK | LBRAX | LE | LPAREN | LSUPER_ASSIGN | LT | MATCH | MATRIX_MULT | MINUS | MOD | MULT | NE | NEWLINE | OR | OR2 | OUTER_PROD | PLUS | QUESTION | RASSIGN | RBRACE | RBRACK | RPAREN | RSUPER_ASSIGN | SEMI | TILDE | USER_OP _ ->
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit A.expr) =                               ( A.Next ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run170 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | END_OF_INPUT ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | NEWLINE ->
        _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit A.expr) =                   ( A.NumericConst (A.Float nan) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit A.expr) =                   ( A.NumericConst (A.Na) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NEWLINE | RBRACE | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState28 in
        let _v : (unit A.expr list) =                                     ( [] ) in
        _menhir_goto_exprlist _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit A.expr) =                   ( A.NumericConst (A.Int _1) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit A.expr) =                   ( A.NumericConst (A.Float infinity) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | SYMBOL _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | EQ_ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BANG ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | BREAK ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | COMPLEX_CONST _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
                | FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | FLOAT_CONST _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | FUNCTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | IF ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | INFINITY ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | INT_CONST _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
                | LBRACE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | LPAREN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | MINUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | NA ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | NAN ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | NEXT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | NULL ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | PLUS ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | QUESTION ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | REPEAT ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | STRING_CONST _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
                | SYMBOL _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
                | TILDE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | TRUE ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
            | COMMA | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _1) = _menhir_stack in
                let _v : (unit A.param list) =                                          ( [A.Param {A.default_ident with name=_1}] ) in
                _menhir_goto_formlist _menhir_env _menhir_stack _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | COMMA | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit A.param list) =                                          ( [] ) in
            _menhir_goto_formlist _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | SYMBOL _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BANG ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | BREAK ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | COMPLEX_CONST _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                | FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | FLOAT_CONST _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | FUNCTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | IF ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | INFINITY ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | INT_CONST _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                | LBRACE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | LPAREN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | MINUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | NA ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | NAN ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | NEXT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | NULL ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | PLUS ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | QUESTION ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | REPEAT ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | STRING_CONST _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                | SYMBOL _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                | TILDE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | TRUE ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit A.expr) =                   ( A.NumericConst (A.Float _1) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit A.expr) =                   ( A.BoolConst false ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run171 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit Rast.program) =                                 ( [] ) in
    _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit A.expr) =                   ( A.NumericConst (A.Complex (0.0, _1)) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit A.expr) =                               ( A.Break ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Rast.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BREAK ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | COMPLEX_CONST _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | END_OF_INPUT ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FLOAT_CONST _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUNCTION ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INFINITY ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT_CONST _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MINUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NA ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NAN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWLINE ->
        _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEXT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NULL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PLUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | QUESTION ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REPEAT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING_CONST _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | SYMBOL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TILDE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



