open Support

type rule =
  | ERuleIdent
  | ERuleMemRef
  | ERuleConst
  | ERuleSeq
  | ERuleLambdaAbs
  | ERuleLambdaAppEval
  | ERuleLambdaAppFuncRet
  | ERuleLambdaAppArgsEval
  | ERuleLambdaAppArgsRet
  | ERuleLambdaAppEnter
  | ERuleLambdaAppComplete
  | ERuleNativeLambdaApp
  | ERuleAssignIdEval
  | ERuleAssignStrEval
  | ERuleAssignRet
  | ERuleIfEval
  | ERuleIfRet
  | ERuleIfRetSym
  | ERuleWhileEval
  | ERuleWhileCondTrue
  | ERuleWhileCondFalse
  | ERuleWhileCondSym
  | ERuleWhileBodyDone
  | ERuleBreak
  | ERuleNext
  | ERuleReturn
  | ERuleDiscard
  | ERuleBlank

type passresult =
  { pass_comps : (rule list * state) list;
    pass_errs : (rule list * state) list;
    pass_incomps : (rule list * state) list }

let fresh_passresult : unit -> passresult =
  fun _ ->
    { pass_comps = [];
      pass_errs = [];
      pass_incomps = [] }

