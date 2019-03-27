(* 
    rules.ml

    Defines types used in interpretation.
*)

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

(* Holds the result of an execution step. The rule list is a list of rules that were applied to
    reduce from the original program to this state, the "history" of this state. comps is the set
    of complete states: states where only a ReturnSlot is on the stack. errs is the set of error states,
    states where none of the rules could be applied. incomps is the set of states where a rule was
    successfully applied, but more computation must be done to produce a final result. *)
type passresult =
  { pass_comps : (rule list * state) list;
    pass_errs : (rule list * state) list;
    pass_incomps : (rule list * state) list }

(* Create an empty passresult. A function because otherwise each fresh_passresult would
    refer to the same underlying data. *)
let fresh_passresult : unit -> passresult =
  fun _ ->
    { pass_comps = [];
      pass_errs = [];
      pass_incomps = [] }

