open Smtsyntax

open List

type smt2 = string

let smt2_of_smtvar : smtvar -> smt2 =
  fun var -> var

let smt2_of_smtconst : smtconst -> smt2 =
  fun const -> const

let smt2_of_smtlogic : smtlogic -> smt2 =
  fun logic ->
    match logic with
    | SmtLogALL -> "ALL"
    | SmtLogQFUF -> "QF_UF"
    | SmtLogQFLIA -> "QF_LIA"
    | SmtLogQFLRA -> "QF_LRA"
    | SmtLogQFNIA -> "QF_NIA"
    | SmtLogQFNRA -> "QF_NRA"
    | SmtLogQFLIRA -> "QF_LIRA"
    | SmtLogQFNIRA -> "QF_NIRA"

let rec smt2_of_smtsort : smtsort -> smt2 =
  fun sort ->
    match sort with
    | SmtSortInt -> "Int"
    | SmtSortFloat -> "Real"
    | SmtSortBitVec i -> "(_ BitVec " ^ string_of_int i ^ ")"
    | SmtSortArray (is, o) ->
        "(Array " ^ (String.concat " " (map smt2_of_smtsort is)) ^ " " ^
                    smt2_of_smtsort o ^ ")"
    | SmtSortBool -> "Bool"
    | SmtSortApp (v, ss) ->
        "(" ^ smt2_of_smtvar v ^ " " ^
              (String.concat " " (map smt2_of_smtsort ss)) ^ ")"

let rec smt2_of_smtexpr : smtexpr -> smt2 =
  fun smtexpr ->
    match smtexpr with
    | SmtVar v -> smt2_of_smtvar v
    | SmtIndVarInt (v, is) ->
        "(_ " ^ smt2_of_smtvar v ^ " " ^
          (String.concat " " (map string_of_int is)) ^ ")"
    | SmtIndVarVar (v1, v2) ->
        "(_ " ^ smt2_of_smtvar v1 ^ " " ^ smt2_of_smtvar v2 ^ ")"
    | SmtQualVarSort (v, s) ->
        "(as " ^ smt2_of_smtvar v ^ " " ^ smt2_of_smtsort s ^ ")"
    | SmtQualVarVar (v1, v2) ->
        "(as " ^ smt2_of_smtvar v1 ^ " " ^ smt2_of_smtvar v2 ^ ")"
    | SmtConst c -> smt2_of_smtconst c

    | SmtGt (e1, e2) ->
        "(> " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"

    | SmtGe (e1, e2) ->
        "(>= " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtLt (e1, e2) ->
        "(< " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtLe (e1, e2) ->
        "(<= " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtEq (e1, e2) ->
        "(= " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtNeq (e1, e2) ->
        "(not (= " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ "))"

    | SmtAnd (e1, e2) ->
        "(and " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtOr (e1, e2) ->
        "(or " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtNeg (e1) ->
        "(not " ^ smt2_of_smtexpr e1 ^ ")"
    | SmtImp (e1, e2) ->
        "(=> " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtIff (e1, e2) ->
        smt2_of_smtexpr (SmtAnd (SmtImp (e1, e2), SmtImp (e2, e1)))

    | SmtPlus (e1, e2) ->
        "(+ " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtSub (e1, e2) ->
        "(- " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtMult (e1, e2) ->
        "(* " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtDiv (e1, e2) ->
        "(/ " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtExp (e1, e2) -> failwith "smt2_of_expr: exponential not supported"
    | SmtMod (e1, e2) ->
        "(mod " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtRem (e1, e2) ->
        "(rem " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"

    | SmtArrGet (e1, e2) ->
        "(select " ^ smt2_of_smtexpr e1 ^ " " ^ smt2_of_smtexpr e2 ^ ")"
    | SmtArrSet (e1, e2, e3) ->
        "(store " ^ smt2_of_smtexpr e1 ^ " " ^
                    smt2_of_smtexpr e2 ^ " " ^
                    smt2_of_smtexpr e3 ^ ")"

    | SmtFunApp (f, es) ->
        "(" ^ smt2_of_smtvar f ^ " " ^
              (String.concat " " (map smt2_of_smtexpr es)) ^ ")"
    | SmtLet (bs, e) ->
      "(let (" ^
        (String.concat " "
          (map (fun (v, e) ->
          "(" ^ smt2_of_smtvar v ^ " " ^
                smt2_of_smtexpr e ^ ")") bs)) ^ ") " ^
        smt2_of_smtexpr e ^ ")"

    | SmtForAll (bs, e) ->
        "(forall (" ^
          (String.concat " "
            (map (fun (v, s) ->
              "(" ^ smt2_of_smtvar v ^ " " ^
                    smt2_of_smtsort s ^ ")") bs)) ^ ") " ^
          smt2_of_smtexpr e ^ ")"

    | SmtExists (bs, e) ->
        "(exists (" ^
          (String.concat " "
            (map (fun (v, s) ->
              "(" ^ smt2_of_smtvar v ^ " " ^
                    smt2_of_smtsort s ^ ")") bs)) ^ ") " ^
          smt2_of_smtexpr e ^ ")"

let rec smt2_of_smtcmd : smtcmd -> smt2 =
  fun stmt ->
    match stmt with
    | SmtSetLogic l ->
      "(set-logic " ^ smt2_of_smtlogic l ^ ")"

    | SmtDeclFun (v, vs, s) ->
      "(declare-fun " ^
          smt2_of_smtvar v ^ " " ^
          "(" ^ (String.concat " " (map smt2_of_smtsort vs)) ^ ") " ^
          smt2_of_smtsort s ^ ")"
    | SmtDefFun (v, vs, s, e) ->
      "(define-fun " ^
        smt2_of_smtvar v ^ " " ^
        "(" ^ (String.concat " "
              (map (fun (n, s) ->
                "(" ^ smt2_of_smtvar n ^ " " ^ smt2_of_smtsort s ^ ")")
                vs)) ^ ") " ^
        smt2_of_smtsort s ^ " " ^
        "(" ^ smt2_of_smtexpr e ^ "))"

    | SmtDeclSort (v, i) ->
      "(declare-sort " ^ smt2_of_smtvar v ^ " " ^ string_of_int i ^ ")"
    | SmtDefSort (v, vs, s) ->
      "(define-sort " ^ smt2_of_smtvar v ^ " " ^
          "(" ^ (String.concat " " (map smt2_of_smtvar vs)) ^ ") " ^
          smt2_of_smtsort s ^ ")"

    | SmtAssert e -> "(assert " ^ smt2_of_smtexpr e ^ ")"
    | SmtGetAsserts -> "(get-assertions)"

    | SmtCheckSat -> "(check-sat)"
    | SmtGetModel -> "(get-model)"
    | SmtGetProof -> "(get-proof)"
    | SmtGetUnsatCore -> "(get-unsat-core)"

    | SmtGetValue es ->
      "(get-value " ^ (String.concat " " (map smt2_of_smtexpr es)) ^ ")"
    | SmtGetAssignment -> "(get-assignment)"

    | SmtPush i -> "(push " ^ string_of_int i ^ ")"
    | SmtPop i -> "(pop " ^ string_of_int i ^ ")"

    | SmtGetOption v ->
      "(get-option :" ^ smt2_of_smtvar v ^ ")"
    | SmtSetOption (v, a) ->
      "(set-option :" ^ smt2_of_smtvar v ^ " " ^ smt2_of_smtconst a ^ ")"

    | SmtGetInfo v ->
      "(get-info :" ^ smt2_of_smtvar v ^ ")"
    | SmtSetInfo (v, a) ->
      "(set-info :" ^ smt2_of_smtvar v ^ " " ^ smt2_of_smtconst a ^ ")"

    | SmtExit -> "(exit)"

    | SmtSat -> "sat"
    | SmtUnsat -> "unsat"
    | SmtModel cs ->
        "(model " ^ (String.concat " " (map smt2_of_smtcmd cs)) ^ ")"

let smt2_of_smtcmd_list : smtcmd list -> smt2 =
  fun stmts ->
    String.concat "\n" (map smt2_of_smtcmd stmts)

let string_of_smt2 : smt2 -> string =
  fun smt2 -> smt2

