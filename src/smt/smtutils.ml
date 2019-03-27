open Smtsyntax

open List

let string_of_pair : ('a * 'b) -> (('a -> string) * ('b -> string)) -> string =
  fun (a, b) (fa, fb) ->
    "(" ^ fa a ^ "," ^ fb b ^ ")"

let string_of_list_semicolon : string list -> string =
  fun strs -> String.concat ";" strs

let string_of_list_comma : string list -> string =
  fun strs -> String.concat "," strs

let string_of_list_newline : string list -> string =
  fun strs -> String.concat "\n" strs

let string_of_list : ('a -> string) -> 'a list -> string =
  fun f xs ->
    "[" ^ String.concat "," (List.map f xs) ^ "]"

let string_of_smtlogic : smtlogic -> string =
  fun logic ->
    match logic with
    | SmtLogALL -> "SmtLogAll"
    | SmtLogQFUF -> "SmtLogQFUF"
    | SmtLogQFLIA -> "SmtLogQFLIA"
    | SmtLogQFLRA -> "SmtLogQFLRA"
    | SmtLogQFNIA -> "SmtLogQFNIA"
    | SmtLogQFNRA -> "SmtLogQFNRA"
    | SmtLogQFLIRA -> "SmtLogQFLIRA"
    | SmtLogQFNIRA -> "SmtLogQFNIRA"

let string_of_smtvar : smtvar -> string =
  fun var -> var

let string_of_smtconst : smtconst -> string =
  fun const -> const

let rec string_of_smtsort : smtsort -> string =
  fun sort ->
    match sort with
    | SmtSortInt -> "SmtSortInt"
    | SmtSortFloat -> "SmtSortFloat"
    | SmtSortBool -> "SmtSortBool"
    | SmtSortBitVec i -> "SmtSortBitVec (" ^ string_of_int i ^ ")"
    | SmtSortArray (is, o) ->
        "SmtSortArray (" ^
          string_of_list_comma (map string_of_smtsort is) ^ "," ^
          string_of_smtsort o ^ ")"
    | SmtSortApp (v, ss) ->
        "SmtSortApp (" ^
          string_of_smtvar v ^ "," ^
          string_of_list_comma (map string_of_smtsort ss) ^ ")"

let rec string_of_smtexpr : smtexpr -> string =
  fun smtexpr ->
    match smtexpr with
    | SmtVar v -> "Var (" ^ string_of_smtvar v ^ ")"
    | SmtIndVarVar (v1, v2) ->
        "IndVarVar (" ^ string_of_smtvar v1 ^ " " ^ string_of_smtvar v2 ^ ")"
    | SmtIndVarInt (v, is) ->
        "IndVarInt (" ^ string_of_smtvar v ^ "," ^
            string_of_list_comma (map string_of_int is) ^ ")"
    | SmtQualVarSort (v, s) ->
        "QualVarSort (" ^ string_of_smtvar v ^ "," ^ string_of_smtsort s ^ ")"
    | SmtQualVarVar (v1, v2) ->
        "QualVarVar (" ^ string_of_smtvar v1 ^ "," ^ string_of_smtvar v2 ^ ")"
    | SmtConst c -> "Const (" ^ string_of_smtconst c ^ ")"

    | SmtGt (e1, e2) ->
        "Gt (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"

    | SmtGe (e1, e2) ->
        "Ge (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtLt (e1, e2) ->
        "Lt (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtLe (e1, e2) ->
        "Le (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtEq (e1, e2) ->
        "Eq (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtNeq (e1, e2) ->
        "Neq (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"

    | SmtAnd (e1, e2) ->
        "And (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtOr (e1, e2) ->
        "Or (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtNeg (e1) ->
        "Neg (" ^ string_of_smtexpr e1 ^ ")"
    | SmtImp (e1, e2) ->
        "Imp (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtIff (e1, e2) ->
        "SmtIff (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"

    | SmtPlus (e1, e2) ->
        "Plus (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtSub (e1, e2) ->
        "Sub (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtMult (e1, e2) ->
        "Mult (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtDiv (e1, e2) ->
        "Div (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtExp (e1, e2) ->
        "Exp (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtMod (e1, e2) ->
        "Mod (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtRem (e1, e2) ->
        "Rem (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"

    | SmtArrGet (e1, e2) ->
        "ArrGet (" ^ string_of_smtexpr e1 ^ "," ^ string_of_smtexpr e2 ^ ")"
    | SmtArrSet (e1, e2, e3) ->
        "ArrSet (" ^ string_of_smtexpr e1 ^ "," ^
                     string_of_smtexpr e2 ^ "," ^
                     string_of_smtexpr e3 ^ ")"

    | SmtFunApp (f, es) ->
        "FunApp (" ^ string_of_smtvar f ^ "," ^
                     string_of_list_comma (map string_of_smtexpr es) ^ ")"
    | SmtLet (bs, e) ->
        "Let (" ^ string_of_list_comma
                  (map (fun (v, e) ->
                    string_of_pair
                    (v, e) (string_of_smtvar, string_of_smtexpr)) bs) ^ "," ^
                  string_of_smtexpr e ^ ")"

    | SmtForAll (bs, e) ->
        "ForAll (" ^ string_of_list_comma
                      (map (fun (v, s) ->
                        string_of_pair (v, s)
                          (string_of_smtvar, string_of_smtsort)) bs) ^ "," ^
                     string_of_smtexpr e ^ ")"

    | SmtExists (bs, e) ->
        "Exists (" ^ string_of_list_comma
                      (map (fun (v, s) ->
                        string_of_pair (v, s)
                          (string_of_smtvar, string_of_smtsort)) bs) ^ "," ^
                     string_of_smtexpr e ^ ")"

let rec string_of_smtcmd : smtcmd -> string =
  fun stmt ->
    match stmt with
    | SmtSetLogic l ->
        "SmtSetLogic (" ^ string_of_smtlogic l ^ ")"

    | SmtDeclFun (f, ps, s) ->
        "SmtDeclFun (" ^
            string_of_smtvar f ^ "," ^
            string_of_list_comma (map string_of_smtsort ps) ^ "," ^
            string_of_smtsort s ^ ")"
    | SmtDefFun (f, ps, s, e) ->
        "SmtDefFun (" ^
            string_of_smtvar f ^ "," ^
            string_of_list_comma
              (map (fun (p, t) ->
                string_of_pair (p, t)
                  (string_of_smtvar, string_of_smtsort)) ps) ^ "," ^
            string_of_smtsort s ^ "," ^
            string_of_smtexpr e ^ ")"
    | SmtDeclSort (s, i) ->
        "SmtDeclSort (" ^ string_of_smtvar s ^ "," ^ string_of_int i ^ ")"
    | SmtDefSort (s, vs, so) ->
        "SmtDefSort (" ^
            string_of_smtvar s ^ "," ^
           (string_of_list_comma (map string_of_smtvar vs)) ^ "," ^
           string_of_smtsort so ^ ")"

    | SmtAssert e ->
        "SmtAssert (" ^ string_of_smtexpr e ^ ")"
    | SmtGetAsserts ->
        "SmtGetAsserts"

    | SmtCheckSat -> "SmtCheckSat"
    | SmtGetModel -> "SmtGetModel"
    | SmtGetProof -> "SmtGetProof"
    | SmtGetUnsatCore -> "SmtGetUnsatCore"

    | SmtGetValue es ->
      "SmtGetValue (" ^ string_of_list_comma (map string_of_smtexpr es) ^ ")"
    | SmtGetAssignment -> "SmtGetAssignment"

    | SmtPush i -> "SmtPush (" ^ string_of_int i ^ ")"
    | SmtPop i -> "SmtPop (" ^ string_of_int i ^ ")"

    | SmtGetOption v -> "SmtGetOption (" ^ string_of_smtvar v ^ ")"
    | SmtSetOption (v, c) ->
        "SmtSetOption (" ^ string_of_smtvar v ^ "," ^
                           string_of_smtconst c ^ ")"

    | SmtGetInfo v -> "SmtGetInfo (" ^ string_of_smtvar v ^ ")"
    | SmtSetInfo (v, c) ->
        "SmtSetInfo(" ^ string_of_smtvar v ^ "," ^
                           string_of_smtconst c ^ ")"


    | SmtExit -> "SmtExit"
    | SmtSat -> "SmtSat"
    | SmtUnsat -> "SmtUnsat"
    
    | SmtModel cs ->
        "SmtModel (" ^ (String.concat " " (map string_of_smtcmd cs)) ^ ")"

let string_of_smtprog : smtprog -> string =
  fun prog ->
    String.concat "\n" (map string_of_smtcmd prog)

