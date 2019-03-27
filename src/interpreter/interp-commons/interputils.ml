(*
    interputils.ml

    Printing code for program state.
*)

open Langutils
open Smtutils
open Support
open Natives
open Rules

open String
open List

let rec range : int -> int -> int list =
  fun low high ->
    if low > high then
      []
    else
      low :: range (low + 1) high

let rec repeat : string -> int -> string =
  fun str n ->
    if n <= 0 then
      ""
    else if n = 1 then
      str
    else
      str ^ repeat str (n - 1)

let tab2 = repeat " " 2
let tab4 = tab2 ^ tab2
let tab8 = tab4 ^ tab4
let bar20 = repeat "----" 5
let bar40 = let b = bar20 in b ^ b
let bar60 = bar20 ^ bar40
let bar80 = let b = bar40 in b ^ b

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


let string_of_pathcons : pathcons -> string =
  fun path ->
    string_of_list_comma (map string_of_smtexpr path.path_list)


let string_of_rtype : rtype -> string =
  fun ty -> match ty with
    | RBool -> "RBool"
    | RInt -> "RInt"
    | RFloat -> "RFloat"
    | RComplex -> "RComplex"
    | RString -> "RString"

let rec string_of_symvec : symvec -> string =
  fun ((v, t, p), ds) ->
    "SymVec (" ^ string_of_smtvar v ^ "," ^
                 string_of_rtype t ^ "," ^
                 string_of_pathcons p ^ "," ^
                 string_of_symdepends ds ^ ")"

and string_of_symdepends : symdepends -> string =
  fun deps ->
    match deps with
    | NoDepends -> "NoDepends"
    | Depends syms -> string_of_list_comma (map string_of_symvec syms)

let string_of_rvector: rvector -> string =
    function
    | IntVec i ->
        string_of_list_comma (map string_of_rint (Array.to_list i))
    | FloatVec f ->
        string_of_list_comma (map string_of_rfloat (Array.to_list f))
    | ComplexVec c ->
        string_of_list_comma (map string_of_rcomplex (Array.to_list c))
    | StrVec s ->
        string_of_list_comma (map Langutils.string_of_rstring (Array.to_list s))
    | BoolVec b ->
        string_of_list_comma (map string_of_rbool (Array.to_list b))
    | SymVec ((i, t, p), ds) ->
        "(" ^ string_of_smtvar i ^ ";" ^
              string_of_rtype t ^ ";" ^
              string_of_pathcons p ^ ")"

let string_of_env: env -> string =
  fun env ->
    let binds = IdentMap.bindings env.id_map in
    let bind_strs = map (fun (i, m) ->
           string_of_pair (i, m) (string_of_id, string_of_mem)) binds in
    let pred_str = string_of_mem env.pred_mem in
        "Env (pred : " ^ pred_str ^ ") " ^
            "{" ^ (string_of_list_semicolon bind_strs) ^ "}"

let string_of_attributes: attributes -> string =
  fun attrs ->
    let strs = Hashtbl.fold (fun k v acc ->
       (string_of_pair (k, v) (Langutils.string_of_rstring, string_of_mem)) :: acc)
                 attrs.rstr_map [] in
      "Attrs {" ^ (string_of_list_semicolon strs) ^ "}"


let string_of_value: value -> string =
  function
    | Vec v -> "[" ^ (string_of_rvector v) ^ "]"
    | RefArray ms ->
        "MEMS[" ^ (string_of_list_comma (map string_of_mem ms)) ^ "]"
    | FuncVal (params, expr, mem) -> let mem_str = string_of_mem mem in
        let params_strs = map string_of_param params in
        let expr_str = string_of_expr expr in
          "Function (env : " ^ mem_str ^ ") " ^
                   "[" ^ string_of_list_comma params_strs ^ "] " ^
                   "{" ^ expr_str ^ "}"
    | EnvVal e -> string_of_env e
    (* | ListVal l ->
        let list_strs = map (fun mem -> string_of_mem mem) l in
          "List [" ^ string_of_list_comma list_strs ^ "]" *)

let string_of_heapobj: heapobj -> string =
  function
    | PromiseObj (e, m) ->
        "Promise (env : " ^ string_of_mem m ^ ") " ^
                "{" ^ string_of_expr e ^ "}"
    | DataObj (v, a) ->
        "Data (" ^ string_of_value v ^ ") " ^
             "(" ^ string_of_attributes a ^ ")"

let string_of_heap: heap -> string =
  fun heap ->
    let binds = MemRefMap.bindings heap.mem_map in
    let strs = map (fun (k, v) ->
          string_of_pair (k, v) (string_of_mem, string_of_heapobj)) binds in
    let mod_strs = map (fun s -> tab4 ^ s) strs in
      "Heap (next : " ^ string_of_mem heap.next_mem ^ ")\n" ^
                        string_of_list_newline mod_strs

let string_of_slot: slot -> string =
    function
    | ReturnSlot m -> "Return (" ^ string_of_mem m ^ ")"
    | UpdateSlot m -> "Update (" ^ string_of_mem m ^ ")"
    | EvalSlot e -> "Evaluate (" ^ string_of_expr e ^ ")"
    | SeqSlot es -> "Seq [" ^ string_of_list_semicolon
                              (map string_of_expr es) ^ "]"
    | ArgsSlot args -> "Args [" ^ string_of_list_comma
                                  (map string_of_arg args) ^ "]"
    | AttrSlot (mopt, expopt) ->
      let mstr = begin match mopt with
        | Some m -> string_of_mem m
        | None -> "no mem"
      end in
      let expstr = begin match expopt with
        | Some e -> string_of_expr e
        | None -> "no expr"
      end in
        "AttrSlot (" ^ mstr ^ "," ^ expstr ^ ")"
    | LoopSlot (e1, e2, mopt) ->
      let e1str = string_of_expr e1 in
      let e2str = string_of_expr e2 in
      let mstr = begin match mopt with
        | Some m -> string_of_mem m
        | None -> "no mem"
      end in
        "LoopSlot (" ^ e1str ^ "," ^ e2str ^ "," ^ mstr ^ ")"
    | BranchSlot (e1, e2) -> let e1str = string_of_expr e1 in
        let e2str = string_of_expr e2 in
        "Branch (" ^ e1str ^ "," ^ e2str ^ ")"

    | AssignSlot id ->
        "AssignSlot (" ^ string_of_id id ^ ")"
    | SupAssignSlot id ->
        "SupAssignSlot (" ^ string_of_id id ^ ")"
    | LambdaASlot (f_mem_opt, da_mems, a_opt, args) ->
        let f_str = match f_mem_opt with
                    | Some mem -> string_of_mem mem
                    | None -> "None" in
        let a_str = match a_opt with
                    | Some arg -> string_of_arg arg
                    | None -> "None" in
          "LambdaASlot (" ^ f_str ^ "," ^
                      "[" ^ string_of_list_comma
                            (map (fun p -> string_of_pair p
                                           (string_of_arg, string_of_mem))
                                 da_mems) ^ "]," ^
                      a_str ^ "," ^
                      "[[" ^
                        string_of_list_comma (map string_of_arg args) ^
                        "]]"
    | LambdaBSlot mem ->
          "LambdaBSlot (" ^ string_of_mem mem ^ ")"

let string_of_frame: frame -> string =
  fun frame ->
    let slot_str = string_of_slot frame.slot in
    let env_mem_str = string_of_mem frame.env_mem in
      "Frame (env : " ^ env_mem_str ^ ")\n" ^
              tab8 ^ tab2 ^ "" ^ slot_str ^ ""

let string_of_stack: stack -> string =
  fun s ->
    let frame_strs = List.map string_of_frame s.frame_list in
    let mod_strs = map (fun s -> tab4 ^ s) frame_strs in
      "Stack\n" ^ string_of_list_newline mod_strs

let string_of_state: state -> string =
  fun state ->
    let stack_str = string_of_stack state.stack in
    let heap_str = string_of_heap state.heap in
    let env_str = string_of_mem state.global_env_mem in
    (* let count = string_of_int state.fresh_count in *)
      "State " ^ "(global: " ^ env_str ^ ") " ^
                 "(" ^ string_of_int state.unique ^ ") "^
                 "(pred : " ^ string_of_int state.pred_unique ^ ")" ^
                 "\n" ^
      "\n" ^  tab2 ^ stack_str ^ "\n" ^
      "\n" ^ tab2 ^ heap_str ^ ""

let string_of_state_list : state list -> string =
  fun states ->
    string_of_list_newline (map string_of_state states)



let string_of_rule : rule -> string =
  fun rule -> match rule with
    | ERuleIdent -> "Ident"
    | ERuleMemRef -> "MemRef"
    | ERuleConst -> "Const"
    | ERuleSeq -> "Seq"
    | ERuleLambdaAbs -> "LambdaAbs"
    | ERuleLambdaAppEval -> "LambdaAppEval"
    | ERuleLambdaAppFuncRet -> "LambdaAppFuncRet"
    | ERuleLambdaAppArgsEval -> "LambdaAppArgsEval"
    | ERuleLambdaAppArgsRet -> "LambdaAppArgsRet"
    | ERuleLambdaAppEnter -> "LambdaAppEnter"
    | ERuleLambdaAppComplete -> "LambdaAppComplete"
    | ERuleNativeLambdaApp -> "NativeLambdaApp"
    | ERuleAssignIdEval -> "AssignIdEval"
    | ERuleAssignStrEval -> "AssignStrEval"
    | ERuleAssignRet -> "AssignRet"
    | ERuleIfEval -> "IfEval"
    | ERuleIfRet -> "IfRet"
    | ERuleIfRetSym -> "IfRetSym"
    | ERuleWhileEval -> "WhileEval"
    | ERuleWhileCondTrue -> "WhileCondTrue"
    | ERuleWhileCondFalse -> "WhileCondFalse"
    | ERuleWhileCondSym -> "WhileCondSym"
    | ERuleWhileBodyDone -> "WhileBodyDone"
    | ERuleBreak -> "Break"
    | ERuleNext -> "Next"
    | ERuleReturn -> "Return"
    | ERuleDiscard -> "Discard"
    | ERuleBlank -> "Blank"


let string_of_rule_list : rule list -> string =
  fun rules ->
    "[" ^ string_of_list_comma (map string_of_rule rules) ^ "]"

let string_of_hist : (rule list * state) -> string =
  fun (rules, state) ->
    string_of_rule_list rules ^ "\n" ^
    string_of_state state ^ ""

let string_of_passresult : passresult -> string =
  fun { pass_comps = comps; pass_errs = errs; pass_incomps = incomps } ->
    string_of_list_newline
      (map (fun c ->
            bar60 ^ "\n" ^
            "{Complete}:\n" ^ string_of_hist c ^ "\n>>> COMPLETE\n" ^
            bar60) comps) ^ "\n" ^

    string_of_list_newline
      (map (fun c ->
            bar60 ^ "\n" ^
            "{Error}:\n" ^ string_of_hist c ^ "\n>>> ERROR\n" ^
            bar60 ^ "") errs) ^ "\n" ^

    string_of_list_newline
      (map (fun c ->
            bar60 ^ "\n" ^
            "{Incomplete}:\n" ^ string_of_hist c ^ "\n>>> INCOMPLETE\n" ^
            bar60 ^ "") incomps) ^ ""

let string_of_passresult_list : passresult list -> string =
  fun passes ->
    let strs = (map string_of_passresult passes) in
    let mods = map (fun (s, i) ->
                       "(" ^ string_of_int i ^ ") " ^
                       repeat "####" 10 ^
                       "\n" ^ s ^ "\n" ^
                       repeat "^^^^" 10)
                   (combine strs (range 1 (length strs))) in
      string_of_list_newline mods


let string_of_passresult_list_first_complete : passresult list -> string =
  fun passes ->
    match filter (fun pr -> length pr.pass_comps > 0) passes with
    | [] -> "No completes yet"
    | (p :: _) -> string_of_passresult p


let string_of_value_attr_opt_pair : (value * attributes) option -> string =
  fun res -> match res with
    | None -> "No result yet, run for higher values of n"
    | Some (v, a) ->
        "value: " ^ string_of_value v ^ "\n" ^
        "attrs: " ^ string_of_attributes a ^ ""


