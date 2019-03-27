
open List
open Syntax
open Support
open Natives
open Copy

type rule =
  | RuleForceP
  | RuleForceF
  | RuleGetF
  | RuleInvF
  | RuleNativeInvF

  | RuleConst
  | RuleFunc
  | RuleFind
  | RuleGetP

  | RuleUpdate
  | RuleAssId
  | RuleDAss
  | RuleAssStr
  | RuleDAssStr

  | RuleIfEval
  | RuleIfRet
  | RuleWhileEval
  | RuleWhileCondTrue
  | RuleWhileCondFalse
  | RuleWhileBodyDone
  | RuleBreak
  | RuleNext

  | RuleSeq
  | RuleObjAttrEval
  | RuleObjAttrObjRet
  | RuleObjAttrObjGet


let id_variadic : ident =
  id_of_rstring (rstring_of_string "...")

let pair_first : 'a * 'b -> 'a =
  fun (a, b) -> a

type ('a , 'b) either =
    OptA of 'a
  | OptB of 'b

let rec split_eithers : (('a , 'b) either) list -> ('a list) * ('b list) =
  fun eithers ->
    match eithers with
    | [] -> ([], [])
    | (OptA a :: e_tl) ->
        let (oas, obs) = split_eithers e_tl in
          (a :: oas, obs)
    | (OptB b :: e_tl) ->
        let (oas, obs) = split_eithers e_tl in
          (oas, b :: obs)

let rec pull_args :
  arg list -> env -> heap -> ((expr, (ident * expr)) either) list =
  fun args env heap ->
    match args with
    | [] -> []
    | (Arg expr :: a_tl) -> OptA expr :: pull_args a_tl env heap
    | (Named (id, expr) :: a_tl) -> OptB (id, expr) :: pull_args a_tl env heap
    | (VarArg :: a_tl) ->
      match env_find id_variadic env heap with
      | None -> []
      | Some mem ->
        match heap_find mem heap with
        | Some (DataObj (RefArray v_mems, _)) ->
            let mem_args = map (fun m -> OptA (MemRef m)) v_mems in
              mem_args @ pull_args a_tl env heap
        | _ -> [] (* WRONG STUFF HERE *)

let rec ids_contains_id : ident list -> ident -> bool =
  fun ids id ->
    match ids with
    | [] -> false
    | (hd :: ids_tl) -> hd.name = id.name || ids_contains_id ids_tl id

let rec remove_used_params : param list -> ident list -> param list =
  fun params args ->
    match params with
    | [] -> []
    | (Param id :: tail) ->
        if ids_contains_id args id
          then remove_used_params tail args
          else Param id :: remove_used_params tail args
    | (Default (id, expr) :: tail) ->
        if ids_contains_id args id
          then remove_used_params tail args
          else Default (id, expr) :: remove_used_params tail args
    | (VarParam :: tail) -> VarParam :: remove_used_params tail args

let rec get_default_params : param list -> (ident * expr) list =
  fun params ->
    match params with
    | [] -> []
    | (Param _ :: tail) -> get_default_params tail
    | (VarParam :: tail) -> get_default_params tail
    | (Default (id, expr) :: tail) -> (id, expr) :: get_default_params tail

let rec match_expr_args :
  param list -> expr list -> (ident * expr) list * (expr list) =
  fun params args ->
    match (params, args) with
    | ([], _) -> ([], []) (* OH NO?? *)
    | (_, []) -> (get_default_params params, [])
    | (VarParam :: tail, _) -> (get_default_params tail, args)
    | (Param id :: p_tail, arg :: a_tail) ->
        let (binds, vars) = match_expr_args p_tail a_tail in
          ((id, arg) :: binds, vars)
    | (Default (id, _) :: p_tail, arg :: a_tail) ->
        let (binds, vars) = match_expr_args p_tail a_tail in
          ((id, arg) :: binds, vars)


(* Oh god I really hope this function works, I've spent too much time here *)
let match_lambda_app :
  param list -> arg list -> env -> heap -> (ident * expr) list * expr list =
  fun params args env heap ->
    let (expr_args, nameds) = split_eithers (pull_args args env heap) in
    let un_params = remove_used_params params (map pair_first nameds) in
    let (positionals, variadics) = match_expr_args un_params expr_args in
      (nameds @ positionals, variadics)

let lift_var_bind :
  expr list -> memref -> memref -> heap -> (memref * heap) option =
  fun exprs expr_env_mem inj_env_mem heap ->
    let proms = map (fun e -> PromiseObj (e, expr_env_mem)) exprs in
    let (mems, heap2) = heap_alloc_list proms heap in
    let data = DataObj (RefArray mems, attrs_empty ()) in
    let (d_mem, heap3) = heap_alloc data heap2 in
      match env_mem_add id_variadic d_mem inj_env_mem heap3 with
        | None -> None
        | Some heap4 -> Some (d_mem, heap4)

let lift_binds :
  (ident * expr) list -> memref -> memref -> heap ->
  ((ident * memref) list * heap) option =
  fun binds expr_env_mem inj_env_mem heap ->
    let pairs1 =
        map (fun (b, e) -> (b, PromiseObj (e, expr_env_mem))) binds in
    let (pairs2, heap2) =
        fold_left (fun (acc, hp) (b, p) ->
                          let (m, hp2) = heap_alloc p hp in
                            ((b, m) :: acc, hp2))
                       ([], heap) pairs1 in
      match env_mem_add_list pairs2 inj_env_mem heap2 with
        | None -> None
        | Some heap3 -> Some (pairs2, heap3)

let is_mem_true : memref -> heap -> bool =
  fun mem heap -> true

let is_mem_val : memref -> heap -> bool =
  fun mem heap ->
    match heap_find mem heap with
    | Some(DataObj _) -> true
    | _-> false

let rec unwind_to_loop_slot :
  stack -> (expr * expr * memref * memref * stack) option =
  fun stack ->
    match stack_pop_v stack with
    | Some (LoopSlot (cond, body, o_body_mem), env_mem, stack2) ->
        let body_mem = (match o_body_mem with
                         | None -> mem_null
                         | Some mem -> mem) in
          Some (cond, body, body_mem, env_mem, stack2)
    | Some (_, _, stack2) -> unwind_to_loop_slot stack2
    | None -> None

let rec pull_all_ids : ident list -> env -> heap -> (memref list) option =
  fun ids env heap ->
    match ids with
    | [] -> Some []
    | (id :: ids_tl) ->
        match (env_find id env heap, pull_all_ids ids_tl env heap) with
          | (Some mem, Some mems_tl) -> Some (mem :: mems_tl)
          | _ -> None

(* Double arrow reduction relations (cf Fig 3) *)


(* Promise Evaluation *)
let rule_ForceP : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (MemRef p_mem), c_env_mem, c_stack2) ->
        (match heap_find p_mem state.heap with
          | Some (PromiseObj (p_expr, p_env_mem)) ->
              let p_frame = { frame_default with
                                env_mem = p_env_mem;
                                slot = EvalSlot p_expr } in
              let c_frame = { frame_default with
                                slot = UpdateSlot p_mem } in
              [{ state with
                        stack = stack_push_list [p_frame; c_frame] c_stack2}]
          | _ -> [])
    | _ -> []


(* Force function *)
let rule_ForceF : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (LambdaApp (fn, args)), c_env_mem, c_stack2) ->
        let f_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = EvalSlot fn } in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ArgsSlot args } in
          [{ state with
                    stack = stack_push_list [f_frame; c_frame] c_stack2 }]
    | _ -> []


(* Technically this rule doesn't even ... matter???
   Because we ``return'' everything through a pointer anyways *)
let rule_GetF : state -> state list =
  fun state ->
    []


let rule_InvF : state -> state list =
  fun state ->
    match stack_pop_v2 state.stack with
    | Some (ReturnSlot f_mem, _,
            ArgsSlot args, c_env_mem,
            c_stack2) -> (match heap_find f_mem state.heap with
      | Some (DataObj (FuncVal (pars, body, f_env_mem), _)) ->
        (match heap_find c_env_mem state.heap with
        | None -> []
        | Some (DataObj (EnvVal cenv, _)) ->
          let (binds, vares) = match_lambda_app pars args cenv state.heap in
          (match lift_var_bind vares c_env_mem f_env_mem state.heap with
          | None -> []
          | Some (vmem, heap2) ->
            (match lift_binds binds c_env_mem f_env_mem heap2 with
            | None -> []
            | Some (bmems, heap3) ->
                let c_frame = { frame_default with
                                  env_mem = f_env_mem;
                                  slot = EvalSlot body } in
                  [{ state with
                            heap = heap3;
                            stack = stack_push c_frame c_stack2 }])))
      | _ -> [])
    | _ -> []


let rule_NativeInvF : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (NativeLambdaApp (f_id, arg_ids)), c_env_mem, c_stack2) ->
      (match heap_find c_env_mem state.heap with
      | Some (DataObj (EnvVal env, _)) ->
        (match pull_all_ids arg_ids env state.heap with
        | Some mems ->
          (match native_call f_id mems env
                             { state with stack = c_stack2 } with
          | Some state -> [state]
          | None -> [])
        | _ -> [])
      | _ -> [])
    | _ -> []


(* Single arrow expression manipulations (cf Fig 5) *)

let rule_Const : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (Const const), _, c_stack2) ->
        let (const_mem, heap2) = heap_alloc_const const state.heap in
        let c_frame = { frame_default with
                          slot = ReturnSlot const_mem } in
          [{ state with
                    heap = heap2;
                    stack = stack_push c_frame c_stack2 }]
    | _ -> []

(* Function Definition *)
let rule_Func : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (LambdaAbs (params, expr)), c_env_mem, c_stack2) ->
        let data = DataObj (FuncVal (params, expr, c_env_mem), attrs_empty ()) in
        let (f_mem, heap2) = heap_alloc data state.heap in
        let c_frame = { frame_default with
                          slot = ReturnSlot f_mem } in
          [{ state with
                    heap = heap2;
                    stack = stack_push c_frame c_stack2 }]
    | _ -> []


(* Symbol. Actual search code in language/support.ml *)
let rule_Find : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (Ident id), c_env_mem, c_stack2) ->
        (match env_mem_find id c_env_mem state.heap with
          | None -> []
          | Some mem ->
              let c_frame = { frame_default with
                                slot = EvalSlot (MemRef mem) } in
                [{ state with
                          stack = stack_push c_frame c_stack2 }])
    | _ -> []


(* Promise Indirection: shortcut when a promise pointer points to a pointer *)
let rule_GetP : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (MemRef p_mem), c_frame2, c_stack2) ->
        (match heap_find p_mem state.heap with
          | Some (PromiseObj (MemRef p_mem2, _)) ->
              let c_frame = { frame_default with
                                slot = EvalSlot (MemRef p_mem2) } in
                [{ state with
                          stack = stack_push c_frame c_stack2 }]
          | _ -> [])
    | _ -> []

(* Updates *)
let rule_Update : state -> state list =
  fun state ->
    match stack_pop_v2 state.stack with
    | Some (ReturnSlot ret_mem, _,
            UpdateSlot upd_mem, c_env_mem,
            c_stack2) ->
      let (cpy_mem, heap2) = deep_copy ret_mem state.heap in
        (match heap_find cpy_mem heap2 with
         | Some hobj ->
            [{ state with
                      heap = heap_add upd_mem hobj heap2 }]
         | _ -> [])
    | _ -> []


(* Assignment *)
let rule_AssId : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (Assign (Ident id, expr)), c_env_mem, c_stack2) ->
      let prom = PromiseObj (expr, c_env_mem) in
      let (p_mem, heap2) = heap_alloc prom state.heap in
      let (u_mem, heap3) = heap_alloc_const Nil heap2 in
        (match env_mem_add id u_mem c_env_mem heap3 with
        | None -> []
        | Some heap4 ->
          let p_frame = { frame_default with
                            slot = EvalSlot (MemRef p_mem) } in
          let c_frame = { frame_default with
                            slot = UpdateSlot u_mem } in
            [{ state with
                      heap = heap4;
                      stack = stack_push_list [p_frame; c_frame] c_stack2 }])
    | _ -> []


(* Super assignment *)
let rule_DAss : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (Assign (Ident id, expr)), c_env_mem, c_stack2) ->
      let prom = PromiseObj (expr, c_env_mem) in
      let (p_mem, heap2) = heap_alloc prom state.heap in
      let (u_mem, heap3) = heap_alloc_const Nil heap2 in
        (match heap_find c_env_mem heap3 with
        | Some (DataObj (EnvVal env, _)) ->
          (match env_mem_add id u_mem env.pred_mem heap3 with
          | None -> []
          | Some heap4 ->
            let p_frame = { frame_default with
                              slot = EvalSlot (MemRef p_mem) } in
            let c_frame = { frame_default with
                              slot = UpdateSlot u_mem } in
            [{ state with
                      heap = heap4;
                      stack = stack_push_list [p_frame; c_frame] c_stack2 }])
        | _ -> [])
    | _ -> []


let rule_AssStr : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (Assign (Const (Str rstr), expr)),
            c_env_mem, c_stack2) ->
        let c_slot =
            EvalSlot (Assign (Ident { id_default with name = rstr }, expr)) in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = c_slot } in
          [{ state with
                    stack = stack_push c_frame c_stack2 }]
    | _ -> []

let rule_DAssStr : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (SuperAssign (Const (Str rstr), expr)),
            c_env_mem, c_stack2) ->
        let c_slot =
            EvalSlot (SuperAssign (Ident { id_default with
                                             name = rstr }, expr)) in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = c_slot } in
          [{ state with
                    stack = stack_push c_frame c_stack2 }]
    | _ -> []

let rule_IfEval : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (If (c_expr, t_expr, f_expr)), c_env_mem, c_stack2) ->
        let t_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = EvalSlot c_expr } in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = BranchSlot (t_expr, f_expr) } in
          [{ state with
                    stack = stack_push_list [t_frame; c_frame] c_stack2 }]
    | _ -> []

let rule_IfRet : state -> state list =
  fun state ->
    match stack_pop_v2 state.stack with
    | Some (ReturnSlot mem, _,
            BranchSlot (t_expr, f_expr), c_env_mem,
            c_stack2) ->
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = if is_mem_true mem state.heap then
                                   EvalSlot t_expr
                                 else
                                   EvalSlot f_expr } in
          [{ state with
                    stack = stack_push c_frame c_stack2 }]
    | _ -> []

let rule_WhileEval : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (While (cond, body)), c_env_mem, c_stack2) ->
        let d_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = EvalSlot cond } in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = LoopSlot (cond, body, Some mem_null) } in
        [{ state with
                  stack = stack_push_list [d_frame; c_frame] c_stack2 }]
    | _ -> []

let rule_WhileCondTrue : state -> state list =
  fun state ->
    match stack_pop_v2 state.stack with
    | Some (ReturnSlot cond_mem, _,
            LoopSlot (cond, body, Some body_mem), c_env_mem,
            c_stack2) ->
        if is_mem_true cond_mem state.heap then
          let b_frame = { frame_default with
                            env_mem = c_env_mem;
                            slot = EvalSlot body } in
          let c_frame = { frame_default with
                            env_mem = c_env_mem;
                            slot = LoopSlot (cond, body, None) } in
            [{ state with
                      stack = stack_push_list [b_frame; c_frame] c_stack2 }]
        else
          []
    | _ -> []

let rule_WhileCondFalse : state -> state list =
  fun state ->
    match stack_pop_v2 state.stack with
    | Some (ReturnSlot cond_mem, _,
            LoopSlot (cond, body, Some body_mem), c_env_mem,
            c_stack2) ->
        if is_mem_true cond_mem state.heap then
          []
        else
          let c_frame = { frame_default with
                            env_mem = c_env_mem;
                            slot = ReturnSlot body_mem } in
            [{ state with
                      stack = stack_push c_frame c_stack2 }]
    | _ -> []

let rule_WhileBodyDone : state -> state list =
  fun state ->
    match stack_pop_v2 state.stack with
    | Some (ReturnSlot body_mem, _,
            LoopSlot (cond, body, None), c_env_mem,
            c_stack2) ->
        let d_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = EvalSlot cond } in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = LoopSlot (cond, body, Some body_mem) } in
          [{ state with
                    stack = stack_push_list [d_frame; c_frame] c_stack2 }]
    | _ -> []

let rule_Break : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot Break, _, c_stack2) ->
      (match unwind_to_loop_slot c_stack2 with
        | Some (_, _, body_mem, l_env_mem, c_stack3) ->
            let c_frame = { frame_default with
                              env_mem = l_env_mem;
                              slot = ReturnSlot body_mem } in
              [{ state with
                        stack = stack_push c_frame c_stack3 }]
        | _ -> [])
    | _ -> []

let rule_Next : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot Next, _, c_stack2) ->
      (match unwind_to_loop_slot c_stack2 with
        | Some (cond, body, body_mem, l_env_mem, c_stack3) ->
            let b_frame = { frame_default with
                              env_mem = l_env_mem;
                              slot = EvalSlot body } in
            let c_frame = { frame_default with
                              env_mem = l_env_mem } in
              [{ state with
                        stack = stack_push_list [b_frame; c_frame] c_stack2 }]
        | _ -> [])
    | _ -> []

let rule_Seq : state -> state list =
  fun state ->
    match stack_pop_v state.stack with
    | Some (EvalSlot (Seq exprs), c_env_mem, c_stack2) ->
        if List.length exprs = 0 then
          let c_frame = { frame_default with
                            env_mem = c_env_mem;
                            slot = ReturnSlot mem_null; } in
            [{ state with
                      stack = stack_push c_frame c_stack2 }]
        else
          let c_frames = List.map (fun e -> { frame_default with
                                                env_mem = c_env_mem;
                                                slot = EvalSlot e }) exprs in
            [{ state with
                      stack = stack_push_list c_frames c_stack2 }]
    | _ -> []



let rule_table : (rule * (state -> state list)) list =
  [ (RuleForceP, rule_ForceP);
    (RuleForceF, rule_ForceF);
    (RuleGetF, rule_GetF);
    (RuleInvF, rule_InvF);
    (RuleNativeInvF, rule_NativeInvF);

    (RuleConst, rule_Const);
    (RuleFunc, rule_Func);
    (RuleFind, rule_Find);
    (RuleGetP, rule_GetP);

    (RuleUpdate, rule_Update);
    (RuleAssId, rule_AssId);
    (RuleDAss, rule_DAss);
    (RuleAssStr, rule_AssStr);
    (RuleDAssStr, rule_DAssStr);

    (RuleIfEval, rule_IfEval);
    (RuleIfRet, rule_IfRet);
    (RuleWhileEval, rule_WhileEval);
    (RuleWhileCondTrue, rule_WhileCondTrue);
    (RuleWhileBodyDone, rule_WhileBodyDone);
    (RuleBreak, rule_Break);
    (RuleNext, rule_Next);

    (RuleSeq, rule_Seq);
    ]


