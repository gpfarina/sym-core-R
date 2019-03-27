(*
    native_calls.ml

    Registers native calls with the interpreter. When the interpreter sees a
    NativeLambdaApp, it first evaluates the arguments down to memory references
    like a normal function call, then calls the native_call function here to
    determine what the actual functionality is. The native_call function simply
    dispatches the arguments to the appropriate primitive function, and then
    pushes a return frame onto the execution stack that holds the return memory
    reference from the primitive function. This also handles things like named
    arguments and argument order.
*)
open Syntax
open Support
open Subscript
module U = Langutils
module C = Complex
open Natives
open Vector
open Arithmetic
open Convert

open List

type ident = Support.ident

let binop_names : string list =
  ["+"; "-"; "*"; "/"; "^";
   "%%"; "%/%"; "%*%"; "%*%"; "%x"; "%in%";
   ">"; ">="; "<"; "<="; "=="; "!=";
   "&&"; "&"; "||"; "|"; "~";
   "<-"; "<<-"; "@"; ":"; "?"; "::"; ":::";
   ]

let extract_native_name : ident -> string option =
  fun id -> 
    match id.pkg with
    | None -> None
    | Some pkg ->
      if pkg = native_rstring then
        string_of_rstring id.name
      else
        None

(* Basic form for every binary operation: 
 expect two arguments only, and put the result in a ReturnSlot.
 The only difference here is which op to do (possible ops in arithmetic.ml) *)
(* TODO: a little ugly to need to pass the env_mem... *)
let do_rvector_bop: (rvector -> rvector -> state -> rvector * state) -> 
        memref list -> memref -> state -> state option =
    fun op arg_mems c_env_mem state ->
    match arg_mems with
    | (v1 :: v2 :: []) ->
        let (mem2, state2) = vector_bop_mems op v1 v2 state in
        let c_frame = { frame_default with
                            env_mem = c_env_mem;
                            slot = ReturnSlot mem2 } in
        Some { state2 with stack = stack_push c_frame state.stack }
    | _ -> None

(* For rvector conversion. For now we do not allow explicit conversion of symbolic
    vectors, so the conversion function is rvector -> rvector. The conversion
    functions are found in native_support.ml.
*)
let do_rvector_conv: (rvector -> rvector) ->
        memref list -> memref -> state -> state option =
    fun op arg_mems c_env_mem state ->
    match arg_mems with
    | [v1] ->
        let (mem2, state2) = convert_vector_mems op v1 state in
        let c_frame = { frame_default with
                            env_mem = c_env_mem;
                            slot = ReturnSlot mem2 } in
        Some { state2 with stack = stack_push c_frame state.stack }
    | _ -> None

(* Matches the identifier used for the call to determine which native mems call it needs to perform
  and matches the memory addresses passed in to the arguments expected. Called when the evaluator sees
  a NativeLambdaApp. Fails when it can't recognize the identifier for the NativeLambdaApp - the function
  requested either doesn't exist or hasn't been registered (see interpreter/interp-commons/natives.ml),
  or when a different number of arguments are passed from expected. Notably does not dereference
  the arguments. 
  This will fail gracefully when the identifier did not match a known native call - it simply
  will not produce a state, and the interpreter will do whatever is default when it applied
  a production rule that didn't succeed. *)
let native_call : ident -> memref list -> memref -> state -> state option =
  fun id arg_mems c_env_mem state ->
    if id = native_is_numeric_id then
      (match arg_mems with
      | (val_mem :: []) ->
        let is_num = (match heap_find val_mem state.heap with
                     | Some (DataObj (value, _)) ->
                        (match value with
                        | Vec (IntVec _) -> 1
                        | Vec (FloatVec _) -> 1
                        | Vec (ComplexVec _) -> 1
                        | Vec (SymVec ((_, ty, _), _)) ->
                            (match ty with
                            | RInt -> 1
                            | RFloat -> 1
                            | RComplex -> 1
                            | _ -> 0)
                        | _ -> 0)
                     | _ -> 0) in
        let bool_const = Bool (rbool_of_bool is_num) in
        let (mem2, heap2) = heap_alloc_const bool_const state.heap in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state with 
                   heap = heap2;
                   stack = stack_push c_frame state.stack }
      | _ -> None)

    else if id = native_is_integer_id then
      (match arg_mems with
      | (val_mem :: []) ->
        let is_int = (match heap_find val_mem state.heap with
                     | Some (DataObj (value, _)) ->
                        (match value with
                        | Vec (IntVec _) -> 1
                        | Vec (SymVec ((_, ty, _), _)) ->
                            (match ty with
                            | RInt -> 1
                            | _ -> 0)
                        | _ -> 0)
                     | _ -> 0) in
        let bool_const = Bool (rbool_of_bool is_int) in
        let (mem2, heap2) = heap_alloc_const bool_const state.heap in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state with
                   heap = heap2;
                   stack = stack_push c_frame state.stack }
      | _ -> None)

    else if id = native_is_logical_id then
      (match arg_mems with
      | (val_mem :: []) ->
        let is_bool = (match heap_find val_mem state.heap with
                     | Some (DataObj (value, _)) ->
                        (match value with
                        | Vec (BoolVec _) -> 1
                        | Vec (SymVec ((_, ty, _), _)) ->
                            (match ty with
                            | RBool -> 1
                            | _ -> 0)
                        | _ -> 0)
                     | _ -> 0) in
        let bool_const = Bool (rbool_of_bool is_bool) in
        let (mem2, heap2) = heap_alloc_const bool_const state.heap in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state with
                   heap = heap2;
                   stack = stack_push c_frame state.stack }
      | _ -> None)

    else if id = native_is_character_id then
      (match arg_mems with
      | (val_mem :: []) ->
        let is_char = (match heap_find val_mem state.heap with
                     | Some (DataObj (value, _)) ->
                        (match value with
                        | Vec (StrVec _) -> 1
                        | Vec (SymVec ((_, ty, _), _)) ->
                            (match ty with
                            | RString -> 1
                            | _ -> 0)
                        | _ -> 0)
                     | _ -> 0) in
        let bool_const = Bool (rbool_of_bool is_char) in
        let (mem2, heap2) = heap_alloc_const bool_const state.heap in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state with
                   heap = heap2;
                   stack = stack_push c_frame state.stack }
      | _ -> None)

    else if id = native_is_complex_id then
      (match arg_mems with
      | (val_mem :: []) ->
        let is_comp = (match heap_find val_mem state.heap with
                     | Some (DataObj (value, _)) ->
                        (match value with
                        | Vec (ComplexVec _) -> 1
                        | Vec (SymVec ((_, ty, _), _)) ->
                            (match ty with
                            | RComplex -> 1
                            | _ -> 0)
                        | _ -> 0)
                     | _ -> 0) in
        let bool_const = Bool (rbool_of_bool is_comp) in
        let (mem2, heap2) = heap_alloc_const bool_const state.heap in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state with
                   heap = heap2;
                   stack = stack_push c_frame state.stack }
      | _ -> None)

    else if id = native_is_null_id then
      (match arg_mems with
      | (val_mem :: []) ->
        let is_null = (match heap_find val_mem state.heap with
                     | Some (DataObj (value, _)) ->
                        (match value with
                        | Vec (IntVec a) -> min 0 (Array.length a)
                        | Vec (FloatVec a) -> min 0 (Array.length a)
                        | Vec (ComplexVec a) -> min 0 (Array.length a)
                        | Vec (StrVec a) -> min 0 (Array.length a)
                        | Vec (BoolVec a) -> min 0 (Array.length a)
                        | _ -> 0) (* Missing symbolics? *)
                     | _ -> 0) in
        let bool_const = Bool (rbool_of_bool is_null) in
        let (mem2, heap2) = heap_alloc_const bool_const state.heap in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state with
                   heap = heap2;
                   stack = stack_push c_frame state.stack }
      | _ -> None)

    else if id = native_is_na_id then
      (match arg_mems with
      | (val_mem :: []) ->
        let is_na = (match heap_find val_mem state.heap with
                     | Some (DataObj (value, _)) ->
                        (match value with
                        | Vec (BoolVec a) -> min 0 (Array.length a)
                        | _ -> 0) (* Missing symbolics? *)
                     | _ -> 0) in
        let bool_const = Bool (rbool_of_bool is_na) in
        let (mem2, heap2) = heap_alloc_const bool_const state.heap in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state with
                   heap = heap2;
                   stack = stack_push c_frame state.stack }
      | _ -> None)

    (* Vector subscripting *)
    else if id = native_vector_subscript_id then
      (match arg_mems with
      | (vec_mem :: sub_mem :: []) ->
        let (mem2, state2) = subscript_mems vec_mem sub_mem state in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state2 with stack = stack_push c_frame state.stack }
      | _ -> None)

    (* Vector subsetting *)
    else if id = native_vector_subset_id then
      (match arg_mems with
      | (vec_mem :: var_mem :: []) ->
        (match heap_find var_mem state.heap with
        | Some (DataObj (RefArray refs, _)) ->
          let (mem2, state2) = subset_mems vec_mem refs state in
          let c_frame = { frame_default with
                            env_mem = c_env_mem;
                            slot = ReturnSlot mem2 } in
            Some { state2 with stack = stack_push c_frame state.stack }
        | _ -> None)
      | _ -> None)

    (* Vector creation *)
    else if id = native_vector_make_id then
      (match arg_mems with
      | (var_mem :: []) ->
          (match heap_find var_mem state.heap with
          | Some (DataObj (RefArray refs, _)) ->
            let (mem2, state2) = make_vector_mems var_mem state in
            let c_frame = { frame_default with
                              env_mem = c_env_mem;
                              slot = ReturnSlot mem2 } in
              Some { state2 with stack = stack_push c_frame state.stack }
          | _ -> None)
      | _ -> None)

    (* Vector length *)
    else if id = native_vector_length_id then
      (match arg_mems with
      | (data_mem :: []) ->
        let (mem2, state2) = rvector_length_mem data_mem state in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state2 with stack = stack_push c_frame state.stack }
      | _ -> None)

    else if id = native_vector_sum_id then
      (match arg_mems with
      | (vec_mem :: []) ->
        let sum = (match heap_find vec_mem state.heap with
            | Some (DataObj (Vec vec, _)) ->
              (match vec with
              | IntVec a ->
                  Int (Array.fold_left (fun acc a ->
                            match (acc, a) with
                            | (Some l, Some r) -> Some (l + r)
                            | (_ , None) -> None
                            | (None, _) -> None) (Some 0) a)

              | FloatVec a ->
                  Float (Array.fold_left (fun acc a ->
                            match (acc, a) with
                            | (Some l, Some r) -> Some (l +. r)
                            | (_ , None) -> None
                            | (None, _) -> None) (Some 0.0) a)

              | ComplexVec a ->
                  Complex (Array.fold_left (fun acc a ->
                            match (acc, a) with
                            | (Some l, Some r) -> Some (Complex.add l r)
                            | (_ , None) -> None
                            | (None, _) -> None) (Some Complex.zero) a)

              | BoolVec a ->
                  Int (Array.fold_left (fun acc a ->
                            match (acc, a) with
                            | (Some l, Some r) -> Some (l + r)
                            | (_ , None) -> None
                            | (None, _) -> None) (Some 0) a)

              | _ -> Int (Some 0))
            | _ -> Int (Some 0)) in
        let (mem2, heap2) = heap_alloc_const (Num sum) state.heap in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state with
                   heap = heap2;
                   stack = stack_push c_frame state.stack }
      | _ -> None)

    else if id = native_vector_colon_id then
      (match arg_mems with
      | (low_mem :: high_mem :: []) ->
        let (mem2, state2) = range_mems low_mem high_mem state in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state2 with stack = stack_push c_frame state.stack }
      | _ -> None)

    (* Vector binary operations *)
    else if id = native_vector_add_id then
       do_rvector_bop rvector_add arg_mems c_env_mem state
    else if id = native_vector_mul_id then
        do_rvector_bop rvector_mul arg_mems c_env_mem state
    else if id = native_vector_div_id then
        do_rvector_bop rvector_div arg_mems c_env_mem state
    else if id = native_vector_sub_id then
        do_rvector_bop rvector_sub arg_mems c_env_mem state
    else if id = native_vector_mod_id then
        do_rvector_bop rvector_mod arg_mems c_env_mem state
    else if id = native_vector_exp_id then
        do_rvector_bop rvector_exp arg_mems c_env_mem state
    else if id = native_vector_lt_id then
        do_rvector_bop rvector_lt arg_mems c_env_mem state
    else if id = native_vector_gt_id then
        do_rvector_bop rvector_gt arg_mems c_env_mem state
    else if id = native_vector_eq_id then
        do_rvector_bop rvector_eq arg_mems c_env_mem state
    else if id = native_vector_neq_id then
        do_rvector_bop rvector_neq arg_mems c_env_mem state
    else if id = native_vector_geq_id then
        do_rvector_bop rvector_geq arg_mems c_env_mem state
    else if id = native_vector_leq_id then
        do_rvector_bop rvector_leq arg_mems c_env_mem state
    else if id = native_vector_andvec_id then
        do_rvector_bop rvector_andvec arg_mems c_env_mem state
    else if id = native_vector_and_id then
        do_rvector_bop rvector_and arg_mems c_env_mem state
    else if id = native_vector_orvec_id then
        do_rvector_bop rvector_orvec arg_mems c_env_mem state
    else if id = native_vector_or_id then
        do_rvector_bop rvector_or arg_mems c_env_mem state
    else if id = native_vector_xor_id then
        do_rvector_bop rvector_xor arg_mems c_env_mem state

    (* make.symbolic, for creating a symbolic vector. *)
    else if id = native_vector_make_symbolic_id then
      (match arg_mems with
      | (len_mem :: type_mem :: []) ->
        let (mem2, state2) =  make_symbolic_mems len_mem type_mem state in
        let c_frame = { frame_default with
                          env_mem = c_env_mem;
                          slot = ReturnSlot mem2 } in
          Some { state2 with stack = stack_push c_frame state.stack }
      | _ -> None)

    else if id = native_vector_intconv_id then
        do_rvector_conv rvector_as_integer arg_mems c_env_mem state
    else if id = native_vector_floatconv_id then
        do_rvector_conv rvector_as_float arg_mems c_env_mem state
    else if id = native_vector_complexconv_id then
        do_rvector_conv rvector_as_complex arg_mems c_env_mem state
    else if id = native_vector_strconv_id then
        do_rvector_conv rvector_as_string arg_mems c_env_mem state
    else if id = native_vector_boolconv_id then
        do_rvector_conv rvector_as_bool arg_mems c_env_mem state
        
    (* Oh no! *)
    else
      None

