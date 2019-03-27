(*
    symbolic_ops.ml

    Defines methods for dealing with symbolic rvectors.

    This file includes symbolic arithmetic, as well as other functions that
    are used to combine symbolic vectors, like concatenation, conversion
    from concrete values, etc. For now, a symbolic combine should assume that
    inputs are correct. That is, division by zero should add a path constraint
    that the values of the denominator are all nonzero. Another paradigm for
    dealing with this is to produce a "good" state with these assumptions, and
    an error state with the negation of them, but we are less interested in
    checking for unsatisfiability of error states and more about proofs
    over valid inputs for now, so the first approach is better.
   
    A note about symbolic vectors:
    The way that we handle symbolic vectors is somewhat complicated.
    To output Smt2Lib code, we need to produce a list at the end of every
    symbolic value. To do this, the state keeps a list of every symbolic 
    value in the heap. Symbolic values are never deallocated because a new
    symbolic vector's definition may depend on an old symbolic value.
    The state_alloc function which allocates new memory also ensures that if
    the object you're allocating is symbolic, it is registered as such.

    What about "implicit" symbolic vectors, though? The most common case where
    this comes up is in an operation like x + 3. For us, we implicitly coerce
    the vector 3 to a symbolic vector which is constrained to have the same
    values, length, type, etc. Then, we do a symbolic addition between these
    vectors, and get back a definition for a new symbolic vector. This
    definition relies on the (already allocated) x, and the (unallocated)
    symbolic 3. Instead of requiring that the function that created sym3 also
    be responsible for allocating it, symbolic vectors come with a dependency
    list of vector definitions. When creating a symbolic vector, there are
    two choices: make a true S.rvector, in which case that vector must be
    allocated, or leave it as an S.symvec, in which case it must appear inside
    the dependencies of an allocated vector.
    This scheme is complex, but it reduces the possibility of writing code that
    accidentally creates symbolic values and never allocates them. This is
    because there is an explicit type difference between the kind of symbolic
    vector which can (and must) be allocated, and the kind of symbolic vector
    which is implicit and should be a dependency for some allocated vector.

    That's why the functions in this file produce symvecs or symdefs;
    the actual native call is where the logic is that decides which
    vectors to allocate.
*)
(* TODO: need to continue to add assumptions for valid inputs.
  Look at vector.ml, subscript.ml, etc. for which kinds of inputs are valid. *)
module S = Support
module C = Native_support
open Smtsyntax
open Smttrans

(* Creates a new unallocated symbolic vector with the name var *)
let vec_to_named_sym: smtvar -> S.rvector ->  S.symvec =
    fun var vec ->
    match vec with
    | S.IntVec i ->
        (* Enforce Get var i = x[i] for all relevant i *)
        let gets = Array.to_list (Array.mapi (fun index n -> 
            SmtEq (
                smt_getn var index,
                smt_int_const n)) (C.resolve_vec i)) in
        (* len(var) = length(x) *)
        let len_constraint = smt_len_array var i in
        ((var, S.RInt, { S.path_list = [len_constraint] @ gets }), S.NoDepends)
    | S.FloatVec f ->
        let gets = Array.to_list (Array.mapi (fun index n ->
            SmtEq (
                smt_getn var index,
                smt_float_const n)) (C.resolve_vec f)) in
        let len_constraint = smt_len_array var f in
        ((var, S.RFloat, { S.path_list = [len_constraint] @ gets }), S.NoDepends)
    | S.ComplexVec c -> failwith "Symbolic complex vectors not implemented"
    | S.StrVec s -> failwith "Symbolic string vectors not implemented"
    | S.BoolVec b ->
        let gets = Array.to_list (Array.mapi (fun index n ->
            SmtEq (
                smt_getn var index,
                smt_rbool_const n)) (C.resolve_vec b)) in
        let len_constraint = smt_len_array var b in
        ((var, S.RBool, { S.path_list = [len_constraint] @ gets }), S.NoDepends)
    | S.SymVec ((_, _, _), _) -> failwith "Vector is already symbolic"

(* For makes a symbolic rvector from a concrete rvector. *)
let vec_to_symvec: S.rvector -> S.state -> (S.symvec * S.state) =
    fun vec state ->
    let name, state' = S.name_fresh state in
    let sy = vec_to_named_sym name vec in
    (sy, state')

(* TODO: refactor vector.ml to use this in cases where it would be used.
 TODO: This is not be the most efficient implementation of some cases. For
 example, when doing the computation x[7] with symbolic x, this function will
 convert 7 to a symbolic vector and do the symbolic computation, rather than just
 creating a new symbolic vector z with the path constraints that
    z_len = 1
    Select z 0 = Select x 7 
*)
(* Takes symbolic definition creation function and applies it to a list of its arguments
  as rvectors. The result is an rvector whose dependencies are the arguments that were
  be converted from concrete to symbolic. *)
let sym_op: (smtvar -> S.symvec list -> S.symdef) ->
        S.rvector list -> S.state -> (S.rvector * S.state) =
    fun smt_op args state ->
    let (sym_args, state') = S.state_map (fun arg state ->
        match arg with
        | S.SymVec sy -> (sy, state)
        | _ -> vec_to_symvec arg state) args state in
    (* Generate a list of lists  which is either empty or sym_list[i] depending on
    whether or not that arg had to be converted. *)
    let depends_lists = List.mapi (fun i a -> 
        match a with
        | S.SymVec _ -> []
        | _ -> [List.nth sym_args i]) args in
    let depends_list = List.fold_left (@) [] depends_lists in
    let depends = match depends_list with
    | [] -> S.NoDepends
    | _ -> S.Depends depends_list in
    let newvec_name, state'' = S.name_fresh state' in
    let newvec_def = smt_op newvec_name sym_args in
    (S.SymVec (newvec_def, depends), state'')

(* Produces an smt_op compatible with symbolize for a function with two arguments *)
let arg2listize: (smtvar -> S.symvec -> S.symvec -> S.symdef) ->
        smtvar -> S.symvec list -> S.symdef =
    fun f name args ->
    match args with
    | [arg1;arg2] -> f name arg1 arg2
    | _ -> failwith "Wrong number of arguments"

(* "" for a function with three arguments *)
let arg3listize: (smtvar -> S.symvec -> S.symvec -> S.symvec -> S.symdef) ->
        smtvar -> S.symvec list -> S.symdef =
    fun f name args ->
    match args with
    | [arg1;arg2;arg3] -> f name arg1 arg2 arg3
    | _ -> failwith "Wrong number of arguments"

(* When combining two vectors, make sure that their types match and
    return a new type for the new vector. *)
let match_tys: S.rtype -> S.rtype -> S.rtype =
    fun ty1 ty2 ->
    match (ty1, ty2) with
    | (S.RInt, S.RInt) -> S.RInt
    | (S.RFloat, S.RFloat) -> S.RFloat
    | (S.RComplex, S.RComplex) -> S.RComplex
    | (S.RString, S.RString) -> S.RString
    | (S.RBool, S.RBool) -> S.RBool
    | (_, _) -> failwith "Incompatible symbolic types"

(* Sum across the vector *)
let symbolic_sum: smtvar -> S.symvec -> S.symdef =
    fun new_name ((name, ty, _),_) ->
    let new_len = lengthn new_name 1 in
    let name_sum = match ty with
    | S.RInt -> all_elements name (SmtEq (
        SmtFunApp ("sym_vec_int_sum", [SmtVar name; SmtVar forall_var]),
        SmtPlus (SmtArrGet (SmtVar name, SmtVar forall_var),
            SmtFunApp ("sym_vec_int_sum",
                [SmtVar name;
                    (SmtSub (SmtVar forall_var, smt_int_const 1))]))))
    | S.RFloat -> all_elements name (SmtEq (
        SmtFunApp ("sym_vec_float_sum", [SmtVar name; SmtVar forall_var]),
        SmtPlus (SmtArrGet (SmtVar name, SmtVar forall_var),
            SmtFunApp ("sym_vec_float_sum",
                [SmtVar name;
                    (SmtSub (SmtVar forall_var, smt_int_const 1))]))))
    | _ -> failwith "Invalid type for symbolic sum" in
    let name_sum_base_case = match ty with
    | S.RInt -> SmtEq (
        SmtFunApp ("sym_vec_int_sum", [SmtVar name; smt_int_const (-1)]),
        smt_int_const 0)
    | S.RFloat -> SmtEq (
        SmtFunApp ("sym_vec_float_sum", [SmtVar name; smt_int_const (-1)]),
        smt_int_const 0)
    | _ -> failwith "Invalid type for symbolic sum" in
    let new_val = match ty with
    | S.RInt -> SmtEq (
        smt_getn new_name 0,
        SmtFunApp ("sym_vec_int_sum", [SmtVar name; SmtSub (smt_len name, smt_int_const 1)]))
    | S.RFloat -> SmtEq (
        smt_getn new_name 0,
        SmtFunApp ("sym_vec_float_sum", [SmtVar name; SmtSub (smt_len name, smt_int_const 1)]))
    | _ -> failwith "Invalid type for symbolic sum" in
    (new_name, ty, { S.path_list = [new_len;new_val] })

(* From two symbolic vectors, make a new symbolic vector which is
 constrained to be the concatenation of v1 and v2. *)
let symbolic_concat: smtvar -> S.symvec -> S.symvec -> S.symdef =
    fun new_name ((name1, ty1, _), _) ((name2, ty2, _), _) ->
    let new_ty = match_tys ty1 ty2 in
    (* len(x@y) = len(x) + len(y) *)
    let len = (SmtEq (
        smt_len new_name,
        SmtPlus (smt_len name1, smt_len name2))) in
    (* The constraint that elements 0 through len(a1) are the elements of a1 *)
    let a1_elts = all_elements name1
        (SmtEq (
            SmtArrGet (SmtVar new_name, SmtVar forall_var),
            SmtArrGet (SmtVar name1, SmtVar forall_var))) in
    (* The constraint that elements len(a1) through len(a1) + len(a2) are the elements of a2 *)
    let a2_elts = all_elements name2
        (SmtEq (
            SmtArrGet (SmtVar new_name, (SmtPlus (SmtVar forall_var, smt_len name2))),
            SmtArrGet (SmtVar name2, SmtVar forall_var))) in
    (new_name, new_ty, { S.path_list = [len; a1_elts; a2_elts] })

(* Produces a definition for symbolic x[y]. Assumes y is an index vector for x. *)
let symbolic_subscript: smtvar -> S.symvec -> S.symvec -> S.symdef =
    fun new_name ((name1, ty1, _), _) ((name2, ty2, _), _) ->
    let _ = match ty2 with
    | S.RInt -> ()
    | _ -> failwith "Subscript with non-integer symbolic vector!" in
    let len = lengthn new_name 1 in
    (* a[0] = x[y[0]] *)
    let value = (SmtEq (
        smt_getn new_name 0,
        SmtArrGet (SmtVar name1, SmtSub (smt_getn name2 0, smt_int_const 1)))) in
    (* Constraints on y *)
    let name2_c = is_index_vec name2 name1 in
    (* Note that constraints about name2 are going into new_name's path constraints.
    Path constraints do not actually care what vector they belong to. *)
    (new_name, ty1, { S.path_list = [len;value;name2_c] })

(* Produces a definition for symbolic x[y] <- z. Assumes y is a proper index vector for x, and
 that z has length 1. *)
let symbolic_subscript_assign: smtvar -> S.symvec -> S.symvec -> S.symvec -> S.symdef =
    fun new_name ((name1, ty1, _), _) ((name2, ty2, _), _) ((name3, ty3, _), _) ->
    let _ = match ty2 with
    | S.RInt -> ()
    | _ -> failwith "Subscript with non-integer symbolic vector!" in
    (* Values in z must be the same type as values in x *)
    let new_ty = match_tys ty1 ty3 in
    (* The new array has the same length as x *)
    let len = (SmtEq (
        smt_len new_name,
        smt_len name1)) in
    (* Forall i in a, if i=y[0]-1 then a[i]=z[0] else a[i]=x[i]. *)
    let vals = all_elements name3 (ifelse
        (SmtEq (SmtVar forall_var, SmtSub (smt_getn name2 0, smt_int_const 1)))
        (SmtEq (
            SmtArrGet (SmtVar new_name, SmtVar forall_var),
            SmtSub (smt_getn name2 0, smt_int_const 1)))
        (SmtEq (
            SmtArrGet (SmtVar new_name, SmtVar forall_var),
            SmtArrGet (SmtVar name1, SmtVar forall_var)))) in
    let name2_c = is_index_vec name2 name1 in
    let name3_c = lengthn name3 1 in
    (new_name, new_ty, {S.path_list = [len;vals;name2_c;name3_c]})

let sym_fail: unit -> (smtexpr -> smtexpr -> smtexpr) =
    fun _ -> failwith "Symbolic operation not implemented."

(* TODO: This should take an "assumptions" function which is smtvar -> smtvar -> smtexpr *)
(* General binary operations on symbolic vectors. Assumes that the warning
 will not be thrown if their lengths aren't compatible. The operator in this case
 must combine smtexprs. For example, the op for plus is expr1 -> expr2 -> SmtPlus (expr1, expr2) *)
let symbolic_op: smtvar -> (smtexpr -> smtexpr -> smtexpr) ->
        (smtvar -> smtvar -> smtexpr list) ->
        S.symvec -> S.symvec -> S.symdef =
    fun new_name smt_combo combo_assume ((name1, ty1, _), _) ((name2, ty2, _), _) ->
    let new_ty = match_tys ty1 ty2 in
    let assumes = combo_assume name1 name2 in
    (* The length of a vectorized operator on two vectors is the length of the longer vector *)
    let len = ifelse (SmtGt ((smt_len name1), (smt_len name2)))
        (SmtEq ((smt_len new_name), (smt_len name1)))
        (SmtEq ((smt_len new_name), (smt_len name2))) in
    (* Modulo the index in the smaller vector *)
    let vals = ifelse (SmtGt ((smt_len name1), (smt_len name2)))
        (all_elements_eq new_name
            (smt_combo (SmtArrGet (SmtVar name1, SmtVar forall_var)) 
                (SmtArrGet (SmtVar name2, (SmtMod (SmtVar forall_var, (smt_len name2)))))))
        (all_elements_eq new_name
            (smt_combo (SmtArrGet (SmtVar name1, (SmtMod (SmtVar forall_var, (smt_len name1)))))
                (SmtArrGet (SmtVar name2, SmtVar forall_var))))
        in
    (new_name, new_ty, { S.path_list = assumes @ [len; vals] })

(* Binary operations - ops are all smtexpr -> smtexpr -> smtexpr *)
(* Operations come with assumptions about the vectors that go into them.
    For example, symbolic div assumes that the divisor's elements are nonzero.
    The assumptions are smtvar -> smtvar -> smtexpr list, where the smtvars are
    the names of the symbolic vectors passed into the symbolic expression.
*)

(* Most binary operations assume nothing. *)
let empty_assume: smtvar -> smtvar -> smtexpr list =
    fun n1 n2 -> []

let symbolic_plus: smtexpr -> smtexpr -> smtexpr =
    fun e1 e2 -> SmtPlus (e1, e2)
let symbolic_sub = fun e1 e2 -> SmtSub (e1, e2)
let symbolic_div: smtexpr -> smtexpr -> smtexpr =
    fun e1 e2 -> SmtDiv (e1, e2)
(* Assumes all elements of the divisor !=0 *)
let symbolic_div_assume = fun n1 n2 ->
    [all_elements n2 (SmtNeq (
        SmtVar forall_var,
        (smt_int_const 0)))]

let symbolic_mul = fun e1 e2 -> SmtMult (e1, e2)
(* TODO: SmtMod is defined for integers - might have to encode our own floating-point
  modulus axioms. *)
let symbolic_mod = fun e1 e2 -> SmtMod (e1, e2)
(* TODO: is this integer exp, real exp, float exp? all three? *)
let symbolic_exp = fun e1 e2 -> SmtExp (e1, e2)

(* Symbolic comparison is the same as any other binop, but the result is a symbolic
  bool vector. *)
let symbolic_cmp: smtvar -> (smtexpr -> smtexpr -> smtexpr) -> S.symvec -> S.symvec -> S.symdef =
    fun new_name smt_combo v1 v2 ->
    let (op_name, op_ty, op_pcs) = symbolic_op new_name smt_combo empty_assume v1 v2 in
    (op_name, S.RBool, op_pcs)

(* TODO: Determine the domains of these functions! Don't want to produce a PC like
  "hello" > "fish" then not have SMT be able to solve just because R allows > for strs, but
  SMTLIB does not. *)
let symbolic_lt: smtexpr -> smtexpr -> smtexpr =
    fun e1 e2 -> SmtLt (e1, e2)
let symbolic_gt = fun e1 e2 -> SmtGt (e1, e2)
let symbolic_eq = fun e1 e2 -> SmtEq (e1, e2)
let symbolic_neq = fun e1 e2 -> SmtNeq (e1, e2)
let symbolic_geq = fun e1 e2 -> SmtGe (e1, e2)
let symbolic_leq = fun e1 e2 -> SmtLe (e1, e2)

(* A symbolic vectorized operation for booleans only - & and |.
  Do v1 & v2 by using (symbolic_bool_vec_op symbolic_and_op symbolic_andvec_failure) *)
let symbolic_logic_vec_op: smtvar -> (smtexpr -> smtexpr -> smtexpr) ->
        (unit -> S.symdef) -> 
        S.symvec -> S.symvec -> S.symdef =
    fun new_name smt_combo fail v1 v2 ->
    let (op_name, op_ty, op_pcs) = symbolic_op new_name smt_combo empty_assume v1 v2 in
    match op_ty with
    | S.RBool -> (op_name, op_ty, op_pcs)
    | _ -> fail ()

let symbolic_andvec_failure = fun _ -> failwith "Non-boolean symbolic &"
let symbolic_and_failure = fun _ -> failwith "Non-boolean symbolic &&"
let symbolic_and = fun e1 e2 -> SmtAnd (e1, e2)

let symbolic_orvec_failure = fun _ -> failwith "Non-boolean symbolic |"
let symbolic_or_failure = fun _ -> failwith "Non-boolean symbolic ||"
let symbolic_or = fun e1 e2 -> SmtOr (e1, e2)

let symbolic_xor = fun _ _ -> failwith "Symbolic xor unimplemented!"
let symbolic_xor_failure = fun _ -> failwith "Symbolic xor unimplemented!"

(* A symbolic single operation for booleans only - && and ||.
  Do v1 && v2 by using (symbolic_bool_single_op smybolic_and_op symbolic_and_failure) *)
let symbolic_logic_single_op: smtvar -> (smtexpr -> smtexpr -> smtexpr) ->
        (unit -> S.symdef) ->
        S.symvec -> S.symvec -> S.symdef =
    fun new_name smt_combo fail ((name1, ty1, _), _) ((name2, ty2, _), _) ->
    (* If they're both symbolic bool vectors, then make a vector of length 1
      with the appropriate path constraints *)
    match match_tys ty1 ty2 with
    | S.RBool -> let len = SmtEq (
            smt_len new_name,
            smt_int_const 1) in
            (* Constrain value[0] to be the combo of name1[0] and name2[0].*)
            let value = SmtEq (
                smt_getn new_name 0,
                smt_combo (smt_getn name1 0) (smt_getn name2 0)) in
            (new_name, S.RBool, { S.path_list = [len; value] })
    | _ -> fail ()

(* Copies a symbolic vector, introducing a new symbolic vector with a different name,
  and the same path constraints induced over the new name. *)
let copy_symbolic: smtvar -> S.symvec -> S.symvec =
    fun new_name ((name1, ty1, pc1), _) ->
    let new_pathcons = List.map (replace name1 new_name) pc1.S.path_list in
    (* The new vector has empty dependencies - The vector it was copied from is
      responsible for holding the vectors it depends on, and making their definitions
      in the Z3 file. *)
    ((new_name, ty1, { S.path_list = new_pathcons }), S.NoDepends)

(*
(* Create a new symbolic vector which contains the ith element of name1 *)
let symbolic_get_int: smtvar -> S.symvec -> int -> S.symvec =
    fun new_name (name1, _, _) n ->
        (* *)

(* Create a new symbolic vector which contains name1[name2[0]] *)
let symbolic_get: smtvar -> S.symvec -> S.symvec -> S.symvec
*)

