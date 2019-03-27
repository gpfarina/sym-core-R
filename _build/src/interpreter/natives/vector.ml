(*
    vector.ml

    Functions for dealing with rvectors - creating them, editing their attributes, etc.
    Functions ending in "mems" are intended to work with memory references and a state,
    and can be registered as native calls. Other functions are helpers for these
    native functions. Note that these functions take an S.state. Before introducing
    symbolic variables, they took an S.heap, since they could only allocate new
    data. Since they might need to produce names on the fly for symbolic values,
    they take an entire state.
    
    Right now, the error-handling scheme is very messy, with functions typically
    failing when they encounter invalid input. One possibility for a better
    error-handling scheme is for these functions to push an ErrorSlot of string
    onto the state's stack, to allow for interpreter rules that unwind the
    stack when encountering an ErrorSlot, rather than just crashing and burning.
*)

module S = Support
module C = Native_support (* was Coerce *)
module Copy = Copy
module L = Langutils
module Sym = Symbolic_ops
open Smtsyntax
open Smttrans

(* Need to do this pattern matching because of how rvector is defined *)
let rvector_length: S.rvector -> int =
    fun vec ->
    match vec with
    | S.IntVec i -> Array.length i
    | S.FloatVec f -> Array.length f
    | S.ComplexVec c -> Array.length c
    | S.StrVec s -> Array.length s
    | S.BoolVec b -> Array.length b
    | S.SymVec s -> failwith "Can't get integer length for symbolic vector!"

(* Create a length-1 rvector holding the length of another rvector. *)
let rvector_length_vec: S.rvector -> S.state -> S.rvector * S.state =
    fun vec state ->
    match vec with
    | S.IntVec i -> (S.IntVec (Array.make 1 (Some (Array.length i))), state)
    | S.FloatVec f -> (S.IntVec (Array.make 1 (Some (Array.length f))), state)
    | S.ComplexVec c -> (S.IntVec (Array.make 1 (Some (Array.length c))), state)
    | S.StrVec s -> (S.IntVec (Array.make 1 (Some (Array.length s))), state)
    | S.BoolVec b -> (S.IntVec (Array.make 1 (Some (Array.length b))), state)
    | S.SymVec ((n, _, _), deps) -> let new_name, state' = S.name_fresh state in
        (* out has length 1 *)
        let len = SmtEq (
            smt_len new_name,
            smt_int_const 1) in
        (* out[0] = len(in) *)
        let value = SmtEq (
            smt_getn new_name 0,
            smt_len n) in
        let vec = S.SymVec ((new_name, S.RInt, { S.path_list = [len; value] }), S.NoDepends) in
        vec, state'

let rvector_length_mem: S.memref -> S.state -> (S.memref * S.state) =
    fun data_ref state ->
    let data_rvec = C.dereference_rvector data_ref state in
    let len_vec, state' = rvector_length_vec data_rvec state in
    (* allocate the length *)
    S.state_alloc (S.DataObj (S.Vec len_vec, S.attrs_empty ())) state'

(* Handles dim(x) <- y. No physical change needs to be made to x because of how R handles
    dimensions: dimensions is just an attribute with the requirement that the product of
    the dimensions is equal to the length of its vector. Subsetting code deals explicitly
    with the dimensions to determine what elements to choose.
    This function returns a memref because of ex.
    y = (dim(x) <- c(1,5))  # y = [1, 5]

    TODO: do we need to copy dim_ref' again so that y here doesn't point directly to x's dims?
    I think it might be fine since there's no way to edit x's dims directly (access will make a copy)
*)
let set_dims_mem: S.memref -> S.memref -> S.state -> (S.memref * S.state) =
    fun data_ref dim_ref state ->
    (* first, copy the reference to the dimension vector, since data will keep it *)
    let dim_ref',state' = Copy.deep_copy dim_ref state in
    (* check if dims is compatible with data (product of dims == length of data) *)
    let dim_vec = C.resolve_vec (C.rvector_to_int_array (C.dereference_rvector dim_ref' state')) in
    let data_len = rvector_length (C.dereference_rvector data_ref state) in
    let dim_size = Array.fold_left (fun x y -> x * y) 1 dim_vec in
    if dim_size = data_len then
        match S.state_find data_ref state' with
        | Some (S.DataObj(_, attrs)) ->
            let _ = S.attrs_add (Some "dim") dim_ref' attrs in
            (dim_ref', state')
        | _ -> failwith "Data missing in set_dims"
    else
        failwith "Dimension not compatible with vector length" (* TODO: sprintf to show what the size was *)

(* Array of length 0 used as the basis for fold with concatenate. *)
let mk_empty_intvec: unit -> S.rvector = fun _ -> S.IntVec (Array.make 0 (Some 0))
let mk_empty_floatvec: unit -> S.rvector = fun _ -> S.FloatVec (Array.make 0 (Some 0.0))
let mk_empty_complexvec: unit -> S.rvector = fun _ -> S.ComplexVec (Array.make 0 (Some Complex.zero))
let mk_empty_strvec: unit -> S.rvector = fun _ -> S.StrVec (Array.make 0 (Some ""))
let mk_empty_boolvec: unit -> S.rvector = fun _ -> S.BoolVec (Array.make 0 (Some 0))
(* This is really ugly - The empty symbolic vector we make needs to be allocated since
    there's no vector for it to be a dependency of.
*)
let mk_empty_symvec: S.state -> S.rtype -> S.rvector * S.state =
    fun state ty ->
    let name, state' = S.name_fresh state in
    (* Length zero *)
    let len = SmtEq (
        smt_len name,
        smt_int_const 0) in
    (* vec is implicit - still need to allocate it! *)
    let vec = ((name, ty, { S.path_list = [len] }), S.NoDepends) in
    (* Memory address is dropped on the floor, we do not need it to refer to this vector since
      the symbolic operations we use it in will refer by name. *)
    let _, state'' = S.state_alloc (S.DataObj (S.Vec (S.SymVec vec), S.attrs_empty ())) state' in
    (S.SymVec vec), state''

(* Concatenate two vectors - For now does not do type coercion. *)
let concat_rvectors: S.rvector -> S.rvector -> S.state -> (S.rvector * S.state) =
    fun v1 v2 state ->
    match (v1, v2) with
    | (S.IntVec i1, S.IntVec i2) -> (S.IntVec (Array.concat [i1;i2]), state)
    | (S.FloatVec f1, S.FloatVec f2) -> (S.FloatVec (Array.concat [f1;f2]), state)
    | (S.ComplexVec c1, S.ComplexVec c2) -> (S.ComplexVec (Array.concat [c1;c2]), state)
    | (S.StrVec s1, S.StrVec s2) -> (S.StrVec (Array.concat [s1;s2]), state)
    | (S.BoolVec b1, S.BoolVec b2) -> (S.BoolVec (Array.concat [b1;b2]), state)
    (* Concrete values are converted to symbolic for symbolic primitive operations. *)
    | (S.SymVec _, _)
    | (_, S.SymVec _) -> Sym.sym_op (Sym.arg2listize Sym.symbolic_concat)
        [v1;v2] state
    | _ -> failwith "Can't concatenate incompatible vectors"

(* Folds rvectors together with concat *)
let fold_rvectors: S.rvector list -> S.state -> (S.rvector * S.state) =
    fun vlist state ->
    match vlist with
    | [] -> failwith "Cannot fold empty rvectors list - type unknown"
    (* Determine the type based on the head. 
    concat_rvectors will throw an error if the types do not reconcile. *)
    | hd::tl -> begin match hd with
        | S.IntVec _ -> S.state_fold_left concat_rvectors (mk_empty_intvec ()) vlist state
        | S.FloatVec _ -> S.state_fold_left concat_rvectors (mk_empty_floatvec ()) vlist state
        | S.ComplexVec _ -> S.state_fold_left concat_rvectors (mk_empty_complexvec ()) vlist state
        | S.StrVec _ -> S.state_fold_left concat_rvectors (mk_empty_strvec ()) vlist state
        | S.BoolVec _ -> S.state_fold_left concat_rvectors (mk_empty_boolvec ()) vlist state
        | S.SymVec ((_, t, _), _) -> let name, state' = S.name_fresh state in
            let empty, state'' = mk_empty_symvec state t in
            S.state_fold_left concat_rvectors (empty) vlist state'
        end

(* Allocates the concatenations of the rvectors. *)
let alloc_fold_vectors: S.rvector list -> S.state -> (S.memref * S.state) =
    fun vecs state ->
    match vecs with
    | [] -> (S.mem_null (), state) (* Empty arrays are null *)
    (* TODO: this might cause bugs if you try to assign to an empty array's dims or something
    since there's no actual object. On the other hand, just Array.makeing a 0-length vector
    also doesn't have the right behavior *)
    (* Fold them with concat *)
    | _ -> let new_vec, state' = fold_rvectors vecs state in
        (* Allocate the new vector and return a reference to it *)
        S.state_alloc (S.DataObj(S.Vec new_vec, S.attrs_empty ())) state'

(* Vector creation - R's "c" function. Does not support ex. c(a=c(1,2))
    This is currently unused since make_vector_mems handles the "names" attribute,
    and so can be applied to more complex cases like above.
*)
let make_vector_simple_mems: S.memref list -> S.state -> (S.memref * S.state) =
    fun vec_mems state ->
    (* First, deep copy the arguments so they're not captured *)
    let vec_mems', state' = Copy.copy_ref_array vec_mems state in
    (* Dereference the copies *)
    let vecs = List.map (fun m -> C.dereference_rvector m state') vec_mems' in
    alloc_fold_vectors vecs state'

(* Produces ex. (Some "a") 2 -> S.StrVec [| (Some "a1"), (Some "a2") |]
    When a vector is created using something like x=c(a=c(1,2),3,b=c(4,5)),
    x is an rvector: [1,2,3,4,5]. In this case, x has a "names" attribute, and
    names(x) = ["a1","a2","","b1","b2"]. Note that this storage method allows a
    vector to have multiple elements named "a1", if you do c(a=c(1,2),a=c(1,2)).
    This is a helper function that produces these names. Don't ask why, when
    an element of the vector was not named, R decided to name it empty string
    rather than NA.
*)
let n_names: S.rstring -> int -> S.rvector =
    fun so n ->
    match so with
    (* Empty string is for a vector that wasn't given a name as an argument.
    That means the names of all of its components in the final version will be "" *)
    | Some "" -> S.StrVec (Array.make n (Some ""))
    (* Non-empty string means that for n > 1, the names will be
     s ^ "1", s ^ "2", etc. *)
    | Some s -> if n = 1 then S.StrVec [|Some s|]
        else S.StrVec (Array.init n (fun i -> Some (s ^ (string_of_int (i + 1)))))
    (* None is invalid - variadic arguments will either not have a names attribute at all
     or have entries with "". Note that None is a valid value for an element of names in general,
     but it will never be passed in a call to make_vector. *)
    | None -> failwith "Invalid None name in n_names"

(* R's c() function. Handles the vector's names using the default
    arguments. Can be used to create symbolic vectors assuming no
    names are provided.
    You would expect x=c(y,a=1) with y symbolic to be an rvector
    with constraints such that x is the concatenation of y and [1].
    The complexity here is that x's names must also be symbolic,
    constrained to be a vector of empty strings equal to the length
    of y, concatenated with ["a"]. Since we do not currently support
    symbolic string vectors, this behavior is not supported.
*)
let make_vector_mems: S.memref -> S.state -> (S.memref * S.state) =
    fun var_mem state ->
    (* First, copy the arguments since vector will capture them *)
    let var_mem', state' = Copy.deep_copy var_mem state in
    (* Dereference the variadic argument into a list of memrefs and the attrs *)
    let vec_mems, list_attrs = C.dereference_rlist_attrs var_mem' state' in
    let vecs = List.map (fun m -> C.dereference_rvector m state') vec_mems in
    begin match S.attrs_find (Some "names") list_attrs with
    (* names is a StrVec with one element per element of vec_mems 
     We need to unpack it so that it has one element per element of the fold of the vectors.
     This means converting the "a" in a=c(1,2) to "a1,a2" *)
    | Some (arg_names_ref) -> let arg_names_list = Array.to_list
            (C.rvector_to_str_array (C.dereference_rvector arg_names_ref state')) in
        let vec_lengths = List.map (fun v -> rvector_length v) vecs in
        let n_names_list = List.map2 n_names arg_names_list vec_lengths in
        (* Fold the new names vectors into a single names and allocate it *)
        let names_ref, state'' = alloc_fold_vectors n_names_list state' in
        (* Fold the data vectors and into a single data and allocate it *)
        let vec_ref, state''' = alloc_fold_vectors vecs state'' in
        (* It's clunky, but we're going to dereference the newly-allocated vec_ref
         to bind it's attributes and create the names attribute. *)
        let _ = begin match S.state_find vec_ref state''' with
        | Some (S.DataObj (_, vec_attrs)) -> S.attrs_add (Some "names") names_ref vec_attrs
        | _ -> failwith "That should not have happened!!!"
        end in
        (* The return reference is to the resulting vector *)
        vec_ref, state'''
    (* No names means we're just supposed to fold the vectors together like usual.
      The resulting rvector has attrs_empty *)
    | None -> alloc_fold_vectors vecs state'
    end

(* Heavy lifting for range - creates a range of integers from start to end as an array *)
let int_range: int -> int -> int array =
    fun start_index end_index ->
    (* How many elements the array will have *)
    let n = (abs (end_index - start_index)) + 1 in
    (* Increasing or decreasing? *)
    let step = if start_index < end_index then 1 else -1 in
    Array.init n (fun i -> start_index + (i * step))

(* Same as ^ but for floats *)
let float_range: float -> float -> float array =
    fun startf endf ->
    (* How many elements the array will have *)
    let n = int_of_float (floor (abs_float (startf -. endf))) + 1 in
    let step = if startf < endf then 1.0 else -1.0 in
    Array.init n (fun i -> startf +. ((float_of_int i) *. step))

(* Helper for range: constrains the length of x:y based on expressions for x[0] and y[0] *)
let mk_range_len: smtvar -> smtexpr -> smtexpr -> smtexpr =
    fun range_name x0 y0 ->
    ifelse (SmtGt (x0, y0))
        (SmtEq (smt_len range_name,
            SmtPlus (SmtSub (x0,y0),
                smt_int_const 1)))
        (SmtEq (smt_len range_name,
            SmtPlus (SmtSub (y0,x0),
                smt_int_const 1)))

(* Helper for range: constrains the values of x:y based on expressions for x[0] and y[0] *)
let mk_range_vals: smtvar -> smtexpr -> smtexpr -> smtexpr =
    fun range_name x0 y0 ->
    all_elements_eq range_name (ifelse (SmtGt (x0, y0))
        (SmtSub (y0, SmtVar forall_var))
        (SmtPlus (y0, SmtVar forall_var)))

(* Helper for range: creates an rvector for x:y based on expressions for x[0] and y[0].
  Does not allocated because range_mems will allocate it. *)
let mk_range_vec: S.rtype -> smtexpr -> smtexpr -> S.state -> (S.rvector * S.state) =
    fun ty x0 y0 state ->
    let name, state' = S.name_fresh state in
    let len = mk_range_len name x0 y0 in
    let vals = mk_range_vals name x0 y0 in
    (S.SymVec ((name, ty, { S.path_list = [len;vals] }), S.NoDepends)), state'

(* Handles 1:5 and 5:-5, etc, as well as symbolic ex. x:3 *)
let range_mems: S.memref -> S.memref -> S.state -> (S.memref * S.state) =
    fun start_index_ref end_index_ref state ->
    (* Dereference the vectors down to rvectors *)
    let start_rvec = C.dereference_rvector start_index_ref state in
    let end_rvec = C.dereference_rvector end_index_ref state in
    (* Match to determine output vector type *)
    let out_rvec, state' = match start_rvec, end_rvec with
    | (S.IntVec [|Some si|], S.IntVec [|Some ei|]) -> let irange = int_range si ei in
        (S.IntVec (C.unresolve_vec irange), state)
    | (S.FloatVec [|Some sf|], S.FloatVec [|Some ef|]) -> let frange = float_range sf ef in
        (S.FloatVec (C.unresolve_vec frange), state)
    (* Symbolic ops. Doesn't use sym_op as usual because we want to use
      the actual value of a concrete vector if we can. *)
    (* TODO: Constrain each symvec which is an argument to have length 1. *)
    | (S.SymVec ((n1, S.RInt, _), _), S.SymVec ((n2, S.RInt, _), _)) ->
        mk_range_vec S.RInt (smt_getn n1 0) (smt_getn n2 0) state
    | (S.SymVec ((n1, S.RFloat, _), _), S.SymVec ((n2, S.RFloat, _), _)) ->
        mk_range_vec S.RFloat (smt_getn n1 0) (smt_getn n2 0) state
    | (S.SymVec ((n1, S.RInt, _), _), S.IntVec [|Some ei|]) ->
        mk_range_vec S.RInt (smt_getn n1 0) (smt_int_const ei) state
    | (S.IntVec [|Some si|], S.SymVec ((n2, S.RInt, _), _)) ->
        mk_range_vec S.RInt (smt_int_const si) (smt_getn n2 0) state
    | (S.SymVec ((n1, S.RFloat, _), _), S.FloatVec [|Some ef|]) ->
        mk_range_vec S.RFloat (smt_getn n1 0) (smt_float_const ef) state
    | (S.FloatVec [|Some sf|], S.SymVec ((n2, S.RFloat, _), _)) ->
        mk_range_vec S.RFloat (smt_float_const sf) (smt_getn n2 0) state
    | _ -> failwith "Bad range call" in (* TODO: better error messaging *)
    S.state_alloc (S.DataObj (S.Vec out_rvec, S.attrs_empty ())) state'

(* Makes an array which is a copied n times. Helper for vectorized operations.
  When we do c(1,2) + c(1,2,3,4), R checks the lengths of the two vectors to
  see if they are divisible. It then multiplies the shorter one to the length
  of the longer one, and does pointwise addition. If the lengths aren't divisible,
  R will warn and we will fail. *)
let rec array_wrap: 'a array -> int -> 'a array =
    fun a n ->
    (* Wrap once just means copy *)
    if n = 1 then Array.copy a
    (* Wrap more than once means append a copy of a onto the rest of the wrap *)
    else (Array.concat ( (Array.copy a)::[(array_wrap a (n-1))]))

(* TODO: can accomplish the same result with iteration
 and having each vector modulo the iterator by their length.
 Doesn't let us use the current setup, but definitely saves
 time copying things. Then again when has R ever cared about
 saving time copying things? :) *)
(* Wraps an rvector n times *)
let rvector_wrap: S.rvector -> int -> S.rvector =
    fun v n ->
    match v with
    | S.IntVec i -> S.IntVec (array_wrap i n)
    | S.FloatVec f -> S.FloatVec (array_wrap f n)
    | S.ComplexVec c -> S.ComplexVec (array_wrap c n)
    | S.StrVec s -> S.StrVec (array_wrap s n)
    | S.BoolVec b -> S.BoolVec (array_wrap b n)
    | S.SymVec s -> failwith "symbolic unimplemented"

(* Helper for vector_bop_mems. Extends the shorter rvector to the length
  of the longer one if their lengths are compatible. Only succeeds for
  non-symbolic vectors *)
let compatify_lengths: S.rvector -> S.rvector -> (S.rvector * S.rvector) =
    fun lhs rhs ->
    let l_length = rvector_length lhs in
    let r_length = rvector_length rhs in
    (* If their lengths match, then no extension is necessary *)
    if l_length = r_length then (lhs, rhs)
    (* Otherwise, check that their lengths are compatible
    (one is a multiple of the other) *)
    else if (l_length < r_length) && (r_length mod l_length = 0) then
        let n = r_length / l_length in
        (rvector_wrap lhs n, rhs)
    else if (l_length > r_length) && (l_length mod r_length = 0) then
        let n = l_length / r_length in
        (lhs, rvector_wrap rhs n)
    else failwith "Can't add vectors with incompatible lengths"

(* Vector binary operations - suitable functions to pass found in arithmetic.ml *)
(* TODO: does this function need to copy the arguments? *)
let vector_bop_mems: (S.rvector -> S.rvector -> S.state -> (S.rvector * S.state)) ->
                     S.memref -> S.memref -> S.state -> (S.memref * S.state) =
    fun binop lhs_ref rhs_ref state ->
    let lhs = C.dereference_rvector lhs_ref state in
    let rhs = C.dereference_rvector rhs_ref state in
    let  true_lhs, true_rhs  = match lhs, rhs with
    (* Symbolic vectors do not need to be altered *)
    | S.SymVec _, S.SymVec _
    | S.SymVec _, _
    | _, S.SymVec _ -> (lhs, rhs)
    (* If they're concrete, multiply out the shorter rvector, if appropriate *)
    | vec1, vec2 -> compatify_lengths vec1 vec2 in
    (* Do the operation on the (possibly wrapped) vectors *)
    let new_vec, state' = binop true_lhs true_rhs state in
    (* Allocate the result! *)
    (* TODO: is attrs_empty () appropriate? *)
    S.state_alloc (S.DataObj ((S.Vec new_vec), S.attrs_empty ())) state'

(* Inefficient, but works. *)
let array_filter: ('a -> bool) -> 'a array -> 'a array =
    fun pred a ->
    let l = List.filter pred (Array.to_list a) in
    Array.of_list l

(* Drops the dimensions of length 1 from a vector. Currently unused, but some
    vector functions have a drop_dims bool which defaults to false. We do not
    implement this, but this function should work if needed.
*)
let drop_dims_mems: S.memref -> S.state -> (S.memref * S.state) =
    fun vec_ref state ->
    (* First, copy the argument *)
    let vec_ref', state' = Copy.deep_copy vec_ref state in
    match S.state_find vec_ref state with
    | Some (S.DataObj (S.Vec v, attrs)) ->
        begin match S.attrs_find (Some "dim") attrs with
        | Some dim_ref -> let dim_vec = C.dereference_rvector dim_ref state in
            let dim_array = C.rvector_to_int_array dim_vec in
            let new_dim_array = array_filter (fun i -> i = Some 1) dim_array in
            (* Allocate the new dims *)
            let (dim_ref', state'') = S.state_alloc
                (S.DataObj (S.Vec (S.IntVec new_dim_array), S.attrs_empty ())) state' in
            (* Replace attrs' dim mapping *)
            let _ = S.attrs_add (Some "dim") dim_ref' attrs in
            (* TODO: what is this supposed to return? *)
            (dim_ref', state'')
        (* No dims means nothing to drop *)
        | None -> (vec_ref', state')
        end
    | Some _ -> failwith "DropDims: Data not a vector!"
    | None -> failwith "DropDims: Data vector not found!"

(* For use with ex. C.rvector_as_integer. Assumes that the conversion function
 will correctly copy the vector (?) *)
let convert_vector_mems: (S.rvector -> S.rvector) -> S.memref -> S.state -> (S.memref * S.state) =
    fun convert_fun vec_ref state ->
    (* Dereference it *)
    let rvec = C.dereference_rvector vec_ref state in
    (* Convert it *)
    let convert = convert_fun rvec in
    (* Allocate it *)
    (* TODO: when does type conversion keep attributes? *)
    S.state_alloc (S.DataObj (S.Vec (convert), S.attrs_empty ())) state

(* Helper for v *)
let array_length_assign: 'a option array -> int -> 'a option array =
    fun a n ->
    Array.init n (fun i -> if i < Array.length a then a.(i) else None)

(* Return an rvector with length len. If len is longer than vec, fills with NA,
  if len is shorter than vec, only uses the first len values *)
let rvector_length_int_assign: S.rvector -> int -> S.rvector =
    fun vec len ->
    match vec with
    | S.IntVec i -> S.IntVec (array_length_assign i len)
    | S.FloatVec f -> S.FloatVec (array_length_assign f len)
    | S.ComplexVec c -> S.ComplexVec (array_length_assign c len)
    | S.StrVec s -> S.StrVec (array_length_assign s len)
    | S.BoolVec b -> S.BoolVec (array_length_assign b len)
    | S.SymVec s -> failwith "symbolic unimplemented"
    
(* TODO: for this to work properly, the preprocessor needs to translate 
 length(x) = 12 as x = length<-(x, 12) rather than just length<-(x,12) *)
(* length<- *)
let vector_length_assign_mems: S.memref -> S.memref -> S.state -> (S.memref * S.state) =
    fun data_ref len_ref state ->
    let data_vec = C.dereference_rvector data_ref state in
    let len = match C.get_single_rint len_ref state with
    | Some i -> i (* TODO: R fails with length<-c(2,2), but this will merely warning *)
    | None -> failwith "NA invalid in length<-" in
    let new_rvec = rvector_length_int_assign data_vec len in
    (* Allocate it *)
    S.state_alloc (S.DataObj (S.Vec (new_rvec), S.attrs_empty ())) state

(* Helper for list_ref_length. Errors out if vec_ref points to anything other than
 a StrVec with length 1. *)
let assert_len1_strvec: S.memref -> S.state -> unit =
    fun vec_ref state ->
    (* dimnames can contain NAs, otherwise match [|Some s|] *)
    match C.dereference_rvector vec_ref state with
    | S.StrVec [|s|] -> ()
    | _ -> failwith "Invalid reference in list_ref_length."

(* Helper for list_dims_mems. Finds the "length" of a reference. In this case,
 Because dimnames is a list of lists or literal values, will produce an error if
 the reference is to anything other than a length 1 string vector or to a list *)
(* NOTE: R will coerce vectors in dimnames to lists - SimpleR will not. *)
let list_ref_length: S.memref -> S.state -> int =
    fun list_ref state ->
    match S.state_find list_ref state with
    (* Matches only a stringvec with length 1 *)
    | Some (S.DataObj (S.Vec (S.StrVec [|s|]), _)) -> 1
    | Some (S.DataObj (S.RefArray l, _)) ->
        (* Assert that each element of the list is a single string. *)
        let _ = List.iter (fun ref -> assert_len1_strvec ref state) l in
        List.length l
    | _ -> failwith "Invalid reference in list_ref_length."

(* Create the implied dims for a list - If the dims produced by this is the same
 as the actual dims for a vector, then this list is a valid dimnames *)
let list_dims_mems: S.memref -> S.state -> int array =
    fun list_ref state ->
    let lst = match S.state_find list_ref state with
    | Some (S.DataObj (S.RefArray l, _)) -> l
    | _ -> failwith "List not found" in
    let out_array = Array.make (List.length lst) 0 in
    (* Assign to out_array the calculated length of each reference *)
    let _ = List.iteri (fun i lref -> out_array.(i) <- list_ref_length lref state) lst in
    out_array

(* Compare two int arrays for equality *)
let compare_dims: int array -> int array -> bool =
    fun dims1 dims2 ->
    (* Want each element to be equal *)
    Array.fold_left (&&) true
        (* The array of bools, whether dims1[i] = dims2[i] *)
        (C.array_map2 (=) dims1 dims2)

(* dimnames<- *)
let vector_dimnames_assign_mems: S.memref -> S.memref -> S.state -> (S.memref * S.state) =
    fun data_ref dimnames_ref state ->
    (* Dimnames must be a LIST whose extent matches the dim attribute of the data.
     ex. If x has dims c(3,3,3), then the dimnames passed here must be a list of three lists,
     each with length three. Note that slicing a vector should also slice its dimnames (TODO) *)
    (* First, copy data and dimnames - data's attrs are edited, and dimnames is captured *)
    let data_ref', state' = Copy.deep_copy data_ref state in
    let dimnames_ref', state'' = Copy.deep_copy dimnames_ref state' in
    begin match S.state_find data_ref' state'' with
    | Some (S.DataObj (S.Vec v, data_attrs)) ->
        (* First get the dims to figure out what lengths dimnames has to satisfy *)
        begin match S.attrs_find (Some "dim") data_attrs with
        | Some (dim_ref) -> let dim_vec = C.resolve_vec 
                (C.rvector_to_int_array (C.dereference_rvector dim_ref state'')) in
            (* Check that dimnames has the right length *)
            let dimnames_dim_vec = list_dims_mems dimnames_ref' state in
            let _ = if compare_dims dim_vec dimnames_dim_vec then ()
                else failwith "Dimnames not compatible with dims" in
            (* Put dimnames into data's attrs *)
            let _ = S.attrs_add (Some "dimnames") dimnames_ref' data_attrs in
            (* Return a reference to dimnames and the state with the copies *)
            (* TODO: Same issue here as with dim<- : does the returned reference need to be a second copy? *)
            (dimnames_ref', state'')
        | None -> failwith "data has no dim attribute"
        end
    | _ -> failwith "Missing data or data not a vec in dimnames assignment"
    end

(* Create a symbolic vector whose length is constrained.
  Can only be used with symbolic int vectors or actual int vectors.
  Does not allocate because make_symbolic_mems will allocate. *)
let make_symbolic_vector: S.rvector -> S.rtype -> S.state -> (S.rvector * S.state) =
    fun vec ty state ->
    let new_name, state' = S.name_fresh state in
    match vec with
    (* TODO: 0-length is ok? *)
    | S.IntVec [| Some i |] when (i >= 0) -> let len = SmtEq (
            smt_len new_name,
            smt_int_const i) in
        (S.SymVec ((new_name, ty, { S.path_list = [len] }), S.NoDepends), state')
    | S.SymVec ((n,S.RInt,_), _) -> let  len = SmtEq (
            smt_len new_name,
            smt_getn n 0) in
        (S.SymVec ((new_name, ty, { S.path_list = [len] }), S.NoDepends), state')
    | _ -> failwith "Bad call to make_symbolic"

let get_ty: S.rvector -> S.rtype =
    function
    | S.StrVec [| Some s |] -> begin match s with
        | "int" -> S.RInt
        | "double" -> S.RFloat
        | "complex" -> failwith "Symbolic complex vectors unimplemented"
        | "string" ->  failwith "Symbolic string vectors unimplemented"
        | "logical" -> S.RBool
        | _ -> failwith "Unknown type in get_ty" end
    | _ -> failwith "Bad call to get_ty"

(* Make a symbolic vector with length equal to the first element of the input.
  Input can be symbolic. Invoke as e.g.
    x = symbolic(12, "int")
  to make symbolic vector of length 12. *)
let make_symbolic_mems: S.memref -> S.memref -> S.state -> (S.memref * S.state) =
    fun len_ref ty_ref state ->
    (* First, find the type that the user wants *)
    let ty_vec = C.dereference_rvector ty_ref state in
    let rty = get_ty ty_vec in
    let len_vec = C.dereference_rvector len_ref state in
    let sym_vec, state' = make_symbolic_vector len_vec rty state in
    S.state_alloc (S.DataObj (S.Vec sym_vec, S.attrs_empty ())) state'

(*
(* Given n, make a1, a2, ..., an *)
let make_index_names: S.rstring -> int -> S.rstring array =
    fun str n ->
    Array.init n (fun i -> str ^ (string_of_int (i+1)))

(* From ("a", c(2,2)) to ("a1", "a2"), (2,2). Does not allocate the rvectors *)
let make_vector_help: (S.rstring * S.memref) -> S.state -> (S.rvector * S.rvector) =
    fun (str, mem) -> let mem_vec = dereference_rvector mem state in
    let indices = make_index_names (rvec_length mem_vec) in
    (indices, mem_vec)

(* does make vector help for each vector, and concatenates them *)
let make_vector: (S.ident option * S.memref) list -> state -> (S.rvector * S.rvector) =
    fun l state ->
    List.map

(*let make_vector_mems: S.memref list -> state -> (memref * state)*)

*)


(*
(* assumes defaults have been taken care of *)
let get_dims: S.rvector -> S.rvector -> S.rvector -> S.rvector =
    fun data nrows ncols ->
    let int_nr = match nrows.(0) with
    | Some x    -> x
    | None      -> failwith "invalid nrows argument" in
    let int_nc = match ncols.(0) with
    | Some x    -> x
    | None      -> failwith "invalid ncols argument" in
    let len = Array.length data in
    let true_nr =
    match int_nr with
    | -1    -> begin match int_nc with
                | -1    -> len
                | 0     -> if len > 0 then failwith "nc = 0 for non-null data"
                    else 0
                | x     -> let xf = float_of_int x in
                    let lenf = float_of_int len in
                    int_of_float (ciel (lenf /. xf))
                end
    | x     ->  x in
    let true_nc =
    match int_nc with
    | -1    -> begin match true_nr with
                | 0 -> if len > 0 then failwith "nr = 0 for non-null data"
                    else 0
                | x -> let xf = float_of_int x in
                    let lenf = float_of_int len in
                    int_of_float (ciel (lenf /. xf))
                end
    | x     -> x in
    (* does the data fit into an nr * nc matrix? *)
    let _ = if len > 0 then
        let nrc = true_nr * true_nc in
        if (len > 1) && (nrc mod len <> 0) then
            if ((len > true_nr) && (len / true_nr) * true_nr <> len) ||
                ((len < true_nr && (true_nr / len) * len <> true_nr)) then
                failwith "data length is not a sub-multiple of the number of rows"
            else if ((len > nc) && (len / true_nc) * true_nc <> len) ||
                ((len < nc) && (true_nc / len) <> true_nc) then
                failwith "data length is not a sub-multiple of the number of columns"
            else ()
        else if (len > 1) && (nrc = 0) then failwith "data length exceeds matrix size"
        else ()
    else () in
    let matrix_dims = Array.make 2 (Some 0) in
    let _ = matrix_dims.(0) <- true_nr in
    let _ = matrix_dims.(1) <- true_nc in
    matrix_dims

let do_matrix_mems: S.memref list -> S.state -> (S.memref * S.state) =
    fun args state ->
    match args with
    | [data_ref;nrows_ref;ncols_ref;byrow_ref;dimnames_ref] ->
        (* copy data, dimnames *)
        let data_ref', state' = Copy.deep_copy data_ref state in
        let dimnames_ref', state'' = Copy.deep_copy dimnames_ref state' in

        (* dereference data that is not assigned to *)
        let nrows = C.dereference_rvector nrows_ref state in
        let ncols = C.dereference_rvector ncols_ref state in
        let byrow = C.dereference_rvector byrow_ref state in
        let old_data  = C.dereference_rvector data_ref' state in

        (* Create and allocate the new dims *)
        let dims = get_dims old_data nrows ncols in
        let dims_ref, state''' = state_alloc (S.DataObj (S.IntVec dims, attrs_empty ())) state'' in

        (* Add dim and dimnames to the new vector's attrs *)
        (* TODO: want to mutably access data attributes without reallocating the object *)
        let new_data, data_attrs = match S.MemRef_Map.find data_ref' state''' with
        | S.DataObj(d, a) -> (d, a)
        | _ -> failwith "invalid attributes in do_matrix"
        in
        let _ = S.attrs_add "dim" dims_ref data_attrs in
        let _ = S.attrs_add "dimnames" dimnames_ref' data_attrs in
        (* Rearrange the new vector according to dims *)
        let _ = rearrange_vec new_data byrow dims in
        (* return the copied vector and the state with dims allocated *)
        (data_ref', state''')
    | _ -> failwith "bad call to do_matrix" (* wrong number of arguments *)
*)

