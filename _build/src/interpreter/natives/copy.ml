module S = Support
module L = Langutils
module Sym = Symbolic_ops
(* TODO: Current copying strategy goes into an infinite loop on cyclic pointer graphs.
Fortunately, I think that because R passes objects by deep copy, it's not possible to
create a cyclic pointer graph. *)
(*
(* TODO: need to do a deeper copy? *)
let copy_matrix: rmatrix -> bool -> rmatrix = 
    fun m byrow ->
    match m with
    | v, dims, opt_dimnames -> if byrow then
        let new_array = copy_vector v in (* TODO: inefficient to copy if I'm just going to overwrite *)
        let nr = dims.(0) in
        let nc = dims.(1) in
        let _ = for x = 0 to nr - 1 do
            for y = 0 to nc - 1 do
                new_array.(x + y*nr) <- v.(x*nc + y)
            done
        done in
        new_array
        (* TODO: need to deep copy dimnames? *)
        else match opt_dimnames with
        | Some dimnames -> (copy_vector v, Array.copy dims, Array.copy dimnames)
        | None -> (copy_vector v, Array.copy dims, None)
*)

(* helper *)
let unzip_list: ('a * 'b) list -> 'a list * 'b list =
    fun zipped_list -> List.fold_right (fun (x, y) (al, bl) -> x::al, y::bl)
    zipped_list ([], [])

let zip_list: 'a list -> 'b list -> ('a * 'b) list =
    fun l1 l2 -> List.map2 (fun x y -> (x,y)) l1 l2

let hashtable_to_alist: ('a, 'b) Hashtbl.t -> ('a * 'b) list =
    fun hashtable ->
    Hashtbl.fold (fun k v l -> (k,v)::l) hashtable []

let alist_to_hashtable: ('a * 'b) list -> ('a, 'b) Hashtbl.t =
    fun alist ->
        let h = Hashtbl.create (List.length alist) in (* TODO: default value? *)
        let _ = List.iter (fun (k, v) -> Hashtbl.add h k v) alist in
        h

(* deep copy and associated functions *)
let copy_rvector: S.state -> S.rvector -> (S.rvector * S.state) =
    fun state vec ->
    match vec with
    | S.IntVec v -> (S.IntVec (Array.copy v), state)
    | S.FloatVec v -> (S.FloatVec (Array.copy v), state)
    | S.ComplexVec v -> (S.ComplexVec (Array.copy v), state)
    | S.StrVec v -> (S.StrVec (Array.copy v), state)
    | S.BoolVec v -> (S.BoolVec (Array.copy v), state)
    | S.SymVec s -> let name, state' = S.name_fresh state in
        (S.SymVec (Sym.copy_symbolic name s), state')

(*
(* recursively copy the elements of a reference list *)
let rec copy_ref_array: S.memref list -> S.state -> (S.memref list * S.state) =
    fun mems state ->
    match mems with
    | hd::tl    -> let (m', s') = deep_copy hd state in
        let (ms, s'') = copy_ref_array tl s' in
        (m'::ms, s'')
    | []        -> ([], state)
*)
let rec copy_ref_array: S.memref list -> S.state -> (S.memref list * S.state) =
    fun mems state ->
    S.state_map deep_copy mems state

(* copy an associative list of keys to memory references *)
and copy_alist:'a. ('a * S.memref) list -> S.state -> ('a * S.memref) list * S.state =
    fun alist state ->
    let keys, mems = unzip_list alist in
    let mems', state' = copy_ref_array mems state in
    ((zip_list keys mems'), state') (* no need to copy keys *)

and copy_env: S.env -> S.state -> (S.env * S.state) =
    fun env state ->
    let bindings = S.IdentMap.bindings env.S.id_map in
    let idmems, state' = copy_alist bindings state in
    let env' = S.env_add_list idmems (S.env_empty ()) in
    let env'' = {env' with S.pred_mem = env.S.pred_mem} in (* parent is shared *)
    (env'', state')

and copy_attributes: S.attributes -> S.state -> (S.attributes * S.state) =
    fun attrib state ->
    let attr_alist = hashtable_to_alist attrib.S.rstr_map in
    let attr_alist', state' = copy_alist attr_alist state in
    let new_table = alist_to_hashtable attr_alist' in (* can use S.attrs_add_list too *)
    ({S.rstr_map = new_table}, state')

(* deep copy a memory reference *)
and deep_copy: S.memref -> S.state -> (S.memref * S.state) =
    fun mem state ->
    let obj = S.state_find mem state in
    match obj with
    | Some (S.PromiseObj _)   -> failwith "can't copy promises" (* TODO: force evaluation somehow? *)
    | Some (S.DataObj (v,a))    -> let (a', s') = copy_attributes a state in
        begin match v with
        | S.Vec v         -> let vnew, s'' = copy_rvector s' v in
            let alloc_obj = S.DataObj (S.Vec vnew, a') in
            S.state_alloc alloc_obj s''

        | S.RefArray v    -> let ms, s'' = copy_ref_array v s' in 
            let alloc_obj = S.DataObj ((S.RefArray ms), a') in
            S.state_alloc alloc_obj s''

        | S.FuncVal _     -> mem, state (* do not try to deepcopy functions *)

        | S.EnvVal v      -> let env, s'' = copy_env v s' in
            let alloc_obj = S.DataObj ((S.EnvVal env), a') in
            S.state_alloc alloc_obj s''
        end
    | None                  -> mem, state (* can copy pointers with no associated memory for free *)


