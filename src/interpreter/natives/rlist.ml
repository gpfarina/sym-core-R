(*
    rlist.ml
    
    Functions for working with lists a la vector.ml.
    Most list operations are not implemented:
    lists are not actually a very common structure to use in R.
*)
module S = Support
module C = Native_support
module Copy = Copy

(* Top level length of a list *)
let list_length_mems: S.memref -> S.heap -> (S.memref * S.heap) =
    fun list_ref heap ->
    let lst, attrs = C.dereference_rlist_attrs list_ref heap in
    List.length lst

(* It might seem silly, but make_list expects its arguments as a RefArray. This is
 because it takes variadic arguments, and variadic arguments are passed as RefArray to
 allow for things like ex.
    list(ayy=c(1), lmao=c(2))
 which is passed as
    RefArray [c(1)_ref::c(2)_ref] , names = ref to (StrVec [| "ayy"; "lmao" |])
 The resulting list should actually be the same as the arguments passed, but copied
 since the arguments are captured. Copying also takes care of setting up the names attribute.
 *)
let make_list_mems: S.memref -> S.heap -> (S.memref * S.heap) =
    fun list_ref heap ->
    Copy.deep_copy list_ref heap

let rec split_rarray: ('a -> S.heapobj) -> 'a array -> heap -> int -> (S.memref list * heap) =
    fun f a heap n ->
    if n = Array.length a then
        ([], heap)
    else
    (* Create a single heap object from the nth element of a *)
    let obj = f a.(n) in
    let mem, heap' = S.heap_alloc obj heap in
    let (mems, heap'') = split_rints a heap' (n+1) in
    mem::mems, heap''

let mk_intvec: S.rint -> S.heapobj =
    fun i -> S.DataObj(S.Vec (S.IntVec [|i|]), S.attr_empty ())
let mk_floatvec: S.rfloat -> S.heapobj =
    fun f -> S.DataObj(S.Vec (S.FloatVec [|f|]), S.attr_empty ())
let mk_complexvec: S.rcomplex -> S.heapobj =
    fun c -> S.DataObj(S.Vec (S.ComplexVec [|c|]), S.attr_empty ())
let mk_strvec: S.rstring -> S.heapobj =
    fun s -> S.DataObj(S.Vec (S.StrVec [|s|]), S.attr_empty ())
let mk_boolvec: S.rbool -> S.heapobj =
    fun b -> S.DataObj(S.Vec (S.BoolVec [|b|]), S.attr_empty ())

(* TODO: should this be in vector.ml? *)
(* Make and allocate n individual rvectors from a vector of length n *)
let split_rvector: S.rvector -> heap -> (S.memref list * heap) =
    fun vec heap ->
       match vec with
       | S.IntVec i -> split_rarray mk_intvec i heap 0
       | S.FloatVec f -> split_rarray mk_floatvec f heap 0
       | S.ComplexVec c -> split_rarray mk_complexvec c heap 0
       | S.StrVec s -> split_rarray mk_strvec s heap 0
       | S.BoolVec b -> split_rarray mk_boolvec b heap 0

(* Conversion *)
(* TODO: bad if we allocate the attrs only to have a problem somewhere else? *)
let list_of_vector_mems: S.memref -> S.heap -> (S.memref * S.heap) =
    fun vec_ref heap ->
    let vec, vec_attrs = match S.heap_find vec_ref heap with
    | Some (S.DataObj (S.Vec v, vattrs)) -> v, vattrs
    | _ -> failwith "Invalid vector argument in list_of_vector"
    in
    (* Copy names and add it to a new attrs *)
    let list_attrs, heap' = match S.attrs_find "names" heap with
    | Some names_ref -> let names_ref', heap' = Copy.deep_copy names_ref in
        let a = S.attr_empty () in
        let _ = S.attrs_add (Some "names") names_ref' a in
        a, heap'
    | None -> S.attr_empty (), heap
    in
    (* Change the vector into a list of length-1 vectors allocated separately *)
    let vec_refs, heap'' = split_rvector vec heap' in
    (* Make a ListVal from the references list, then allocate it *)
    let new_list = S.ListVal (List.map (fun ref -> (None, ref)) vec_refs) in
    S.heap_alloc S.DataObj(new_list, list_attrs) heap''

(* names<- for lists *)
let list_names_assign_mems: S.memref -> S.memref -> S.heap -> (S.memref * S.heap) =
    fun list_ref names_ref heap ->
    (* Copy the list since we're editing it *)
    let list_ref', heap' = Copy.deep_copy list_ref heap in
    (* Dereference the list to bind its attrs and check its length *)
    let list_len, list_attrs = match S.heap_find list_ref' heap' with
    | Some (S.DataObj (S.ListVal l, lattrs)) -> (List.length l), lattrs
    | _ -> failwith "Invalid list argument in list_names_assign" in
    (* Dereference the vector to extend it to lst_len. Extending
      copies the vector, so no need to copy it earlier (plus its attributes aren't needed
      for it to be a 'names') *)
    let names = match S.heap_find names_ref', heap' with
    | Some (S.DataObj (S.Vec (S.StrVec s), _)) -> C.na_extend_array s list_len
    | _ -> failwith "Invalid names argument in list_names_assign" in
    (* Allocate the new names and add it to list_attrs *)
    let names_ref', heap'' = S.heap_alloc (S.DataObj (S.Vec (S.StrVec names)), S.attr_empty ()) in
    let _ = S.attrs_add (Some "names") names_ref' lattrs in
    (names_ref', heap'')

