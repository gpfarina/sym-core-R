module S = Support
(* open Support *)
(* open Smtsyntax
open Smttrans
*)

(* TODO: Array.map2 introduced in 4.03 but for dumb reasons we're in 4.01.
 This is my best shot at conciseness *)
let array_map2: ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array =
    fun f a1 a2 ->
    let l1 = Array.length a1 in
    let l2 = Array.length a2 in
    if l1 = l2 then
        (* Kind of gross *)
        Array.init l1 (fun i -> f a1.(i) a2.(i))
    else
        failwith "Map2: arrays not of same length"

(* Memory management *)
let resolve_vec: 'a. 'a option array -> 'a array =
    fun v ->
    Array.map (fun x -> match x with
        | Some x' -> x'
        | None    -> failwith "NA when resolving vector") v

let unresolve_vec: 'a array -> 'a option array =
    fun v ->
    Array.map (fun x -> Some x) v

let rvector_to_int_array: S.rvector -> S.rint array =
    fun vec -> match vec with
    | S.IntVec i  -> i
    | _         -> failwith "can't cast vector to int array!" (* TODO other types can be coerced *)

let rvector_to_str_array: S.rvector -> S.rstring array =
    fun vec -> match vec with
    | S.StrVec s -> s
    | _         -> failwith "can't cast vector to string array!"

let value_to_rvector: S.value -> S.rvector =
    fun value -> match value with
    | S.Vec vec   -> vec
    | _         -> failwith "can't cast! to rvector!" (* TODO: lists can be coerced *)

let value_to_int_array: S.value -> S.rint array =
    fun value -> let vec = value_to_rvector value in
    rvector_to_int_array vec

let dereference_rvector: S.memref -> S.state -> S.rvector =
    fun mem state ->
    match S.state_find mem state with
    | Some (S.DataObj (S.Vec v, _)) -> v
    | _ -> failwith "Vec expected in dereference"

let dereference_rlist_attrs: S.memref -> S.state -> S.memref list * S.attributes =
    fun mem state ->
    match S.state_find mem state with
    | Some (S.DataObj (S.RefArray l, a)) -> l, a
    | _ -> failwith "List expected in dereference"

(* TODO: catch a dereference error somehow? *)
let dereference: S.memref list -> S.state -> S.heapobj list =
    fun mems state ->
    List.map (fun mem -> S.MemRefMap.find mem state.S.heap.S.mem_map) mems

(* Get a number from a memref. Fails if the memref does not point to an int vector
 Warns if the vector has length greater than 1. *)
let get_single_rint: S.memref -> S.state -> S.rint =
    fun mem state ->
    let vec = dereference_rvector mem state in
    let a = rvector_to_int_array vec in
    let _ = if (Array.length a) > 1 then Printf.printf
        "Warning: only the first element used of expression with %d elements" (Array.length a)
    else
        () in
    a.(0)

let get_single_rstr: S.memref -> S.state -> S.rstring =
    fun mem state ->
    let vec = dereference_rvector mem state in
    let a = rvector_to_str_array vec in
    let _ = if (Array.length a) > 1 then Printf.printf
        "Warning: only the first element used of expression with %d elements" (Array.length a)
    else
        () in
    a.(0)

(* Extend a vector to 'length' with NAs of the appropriate type. Error if length is less than
 the length of the vector *)

let array_extend_err: unit -> 'a =
    fun _ -> failwith "Cannot extend array to a smaller length"

(* Not actually recursive, but this lets me use na_extend_help instead of 
  writing five different versions with separate type annotations *)
let na_extend_array: 'a. 'a option array -> int -> 'a option array =
    fun a n ->
    let a_len = Array.length a in
    if n >= a_len then
        Array.init n (fun i -> if i < a_len then a.(i) else None)
    else array_extend_err ()

let na_extend: S.rvector -> int -> S.rvector =
    fun vec len ->
    match vec with
    | S.IntVec i -> S.IntVec (na_extend_array i len)
    | S.FloatVec f -> S.FloatVec (na_extend_array f len)
    | S.ComplexVec c -> S.ComplexVec (na_extend_array c len)
    | S.StrVec s -> S.StrVec (na_extend_array s len)
    | S.BoolVec b -> S.BoolVec (na_extend_array b len)
    | S.SymVec _ -> failwith "NA extend not implemented for symbolic vectors"

