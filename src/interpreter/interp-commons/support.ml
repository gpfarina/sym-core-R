(*
    support.ml

    Type definitions and various helper functions for the interpreter.
    Defines all heap objects, the heap, the stack, etc., and provides
    some basic functions for manipulating each.
*)

module R = Syntax
module A = Annotations
module S = Smtsyntax

(* Type aliases *)
type complex = Complex.t

type tag = A.tag
type annot = A.annot
type tick = A.annot R.tick
type memref = R.memref
type numeric = R.numeric
type ident = tag R.ident
type const = R.const
type param = (tag, annot) R.param
type arg = (tag, annot) R.arg
type expr = (tag, annot) R.expr
type rint = R.rint
type rfloat = R.rfloat
type rcomplex = R.rcomplex
type rstring = R.rstring
type rbool = R.rbool

type smtvar = S.smtvar
type smtconst = S.smtconst
type smtexpr = S.smtexpr

(* R data types. A symbolic vector uses this to store its type information. *)
type rtype =
  | RBool
  | RInt
  | RFloat
  | RComplex
  | RString

(* Memory reference Map *)
module MemRef = struct
  type t = memref

  let compare : t -> t -> int =
    fun a b ->
      compare a.R.addr b.R.addr
end

(* The heap is a MemRefMap *)
module MemRefMap = Map.Make(MemRef)


(* RString Map *)
module RString = struct
  type t = rstring

  let compare : t -> t -> int =
    fun a b ->
      compare a b
end

module RStringMap = Map.Make(RString)


(* Identifier Map *)
module Ident = struct
  type t = ident

  let compare : t -> t -> int =
    fun a b ->
      compare (a.R.pkg, a.R.name) (b.R.pkg, b.R.name)
end

module IdentMap = Map.Make(Ident)


(* The environment is an RStringMap with a pointer to its predecessor. 
    Looking up a variable amounts to searching the current environment, then
    going up a level and repeating this process until the variable is found, or
    the current environment has no predecessor. *)
(* Environment *)
type env =
  { id_map : memref IdentMap.t;
    pred_mem : memref }

(* Logical path constraints *)
type pathcons =
  { path_list : smtexpr list }

(* Note: cannot do symvec = (smtvar * rtype * pathcons * symvec list) because
    of how type checking works. *)
(* For info on why symbolic vectors have symdepends, see natives/symbolic_ops.ml *)
(* Named type for symbolic definition *)
type symdef = (smtvar * rtype * pathcons)
type symvec = (symdef * symdepends)

(* Hack to fix type alias checking *)
and symdepends =
  | NoDepends
  | Depends of symvec list

(* Values *)
type rvector =
  | IntVec of rint array
  | FloatVec of rfloat array
  | ComplexVec of rcomplex array
  | StrVec of rstring array
  | BoolVec of rbool array
  | SymVec of symvec

type value =
  | Vec of rvector
  | RefArray of memref list (* for function arguments as well as R lists *)
  | FuncVal of param list * expr * memref
  | EnvVal of env

(* Implemented as a hashtbl for ease of use and efficiency of copying. *)
type attributes =
  { rstr_map : (rstring, memref) Hashtbl.t }

(* Stack *)
type slot =
    EvalSlot of expr
  | ReturnSlot of memref
  | SeqSlot of expr list
  | BranchSlot of expr * expr
  | LoopSlot of expr * expr * memref option (* body's return value *)

  (* Used for eager stuff *)
  | AssignSlot of ident
  | SupAssignSlot of ident
  | LambdaASlot of memref option * (arg * memref) list * arg option * arg list
  | LambdaBSlot of memref

  (* Used for lazy evaluation *)
  | UpdateSlot of memref
  | ArgsSlot of arg list
  | AttrSlot of memref option * expr option

type frame =
  { env_mem : memref;
    slot : slot }

type stack =
  { frame_list : frame list }


(* Heap *)
type heapobj =
    PromiseObj of expr * memref
  | DataObj of value * attributes

(* next_mem used to produce fresh memory references *)
type heap =
  { mem_map : heapobj MemRefMap.t;
    next_mem : memref }

(* The list of symbolic memory references that will be used to ouput Z3 path constraints.
    Every symbolic vector ever allocated should be a part of this list, and should not be
    deallocated. *)
type sym_mems =
  { mem_list : memref list }

(* Execution state *)
type state =
  { stack : stack;
    heap : heap;
    global_env_mem : memref;
    sym_mems : sym_mems;
    fresh_count : int; (* fresh_count used to produce fresh symbolic names. *)
    pred_unique : int;
    unique : int }


(* Utility functions *)

let state_unique : int ref = ref 32

let tag_state_unique : int -> state -> state =
  fun pred state ->
    let state2 = { state with pred_unique = pred;
                              unique = !state_unique } in
      incr state_unique;
      state2;;


(* Memory references *)
let mem_of_int : int -> memref =
  fun addr -> { R.addr = addr }

let mem_incr : memref -> memref =
  fun mem -> { mem with R.addr = mem.R.addr + 1 }

let mem_null : unit -> memref =
  fun _ -> mem_of_int 0

let is_mem_null : memref -> bool =
  fun mem -> mem = mem_null ()


(* R type utility *)
let rint_of_int : int -> rint =
  fun i -> Some i

let na_rint : unit -> rint =
  fun _ -> None

let int_of_rint : rint -> int option =
  fun ri -> ri

let rfloat_of_float : float -> rfloat =
  fun f -> Some f

let na_rfloat : unit -> rfloat =
  fun _ -> None

let float_of_rfloat : rfloat -> float option =
  fun rf -> rf

let rcomplex_of_complex : complex -> rcomplex =
  fun c -> Some c

let na_rcomplex : unit -> rcomplex =
  fun _ -> None

let complex_of_rcomplex : rcomplex -> complex option =
  fun rc -> rc

let rbool_of_bool : int -> rbool =
  fun b -> Some b

let na_rbool : unit -> rbool =
  fun _ -> None

let bool_of_rbool : rbool -> int option =
  fun rb -> rb

let rstring_of_string : string -> rstring =
  fun s -> Some s

let na_rstring : unit -> rstring =
  fun _ -> None

let string_of_rstring : rstring -> string option =
  fun rs -> rs

(* Values *)
let rvector_length : rvector -> int =
  fun rvec ->
    match rvec with
    | IntVec ivec -> Array.length ivec
    | FloatVec fvec -> Array.length fvec
    | ComplexVec cvec -> Array.length cvec
    | StrVec svec -> Array.length svec
    | BoolVec bvec -> Array.length bvec
    | SymVec _ -> failwith "No integer length for symbolic vectors."

(* One-based indexing *)
let rvector_get_rint : rvector -> int -> rint option =
  fun rvec i ->
    match rvec with
    | IntVec arr ->
        if i <= rvector_length rvec then
          Some (Array.get arr (i - 1))
        else
          None
    | _ -> None

let rvector_get_rfloat : rvector -> int -> rfloat option =
  fun rvec i ->
    match rvec with
    | FloatVec arr ->
        if i <= rvector_length rvec then
          Some (Array.get arr (i - 1))
        else
          None
    | _ -> None

let rvector_get_rcomplex : rvector -> int -> rcomplex option =
  fun rvec i ->
    match rvec with
    | ComplexVec arr ->
        if i <= rvector_length rvec then
          Some (Array.get arr (i - 1))
        else
          None
    | _ -> None

let rvector_get_rstring : rvector -> int -> rstring option =
  fun rvec i ->
    match rvec with
    | StrVec arr ->
        if i <= rvector_length rvec then
          Some (Array.get arr (i - 1))
        else
          None
    | _ -> None

let rvector_get_rbool : rvector -> int -> rbool option =
  fun rvec i ->
    match rvec with
    | BoolVec arr ->
        if i <= rvector_length rvec then
          Some (Array.get arr (i - 1))
        else
          None
    | _ -> None


(* Fresh identifier *)
let id_default : unit -> ident =
  fun _ ->
    { R.pkg = None;
      R.name = na_rstring ();
      R.tag = None }

let id_of_rstring : rstring -> ident =
  fun name ->
    { (id_default ()) with R.name = name }

let id_of_string : string -> ident =
  fun name ->
    id_of_rstring (rstring_of_string name)

(* Produce a fresh identifier for a variable. Unused. *)
let id_fresh : state -> ident * state =
  fun state ->
    let count2 = state.fresh_count + 1 in
    let name = rstring_of_string ("fs" ^ string_of_int count2) in
      (id_of_rstring name, { state with fresh_count = count2 })

(* Produce a fresh name for a symbolic vector. *)
let name_fresh: state -> string * state =
    fun state ->
    let count2 = state.fresh_count + 1 in
    let name = "fs" ^ (string_of_int count2) in
    (name, {state with fresh_count = count2})

(* List.map for states, but doing the operation so that the new state
   produced from the first operation is used in the second, and so on.
   Used for ex. allocating a list of vectors. *)
let rec state_map:
  ('a -> state -> ('b * state)) -> 'a list -> state -> ('b list * state) =
  fun f alist state ->
    match alist with
    | hd :: tl -> let b, state' = f hd state in
        let bs, state'' = state_map f tl state' in
        (b::bs, state'')
    | [] -> ([], state)

(* List.fold_left that preserves changes in the state during the operation, just like
    state_map. *)
let rec state_fold_left:
  ('a -> 'b -> state -> ('a * state)) -> 'a -> 'b list -> state ->
    ('a * state) =
  fun f init bs state ->
    match bs with
    | (hd :: tl) -> let a, state' = f init hd state in
        state_fold_left f a tl state'
    | [] -> (init, state)

let rec id_fresh_list : int -> state -> (ident list) * state =
  fun n state ->
    if n < 1 then
      ([], state)
    else
      let (id, state2) = id_fresh state in
      let (ids_tl, state3) = id_fresh_list (n - 1) state2 in
        (id :: ids_tl, state3)

let id_variadic : unit -> ident =
  fun _ ->
    id_of_string "..."


(* Attributes *)
let attrs_empty : unit -> attributes =
  fun _ ->
    { rstr_map = Hashtbl.create 20 } (* TODO: what is the expected size? *)

let attrs_find : rstring -> attributes -> memref option =
  fun rstr attrs ->
    try
      Some (Hashtbl.find attrs.rstr_map rstr)
    with
      Not_found -> None

(* Replaces because Hashtbls can have multiple bindings of the same key
   that shadow each other.
   In our case, R attributes should not be able to do the same thing. *)
let attrs_add : rstring -> memref -> attributes -> unit =
  fun rstr mem attrs ->
    Hashtbl.replace attrs.rstr_map rstr mem;;

let rec attrs_add_list : (rstring * memref) list -> attributes -> unit =
  fun binds attrs ->
    match binds with
    | [] -> ()
    | ((rstr, mem) :: binds_tl) ->
        attrs_add rstr mem attrs;
        attrs_add_list binds_tl attrs;;

(* Frame operations *)
let frame_default : unit -> frame =
  fun _ ->
    { env_mem = mem_null ();
      slot = ReturnSlot (mem_null ()) }


(* Stack operations *)
let stack_empty : unit -> stack =
  fun _ ->
    { frame_list = [] }

let stack_pop : stack -> (frame * stack) option =
  fun stack ->
    match stack.frame_list with
    | [] -> None
    | (frame :: frames_tl) ->
        Some (frame, { stack with frame_list = frames_tl })

let stack_pop_v : stack -> (slot * memref * stack) option =
  fun stack ->
    match stack_pop stack with
    | None -> None
    | Some (frame, stack2) ->
        Some (frame.slot, frame.env_mem, stack2)

let stack_pop_v2 : stack -> (slot * memref * slot * memref * stack) option =
  fun stack ->
    match stack_pop_v stack with
    | None -> None
    | Some (slot1, env_mem1, stack2) ->
        match stack_pop_v stack2 with
        | None -> None
        | Some (slot2, env_mem2, stack3) ->
            Some (slot1, env_mem1, slot2, env_mem2, stack3)

let stack_push : frame -> stack -> stack =
  fun frame stack ->
    { stack with frame_list = frame :: stack.frame_list }

let rec stack_push_list : frame list -> stack -> stack =
  fun frames stack ->
    match frames with
    | [] -> stack
    | (frame :: frames_tl) ->
        stack_push frame (stack_push_list frames_tl stack)


(* Heap operations *)
let heap_empty : unit -> heap =
  fun _ ->
    { mem_map = MemRefMap.empty;
      next_mem = mem_incr (mem_null ()) }

let heap_empty_offset : int -> heap =
  fun offset ->
    { (heap_empty ()) with next_mem = mem_of_int offset }

let binds_of_heap : heap -> (memref * heapobj) list =
  fun heap ->
    MemRefMap.bindings heap.mem_map

let heap_find : memref -> heap -> heapobj option =
  fun mem heap ->
    try
      Some (MemRefMap.find mem heap.mem_map)
    with
      Not_found -> None

let rec heap_find_deep : memref -> heap -> (memref * heapobj) option =
  fun mem heap ->
    match heap_find mem heap with
    | None -> None
    | Some (PromiseObj (R.MemRef mem2, _)) -> heap_find_deep mem2 heap
    | Some hobj -> Some (mem, hobj)

let heap_add : memref -> heapobj -> heap -> heap =
  fun mem hobj heap ->
    { heap with mem_map = MemRefMap.add mem hobj heap.mem_map }

let rec heap_add_list : (memref * heapobj) list -> heap -> heap =
  fun binds heap ->
    match binds with
    | [] -> heap
    | ((mem, hobj) :: binds_tl) ->
        heap_add_list binds (heap_add mem hobj heap)

(* Allocate a heap object on the heap. *)
let heap_alloc : heapobj -> heap -> memref * heap =
  fun hobj heap ->
    let used_mem = heap.next_mem in
      (used_mem, { (heap_add used_mem hobj heap) with
                     next_mem = mem_incr used_mem })

let rec heap_alloc_list : heapobj list -> heap -> memref list * heap =
  fun hobjs heap ->
    match hobjs with
    | [] -> ([], heap)
    | (hobj :: hobjs_tl) ->
        let (mem, heap2) = heap_alloc hobj heap in
        let (mems_tl, heap3) = heap_alloc_list hobjs_tl heap2 in
          (mem :: mems_tl, heap3)

let heap_alloc_const : R.const -> heap -> (memref * heap) =
  fun const heap ->
    heap_alloc
      (DataObj ((match const with
          R.Str s -> Vec (StrVec ([|s|]))
        | R.Num (R.Int i) -> Vec (IntVec ([|i|]))
        | R.Num (R.Float f) -> Vec (FloatVec ([|f|]))
        | R.Num (R.Complex c) -> Vec (ComplexVec ([|c|]))
        | R.Bool b -> Vec (BoolVec ([|b|]))
        | R.Nil -> Vec (BoolVec ([||]))
        ), attrs_empty ())) heap

let heap_remove : memref -> heap -> heap =
  fun mem heap ->
    { heap with mem_map = MemRefMap.remove mem heap.mem_map }

let rec heap_remove_list : memref list -> heap -> heap =
  fun mems heap ->
    match mems with
    | [] -> heap
    | (mem :: mems_tl) -> heap_remove_list mems_tl (heap_remove mem heap)


(* Environment functions *)

let env_empty : unit -> env =
  fun _ ->
    { id_map = IdentMap.empty;
      pred_mem = mem_null () }

(* Flattening *)
let binds_of_env : env -> (ident * memref) list =
  fun env ->
    IdentMap.bindings env.id_map

(* First occurrence within the environment *)
let rec env_find : ident -> env -> heap -> memref option =
  fun id env heap ->
    try
      Some (IdentMap.find id env.id_map)
    with
      Not_found ->
        match heap_find env.pred_mem heap with
        | Some (DataObj (EnvVal env2, _)) -> env_find id env2 heap
        | _ -> None

let rec env_find_rstr : rstring -> env -> heap -> memref option =
  fun rstr env heap ->
    match List.filter (fun (i, m) -> i.R.name = rstr) (binds_of_env env) with
    | ((_, mem) :: _)-> Some mem
    | [] ->
        match heap_find env.pred_mem heap with
        | Some (DataObj (EnvVal env2, _)) -> env_find_rstr rstr env2 heap
        | _ -> None

(* Finds id in the environment pointed to by env_mem *)
let env_mem_find : ident -> memref -> heap -> memref option =
  fun id env_mem heap ->
    match heap_find env_mem heap with
    | Some (DataObj (EnvVal env, _)) -> env_find id env heap
    | _ -> None

let env_mem_find_rstr : rstring -> memref -> heap -> memref option =
  fun rstr env_mem heap ->
    match heap_find env_mem heap with
    | Some (DataObj (EnvVal env, _)) -> env_find_rstr rstr env heap
    | _ -> None

(* Add to outermost level of environment *)
let env_add : ident -> memref -> env -> env =
  fun id mem env ->
    { env with id_map = IdentMap.add id mem env.id_map }

let rec env_add_list : (ident * memref) list -> env -> env =
  fun binds env ->
    match binds with
    | [] -> env
    | ((id, mem) :: binds_tl) ->
        env_add_list binds_tl (env_add id mem env)

let env_mem_add : ident -> memref -> memref -> heap -> heap option =
  fun id mem env_mem heap ->
    match heap_find env_mem heap with
    | Some (DataObj (EnvVal env, attrs)) ->
        let env2 = env_add id mem env in
          Some (heap_add env_mem (DataObj (EnvVal env2, attrs)) heap)
    | _ -> None

let env_mem_bind : ident -> heapobj -> memref -> heap -> heap option =
  fun id hobj env_mem heap ->
    let (mem, heap2) = heap_alloc hobj heap in
      env_mem_add id mem env_mem heap2

let rec env_mem_bind_list :
  (ident * heapobj) list -> memref -> heap -> heap option =
  fun binds env_mem heap ->
    match binds with
    | [] -> Some heap
    | ((id, hobj) :: binds_tl) ->
      match env_mem_bind id hobj env_mem heap with
      | None -> None
      | Some heap2 -> env_mem_bind_list binds_tl env_mem heap2

let env_mem_add_list :
  (ident * memref) list -> memref -> heap -> heap option =
  fun binds env_mem heap ->
    match heap_find env_mem heap with
    | Some (DataObj (EnvVal env, attrs)) ->
        let env2 = env_add_list binds env in
          Some (heap_add env_mem (DataObj (EnvVal env2, attrs)) heap)
    | _ -> None

(* Remove at the top level *)
let env_remove : ident -> env -> env =
  fun id env ->
    { env with id_map = IdentMap.remove id env.id_map }

let rec env_remove_list : ident list -> env -> env =
  fun ids env ->
    match ids with
    | [] -> env
    | (id :: ids_tl) -> env_remove_list ids_tl (env_remove id env)

(* Remove all occurrences *)
let rec env_mem_remove_all : ident -> memref -> heap -> heap option =
  fun id env_mem heap ->
    match heap_find env_mem heap with
    | Some (DataObj (EnvVal env, attrs)) ->
        let env2 = env_remove id env in
        let heap2 = heap_add env_mem (DataObj (EnvVal env2, attrs)) heap in
          if is_mem_null env.pred_mem then
            Some heap2
          else
            env_mem_remove_all id env.pred_mem heap2
    | _ -> None

let rec env_mem_remove_all_list : ident list -> memref -> heap -> heap option =
  fun ids env_mem heap ->
    match ids with
    | [] -> Some heap
    | (id :: ids_tl) ->
        match env_mem_remove_all id env_mem heap with
        | None -> None
        | Some heap2 -> env_mem_remove_all_list ids_tl env_mem heap2

(* Nesting *)
let env_nest : memref -> env =
  fun env_mem ->
    { (env_empty ()) with pred_mem = env_mem }


(* Path constraints *)
let empty_pathcons : unit -> pathcons =
  fun _ ->
    { path_list = [] }

let add_pathcons : smtexpr -> pathcons -> pathcons =
  fun smtexpr pathcons ->
    { pathcons with path_list = pathcons.path_list @ [smtexpr] }

(* Register sym mems *)
let empty_sym_mems : unit -> sym_mems =
  fun _ ->
    { mem_list = [] }

let add_sym_mems : memref -> sym_mems -> sym_mems =
  fun mem syms ->
    { syms with mem_list = syms.mem_list @ [mem] }

let mem_list_of_sym_mems : sym_mems -> memref list =
  fun syms ->
    syms.mem_list

(* Value detection *)
let is_mem_symval : memref -> heap -> bool =
  fun mem heap ->
    match heap_find mem heap with
    | Some (DataObj (Vec (SymVec _), _)) -> true
    | _ -> false

let is_mem_conc_true : memref -> heap -> bool =
  fun mem heap ->
    match heap_find mem heap with
    | Some (DataObj (Vec rvec, _)) ->
      (match (rvector_get_rint rvec 1,
              rvector_get_rfloat rvec 1,
              rvector_get_rcomplex rvec 1,
              rvector_get_rstring rvec 1,
              rvector_get_rbool rvec 1) with
      | (Some v, _, _, _, _) -> v <> rint_of_int 0
      | (_, Some v, _, _, _) -> v <> rfloat_of_float 0.0
      | (_, _, Some v, _, _) -> v <> rcomplex_of_complex Complex.zero
      | (_, _, _, Some v, _) -> true
      | (_, _, _, _, Some v) -> v <> rbool_of_bool 0
      | _ -> false)
    | Some _ -> true
    | None -> false

(* Execution state *)
let state_default : state =
  { stack = stack_empty ();
    heap = heap_empty ();
    global_env_mem = mem_null ();
    sym_mems = empty_sym_mems ();
    fresh_count = 1;
    pred_unique = 0;
    unique = 1 }
    
(* Bindings for e.g. heap_alloc that work on states. The state alloc-er will
  automatically register symbolic values with its sym_mems list. State alloc is
  used for this default behavior, and because native functions will typically get
  access to the state rather than the heap since they might need to create new
  symbolic names with name_fresh. *)
let state_alloc : heapobj -> state -> memref * state =
  fun hobj state ->
    (* Pattern match on the type of heap object being allocated to add it's location
        to the symbolic memory list if necessary. *)
    match hobj with
    | DataObj (Vec (SymVec _), _) as symobj ->
      let (memref, heap2) = heap_alloc symobj state.heap in
        (memref, { state with
                    heap = heap2;
                    sym_mems = add_sym_mems memref state.sym_mems })
    | _ -> let (memref, heap2) = heap_alloc hobj state.heap in
        (memref, { state with heap = heap2 })

(* Dereference memref in the state's heap. *)
let state_find : memref -> state -> heapobj option =
    fun mem state ->
    heap_find mem state.heap

