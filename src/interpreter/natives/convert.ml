(*
    convert.ml

    Essentially arithmetic.ml for vector conversion functions.
    Used in conjunction with convert_vector_mems from vector.ml

*)

module S = Support

(* For using ex. float_of_int to convert float option to int option *)
let convert_opt: ('a -> 'b) -> 'a option -> 'b option =
    fun f ao ->
    match ao with
    | Some a -> Some (f a)
    | None -> None

(* Newer versions of ocaml have these in standard library *)
let float_of_string_opt: string -> float option =
    fun s ->
    try Some (float_of_string s)
    with Failure "float_of_string" -> None

let int_of_string_opt: string -> int option =
    fun s ->
    try Some (int_of_string s)
    with Failure "int_of_string" -> None

(* TODO: ocaml's string conversion might be different from R's *)
let float_opt_of_string_opt: string option -> float option =
    fun so ->
    match so with
    | Some s -> float_of_string_opt s
    | None -> None

let int_opt_of_string_opt: string option -> int option =
    fun so ->
    match so with
    | Some s -> int_of_string_opt s
    | None -> None

(* TODO: other conversion nuances cf. type_convert.ml *)

(* TODO: warn when discarding imaginary parts of complex *)
let rvector_as_integer: S.rvector -> S.rvector =
    function
    | S.IntVec i -> S.IntVec (Array.copy i) (* TODO: is this copy necessary? *)
    | S.FloatVec f -> S.IntVec (Array.map (convert_opt int_of_float) f)
    | S.ComplexVec c -> S.IntVec (Array.map (convert_opt (fun cplx -> int_of_float cplx.Complex.re)) c)
    | S.StrVec s -> S.IntVec (Array.map int_opt_of_string_opt s)
    | S.BoolVec b -> S.IntVec (Array.copy b) (* TODO: is this copy necessary? *)
    | S.SymVec _ -> failwith "Symbolic conversion not implemented"

(* R calls its float vector conversion as.double *)
let rvector_as_float: S.rvector -> S.rvector =
    function
    | S.IntVec i -> S.FloatVec (Array.map (convert_opt float_of_int) i)
    | S.FloatVec f -> S.FloatVec (Array.copy f)
    | S.ComplexVec c -> S.FloatVec (Array.map (convert_opt (fun cplx -> cplx.Complex.re)) c)
    | S.StrVec s -> S.FloatVec (Array.map float_opt_of_string_opt s)
    | S.BoolVec b -> S.FloatVec (Array.map (convert_opt float_of_int) b)
    | S.SymVec _ -> failwith "Symbolic conversion not implemented"

let cplx_of_int: S.rint -> S.rcomplex =
    convert_opt (fun i ->
        {Complex.re = (float_of_int i); Complex.im = 0.0})

let cplx_of_float: S.rfloat -> S.rcomplex =
    convert_opt (fun f ->
        {Complex.re = f; Complex.im = 0.0})

let rvector_as_complex: S.rvector -> S.rvector =
    function
    | S.IntVec i -> S.ComplexVec (Array.map cplx_of_int i)
    | S.FloatVec f -> S.ComplexVec (Array.map cplx_of_float f)
    | S.ComplexVec c -> S.ComplexVec (Array.copy c)
    | S.StrVec s -> failwith "No string to complex conversion" (* TODO *)
    | S.BoolVec b -> S.ComplexVec (Array.map cplx_of_int b)
    | S.SymVec _ -> failwith "Symbolic conversion not implemented"

let string_of_logical: S.rbool -> S.rstring =
    function
    | Some 0 -> Some "FALSE"
    | Some 1 -> Some "TRUE"
    | Some _ -> failwith "Invalid logical in string conversion"
    | None -> None

(* R calls this as.character() *)
let rvector_as_string: S.rvector -> S.rvector =
    function
    | S.IntVec i -> S.StrVec (Array.map (convert_opt string_of_int) i)
    | S.FloatVec f -> S.StrVec (Array.map (convert_opt string_of_float) f)
    | S.ComplexVec c -> failwith "No complex to string conversion" (* TODO *)
    | S.StrVec s -> S.StrVec (Array.copy s)
    | S.BoolVec b -> S.StrVec (Array.map string_of_logical b)
    | S.SymVec _ -> failwith "Symbolic conversion not implemented"

let logical_of_int: S.rint -> S.rbool =
    function
    | Some 0 -> Some 0
    | None -> None
    | _ -> Some 1

let logical_of_string: S.rstring -> S.rbool =
    function
    | Some "T" -> Some 1
    | Some "TRUE" -> Some 1
    | Some "True" -> Some 1
    | Some "true" -> Some 1
    | Some "F" -> Some 0
    | Some "FALSE" -> Some 0
    | Some "False" -> Some 0
    | Some "false" -> Some 0
    | Some _ -> None
    | None -> None

let logical_of_float: S.rfloat -> S.rbool =
    function
    | Some f ->
        (* If f is NaN then NA_BOOL *)
        if f <> f then None else
        (* If f is 0 then false *)
        if f = 0.0 then Some 0 else
        Some 1
    | None -> None

let logical_of_complex: S.rcomplex -> S.rbool =
    function
    | Some c ->
        (* Check for either part being NaN *)
        if (c.Complex.re <> c.Complex.re) || (c.Complex.im <> c.Complex.im) then None else
        (* Nonzero real OR complex part means true *)
        if (c.Complex.re = 0.0) && (c.Complex.im = 0.0) then Some 0 else
        Some 1
    | None -> None

(* R calls this as.logical() *)
let rvector_as_bool: S.rvector -> S.rvector =
    function
    | S.IntVec i -> S.BoolVec (Array.map logical_of_int i)
    | S.FloatVec f -> S.BoolVec (Array.map logical_of_float f)
    | S.ComplexVec c -> S.BoolVec (Array.map logical_of_complex c)
    | S.StrVec s -> S.BoolVec (Array.map logical_of_string s)
    | S.BoolVec b -> S.BoolVec (Array.copy b)
    | S.SymVec _ -> failwith "Symbolic conversion not implemented"

