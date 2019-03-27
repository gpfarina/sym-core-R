(* 
    type_convert.ml

    UNUSED
    Contains some old type conversion code.
    Kept around because it holds some of R's type conversion
    rules that are strange.
*)

module S = Syntax

(* Based on R's coerce.c *)
(* TODO: better error messaging *)

int_of_bool = function
    | true  -> 1
    | false -> 0

let int_to_bool: int option -> S.expr =
    function
    | None      -> S.Const (S.Bool None)
    | Some x    -> S.Const(S.Bool (Some (x<>0)))

let float_to_bool: float option -> S.expr =
    function
    | None      -> S.Const (S.Bool None)
    | Some x    -> let b = if x<>x then S.Bool None (* x<>x means x is NaN *)
                    else S.Bool (x<>0) in S.Const(b)

let complex_to_bool: Complex.complex option -> S.expr =
    function
    | None      -> S.Const (S.Bool None)
    | Some x    -> let b = if (x.re <> x.re) || (x.im <> x.im) then S.Bool None
                    else S.Bool (x.re <> 0) || (x.im <> 0) in S.Const(b)

let str_to_bool_help: string -> int option =
    function
    | "T"       -> Some 1
    | "TRUE"    -> Some 1
    | "True"    -> Some 1
    | "true"    -> Some 1
    | "F"       -> Some 0
    | "FALSE"   -> Some 0
    | "False"   -> Some 0
    | "false"   -> Some 0
    | _         -> None

let string_to_bool: string option -> S.expr =
    function
    | None      -> S.Const (S.Bool None)
    | Some s    -> S.Const (S.Bool (str_to_bool_help s))

let bool_to_int: int option -> S.expr =
    function
    | None      -> S.Const (S.Num (S.Int None))
    | Some b    -> S.Const (S.Num (S.Int (Some b)))

let float_to_int_help: float option -> int option = function
    | Some x when x > max_int || x < min_int -> let _ = Printf.printf 
        "Float conversion exceeds integer bounds" in None
    | Some x -> Some (int_of_float x)
    | None -> None

(* TODO: is there a separate float NA? *)
let float_to_int: float option -> S.expr =
    function
    | x      -> S.Const (S.Num (S.Int (float_to_int_help x)))

let complex_to_int_help: Complex.complex option -> int option = function
    | Some x when x.re > max_int    -> None
    | Some x when x.im > max_int    -> None
    | Some x when x.im <> 0         -> let _ = Printf.printf "Implicit conversion of complex" in
        Some (int_of_float x.re)
    | Some x                        -> Some (int_of_float x.re)
    | None     -> None

let complex_to_int: Complex.complex option -> S.expr =
    function
    | x      -> S.Const (S.Num (S.Int (complex_to_int_help x)))

(* TODO: R's conversion may be slightly different *)
let string_to_int: string option -> S.expr =
    function
    | None      -> S.Const (S.Num (S.Int None))
    | Some s    -> S.Const (S.Num (S.Int (Some (int_of_string s))))

let bool_to_float: int option -> S.expr =
    function
    | None      -> S.Const (S.Num (S.Float None))
    | Some b    -> S.Const (S.Num (S.Float (Some (float_of_int b))))

let int_to_float: int option -> S.expr =
    function
    | None      -> S.Const (S.Num (S.Float None))
    | Some x    -> S.Const (S.Num (S.Float (float_of_int x)))

let complex_to_float_help: Complex.complex option -> float option = function
    | Some x -> begin match x.im with
        | 0.0 -> let _ = Printf.printf("Implicit conversion of complex") in Some(x.re)
                end
    | None  -> None

let complex_to_float: Complex.complex option -> S.expr =
    function
    | x -> S.Const (S.Num (S.Float (complex_to_float_help x)))

let string_to_float: string option -> S.expr =
    function
    | None       -> S.Const (S.Num (S.Float None))
    | Some s     -> S.Const (S.Num (S.Float (Some (float_of_string s))))

let bool_to_complex: int option -> S.expr =
    function
    | None      -> S.Const (S.Num (S.Complex None))
    | Some b    -> S.Const (S.Num (S.Complex (Some ({re=(float_of_int b), im=0}))))

(* TODO: what if x is NaN? *)
let float_to_complex: float option -> S.expr =
    function
    | None      -> S.Const (S.Num (S.Complex None)) (* TODO: R has a complex number made from two NA_REALs *)
    | Some x    -> S.Const (S.Num (S.Complex (Some {re=x, im=0})))

(* TODO: see /src/coerce.c for R's implementation *)
let string_to_complex: Complex.complex option -> S.expr =
    function
    | _ -> failwith "String to Complex unimplemented!"

let bool_to_string: int option -> S.expr =
    function
    | None      -> S.Const (S.String None)
    | Some x    -> S.Const (S.String (if x = 0 then "False" else "True"))

let int_to_string: int option -> S.expr =
    function
    | S.Const (S.Num (S.Int None))      -> S.Const (S.String None)
    | S.Const (S.Num (S.Int (Some x)))  -> S.Const (S.String (Some (string_of_int x)))

(* TODO: this formatting is probably subtly wrong *)
let complex_to_string: Complex.complex option -> S.expr =
    function
    | None      -> S.Const (S.String None)
    | Some x    -> let s = Printf.sprintf "%f + %fi" x.re x.im in
        S.Const (S.String (Some s))

(* TODO: where is this useful? *)
let coerce_to_symbol: S.expr -> S.expr =
    function
    | _ -> failwith "to_symbol unimplemented!"

let coerce_to_bool: S.expr -> S.expr =
    function
    | S.Const (S.Bool b)            -> S.Const (S.Bool b) (* TODO: do we need this? *)
    | S.Const (S.Num (S.Int i))     -> int_to_bool i
    | S.Const (S.Num (S.Float f))   -> float_to_bool f
    | S.Const (S.Num (S.Complex c)) -> complex_to_bool c
    | S.Const (S.String s)          -> string_to_bool s
    | _                             -> failwith "Unimplemented to_bool type"

let coerce_to_int: S.expr -> S.expr =
    function
    | S.Const (S.Bool b)            -> bool_to_int b
    | S.Const (S.Num (S.Int i))     -> S.Const (S.Num (S.Int i))
    | S.Const (S.Num (S.Float f))   -> float_to_int f
    | S.Const (S.Num (S.Complex c)) -> complex_to_int c
    | S.Const (S.String s)          -> string_to_int s
    | _                             -> failwith "Unimplemented to_int type"

let coerce_to_float: S.expr -> S.expr =
    function
    | S.Const (S.Bool b)            -> bool_to_float b
    | S.Const (S.Num (S.Int i))     -> int_to_float i
    | S.Const (S.Num (S.Float f))   -> S.Const (S.Num (S.Float f))
    | S.Const (S.Num (S.Complex c)) -> complex_to_float c
    | S.Const (S.String s)          -> string_to_float s
    | _                             -> failwith "Unimplemented to_int type"

let coerce_to_complex: S.expr -> S.expr =
    function
    | S.Const (S.Bool b)            -> bool_to_complex b
    | S.Const (S.Num (S.Int i))     -> int_to_complex i
    | S.Const (S.Num (S.Float f))   -> float_to_complex f
    | S.Const (S.Num (S.Complex c)) -> S.Const (S.Num (S.Complex c))
    | S.Const (S.String s)          -> string_to_complex s
    | _                             -> failwith "Unimplemented to_int type"

let coerce_to_string: S.expr -> S.expr =
    function
    | S.Const (S.Bool b)            -> bool_to_string b
    | S.Const (S.Num (S.Int i))     -> int_to_string i
    | S.Const (S.Num (S.Float f))   -> float_to_string f
    | S.Const (S.Num (S.Complex c)) -> complex_to_string c
    | S.Const (S.String s)          -> S.Const (S.String s)
    | _                             -> failwith "Unimplemented to_int type"

let coerce_to_expr: S.expr -> S.expr =
    function
    | _ -> failwith "to_expr unimplemented"

let coerce_to_vec: S.expr -> S.expr =
    function
    | _ -> failwith "to_vec unimplemented"

let coerce_to_ty S.expr -> S.basic_rty -> S.expr =
    function
    | _ => failwith "unimplemented"

(* TODO coerce_vector_to_type coercion *)

(* TODO: S.val -> S.ty/S.symbol -> S.val or something? 
 do_as takes an evaluated expression (a value), and
 converts it to another evaluated expression of a different type.
 Eventually this should do some form of dynamic dispatch first.
*)

let do_as: S.expr -> string -> S.expr =
fun e s ->
    match s with
    | "as.character"    -> coerce_to_string e
    | "as.integer"      -> coerce_to_int e
    | "as.double"       -> coerce_to_float e
    | "as.complex"      -> coerce_to_complex e
    | "as.logical"      -> coerce_to_bool e
    | "as.raw"          -> failwith "raw unimplemented"

