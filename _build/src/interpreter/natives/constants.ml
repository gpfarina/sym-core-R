
(* We do the constant folding here *)

open Syntax
module C = Complex

type numbinop =
    NumPlus
  | NumMinus
  | NumMult
  | NumDiv
  | NumPow
  | NumMod
  | NumIntDiv

  | NumGt
  | NumGe
  | NumLt
  | NumLe
  | NumEq
  | NumNeq

  | NumAndVec
  | NumAnd
  | NumOrVec
  | NumOr

type castedtype =
    IntTy
  | FloatTy
  | ComplexTy
  | NaTy

type castedpair =
    IntPair of int option * int option
  | FloatPair of float option * float option
  | ComplexPair of C.t option * C.t option

let cast_numeric_pair : numeric * numeric -> castedpair =
  fun (n1, n2) -> match (n1, n2) with
    | (Int i1, Int i2) -> IntPair (i1, i2)
    | (Int (Some i), Float f) -> FloatPair (Some (float i), f)
    | (Int None, Float f)     -> FloatPair (None, f)
    | (Int (Some i), Complex c) -> ComplexPair (Some {C.re=(float i); C.im=0.0}, c)
    | (Int None, Complex c)     -> ComplexPair (None, c)

    | (Float f, Int (Some i)) -> FloatPair (f, Some (float i))
    | (Float f, Int None)  -> FloatPair (f, None)
    | (Float f1, Float f2) -> FloatPair (f1, f2)
    | (Float (Some f), Complex c) -> ComplexPair (Some {C.re=f; C.im=0.0} , c)
    | (Float None, Complex c) -> ComplexPair (None , c)

    | (Complex c, Int (Some i)) -> ComplexPair (c, Some {C.re=float i; C.im=0.0})
    | (Complex c, Int None) -> ComplexPair (c, None)
    | (Complex c, Float (Some f)) -> ComplexPair (c, Some {C.re=f; C.im=0.0})
    | (Complex c, Float None) -> ComplexPair (c, None)
    | (Complex c1, Complex c2) -> ComplexPair (c1, c2)
    

let int_of_bool : bool -> int =
  fun b ->
    if b then 1 else 0

let int_of_bool_opt : bool option -> int option =
    fun b -> match b with
        | Some b1 -> Some (if b1 then 1 else 0)
        | None -> None

exception Numeric_Binop_Exception of numeric * numeric * string

let msg_div_by_zero : string =
  "divide by zero"

let msg_complex_mod : string =
  "complex modulo is not a supported operation in R"

let msg_complex_int_div : string =
  "complex integer division is not a supported operation in R"

let msg_complex_comp : string =
  "complex numbers do not form a naturally ordered set you uncultured swine"

let opt_op f x y =
    match x with
    | Some x1   -> begin match y with
        | Some y1   -> Some (f x1 y1)
        | None      -> None
        end
    | None      -> None

let exception_divide x y =
    if y <> 0 then
      x / y
    else
      (* Changes the error slightly TODO*)
      raise (Numeric_Binop_Exception (Int (Some x), Int (Some y), msg_div_by_zero))

let exception_float_divide x y = 
    if y <> 0.0 then
      x /. y
    else
      raise (Numeric_Binop_Exception (Float (Some x), Float (Some y), msg_div_by_zero))

let exception_complex_divide x y =
    if y.C.re <> 0.0 || y.C.im <> 0.0 then
      C.div x y
    else
      raise (Numeric_Binop_Exception (Complex (Some x), Complex (Some y), msg_div_by_zero))

let exception_float_int_divide x y =
    if y <> 0.0 then
      floor (x /. y)
    else
      raise (Numeric_Binop_Exception (Float (Some x), Float (Some y), msg_div_by_zero))

let do_numeric_binop : numbinop -> numeric -> numeric -> numeric =
  fun op n1 n2 -> match (op, cast_numeric_pair (n1, n2)) with

    | (NumPlus, IntPair (i1, i2)) -> Int (opt_op (+) i1 i2)
    | (NumPlus, FloatPair (f1, f2)) -> Float (opt_op (+.) f1 f2)
    | (NumPlus, ComplexPair (c1, c2)) -> Complex (opt_op C.add c1 c2)

    | (NumMinus, IntPair (i1, i2)) -> Int (opt_op (-) i1 i2)
    | (NumMinus, FloatPair (f1, f2)) -> Float (opt_op (-.) f1 f2)
    | (NumMinus, ComplexPair (c1, c2)) -> Complex (opt_op C.sub c1 c2)

    | (NumMult, IntPair (i1, i2)) -> Int (opt_op (fun x y -> x*y) i1 i2) (* dumb special case *)
    | (NumMult, FloatPair (f1, f2)) -> Float (opt_op (fun x y -> x*.y) f1 f2)
    | (NumMult, ComplexPair (c1, c2)) -> Complex (opt_op C.mul c1 c2)

    | (NumDiv, IntPair (i1, i2)) -> Int (opt_op exception_divide i1 i2)
    | (NumDiv, FloatPair (f1, f2)) -> Float (opt_op exception_float_divide f1 f2)
    | (NumDiv, ComplexPair (c1, c2)) -> Complex (opt_op exception_complex_divide c1 c2)

    | (NumPow, IntPair (i1, i2)) -> Float (opt_op (fun x y -> float x ** float y) i1 i2)
    | (NumPow, FloatPair (f1, f2)) -> Float (opt_op (fun x y -> x ** y) f1 f2)
    | (NumPow, ComplexPair (c1, c2)) -> Complex (opt_op C.pow c1 c2)

    | (NumMod, IntPair (i1, i2)) -> Int (opt_op (mod) i1 i2)
    | (NumMod, FloatPair (f1, f2)) -> Float (opt_op mod_float f1 f2)
    | (NumMod, ComplexPair _) ->
        raise (Numeric_Binop_Exception (n1, n2, msg_complex_mod))

    | (NumIntDiv, IntPair (i1, i2)) -> Int (opt_op exception_divide i1 i2)
    | (NumIntDiv, FloatPair (f1, f2)) -> Float (opt_op exception_float_int_divide f1 f2)
    | (NumIntDiv, ComplexPair _) ->
        raise (Numeric_Binop_Exception (n1, n2, msg_complex_int_div))

    | (NumGt, IntPair (i1, i2)) -> Int (int_of_bool_opt (opt_op (>) i1 i2))
    | (NumGt, FloatPair (f1, f2)) -> Int (int_of_bool_opt (opt_op (>) f1 f2))
    | (NumGt, ComplexPair _) ->
        raise (Numeric_Binop_Exception (n1, n2, msg_complex_comp))

    | (NumGe, IntPair (i1, i2)) -> Int (int_of_bool_opt (opt_op (>=) i1 i2))
    | (NumGe, FloatPair (f1, f2)) -> Int (int_of_bool_opt (opt_op (>=) f1 f2))
    | (NumGe, ComplexPair _) ->
        raise (Numeric_Binop_Exception (n1, n2, msg_complex_comp))

    | (NumLt, IntPair (i1, i2)) -> Int (int_of_bool_opt (opt_op (<) i1 i2))
    | (NumLt, FloatPair (f1, f2)) -> Int (int_of_bool_opt (opt_op (<) f1 f2))
    | (NumLt, ComplexPair _) ->
        raise (Numeric_Binop_Exception (n1, n2, msg_complex_comp))

    | (NumLe, IntPair (i1, i2)) -> Int (int_of_bool_opt (opt_op (<=) i1 i2))
    | (NumLe, FloatPair (f1, f2)) -> Int (int_of_bool_opt (opt_op (<=) f1 f2))
    | (NumLe, ComplexPair _) ->
        raise (Numeric_Binop_Exception (n1, n2, msg_complex_comp))

    | (NumEq, IntPair (i1, i2)) -> Int (int_of_bool_opt (opt_op (=) i1 i2))
    | (NumEq, FloatPair (f1, f2)) -> Int (int_of_bool_opt (opt_op (=) f1 f2))
    | (NumEq, ComplexPair (c1, c2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x.C.re = y.C.re && x.C.im = y.C.im) c1 c2))

    | (NumNeq, IntPair (i1, i2)) -> Int (int_of_bool_opt (opt_op (<>) i1 i2))
    | (NumNeq, FloatPair (f1, f2)) -> Int (int_of_bool_opt (opt_op (<>) f1 f2))
    | (NumNeq, ComplexPair (c1, c2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x.C.re <> y.C.re || x.C.im <> y.C.im) c1 c2))

    | (NumAndVec, IntPair (i1, i2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x<>0 && y<>0) i1 i2))
    | (NumAndVec, FloatPair (f1, f2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x<>0.0 && y<>0.0) f1 f2))
    | (NumAndVec, ComplexPair (c1, c2)) ->
        Int (int_of_bool_opt (opt_op (fun x y ->
            (x.C.re <> 0.0 || x.C.im <> 0.0) && (y.C.re <> 0.0 || y.C.im <> 0.0)) c1 c2))

    | (NumAnd, IntPair (i1, i2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x <> 0 && y <> 0) i1 i2))
    | (NumAnd, FloatPair (f1, f2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x <> 0.0 && y <> 0.0) f1 f2))
    | (NumAnd, ComplexPair (c1, c2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> (x.C.re <> 0.0 || x.C.im <> 0.0) &&
                          (y.C.re <> 0.0 || y.C.im <> 0.0)) c1 c2))
    (* TODO: vectorized operations are different! *)
    | (NumOrVec, IntPair (i1, i2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x <> 0 || y <> 0) i1 i2))
    | (NumOrVec, FloatPair (f1, f2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x <> 0.0 || y <> 0.0) f1 f2))
    | (NumOrVec, ComplexPair (c1, c2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x.C.re <> 0.0 || x.C.im <> 0.0 ||
                          y.C.re <> 0.0 || y.C.im <> 0.0) c1 c2))

    | (NumOr, IntPair (i1, i2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x <> 0 || y <> 0) i1 i2))
    | (NumOr, FloatPair (f1, f2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x <> 0.0 || y <> 0.0) f1 f2))
    | (NumOr, ComplexPair (c1, c2)) ->
        Int (int_of_bool_opt (opt_op (fun x y -> x.C.re <> 0.0 || x.C.im <> 0.0 ||
                          y.C.re <> 0.0 || y.C.im <> 0.0) c1 c2))


