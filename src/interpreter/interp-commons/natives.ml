(*
    natives.ml

    This file contains the definitions for the native calls that are supported
    by the interpreter. To make a new native call, there are a few things that
    need to happen:
    1. Register it here. Here, we create an identifier for the native call, and
    a NativeLambdaApp that actually executes the call. The "injection pairs" at
    the bottom are registered with R as actual R functions, so that when the
    interpreter is doing a native call, it appears as just another function.

    2. Edit the native_call function in interpreter/natives/native_calls.ml. That's the
    function that is actually invoked when the interpreter encounters a NativeLambdaApp.
    It's essentially a list of cases for which different calls to invoke based on the
    identifier of the NLA. More details about how to register a new function appear there.

    3. If necessary, edit interpreter/preprocess/rast_to_language.ml to generate a case
    of when your function will be called. If you're making a native function called foo
    and you expect the user to invoke it from R as foo(), then this isn't necessary, but
    if you're implementing some weird operation invoked as x >-< y, then a) the parser
    needs to get a token for >-< with precedence rules, and b), the preprocessor should
    translate the new binop to a LambdaApp using the identifier you created in step 1.

    4. Actually write the native call. More details about how to do that are in
    interpreter/natives, but the function should take one memory reference for each
    argument, and a program state. It should return a new memory reference to the output
    and a new program state (with the return value allocated in it, as well as having
    registered any side effects). The native_call function that you edited in step 2
    will push a ReturnSlot onto the stack so that the interpreter knows that it has
    successfully evaluated the function, and the return value is allocated at the
    memory reference your native function returned.
*)

open Syntax
open Support

let native_rstring : rstring =
  rstring_of_string "$native"

let native_id_of_rstring : rstring -> ident =
  fun name -> { (id_of_rstring name) with pkg = Some native_rstring }

let native_id_of_string : string -> ident =
  fun name ->
    native_id_of_rstring (rstring_of_string name)

(* Primitive type checking *)
let native_is_numeric_id : ident = native_id_of_string "is.numeric"
let nw_fun_is_numeric : (param list) * expr =
  ([Param (id_of_string "x")],
   NativeLambdaApp
    (native_is_numeric_id,
     [id_of_string "x"]))

let native_is_integer_id : ident = native_id_of_string "is.integer"
let nw_fun_is_integer : (param list) * expr =
  ([Param (id_of_string "x")],
   NativeLambdaApp
    (native_is_integer_id,
     [id_of_string "x"]))

let native_is_logical_id : ident = native_id_of_string "is.logical"
let nw_fun_is_logical : (param list) * expr =
  ([Param (id_of_string "x")],
   NativeLambdaApp
    (native_is_logical_id,
     [id_of_string "x"]))

let native_is_character_id : ident = native_id_of_string "is.character"
let nw_fun_is_character : (param list) * expr =
  ([Param (id_of_string "x")],
   NativeLambdaApp
    (native_is_character_id,
     [id_of_string "x"]))

let native_is_complex_id : ident = native_id_of_string "is.complex"
let nw_fun_is_complex : (param list) * expr =
  ([Param (id_of_string "x")],
   NativeLambdaApp
    (native_is_complex_id,
     [id_of_string "x"]))

let native_is_null_id : ident = native_id_of_string "is.null"
let nw_fun_is_null : (param list) * expr =
  ([Param (id_of_string "x")],
   NativeLambdaApp
    (native_is_null_id,
     [id_of_string "x"]))

let native_is_na_id : ident = native_id_of_string "is.na"
let nw_fun_is_na : (param list) * expr =
  ([Param (id_of_string "x")],
   NativeLambdaApp
    (native_is_complex_id,
     [id_of_string "x"]))


(* Vector subsetting *)
let native_vector_subscript_id: ident = native_id_of_string "vector.subscript"
let nw_fun_vec_subscript : (param list) * expr =
  ([Param (id_of_string "vector"); Param (id_of_string "sub")],
   NativeLambdaApp
     (native_vector_subscript_id,
      [id_of_string "vector"; id_of_string "sub"]))

let native_vector_subset_id : ident = native_id_of_string "vector.subset"
let nw_fun_vec_subset : (param list) * expr =
  ([Param (id_of_string "vector"); VarParam],
   NativeLambdaApp
     (native_vector_subset_id,
      [id_of_string "vector"; id_variadic ()]))

(* Vector making *)
let native_vector_make_id : ident = native_id_of_string "vector.make"
let nw_fun_vec_make : (param list) * expr =
  ([VarParam],
   NativeLambdaApp
    (native_vector_make_id,
     [id_variadic ()]))

(* Vector length *)
let native_vector_length_id: ident = native_id_of_string "vector.length"
let nw_fun_vec_length : (param list) * expr =
  ([Param (id_of_string "data")],
  NativeLambdaApp
      (native_vector_length_id,
      [id_of_string "data"]))

let native_vector_sum_id : ident = native_id_of_string "vector.sum"
let nw_fun_vec_sum : (param list) * expr =
  ([Param (id_of_string "vec")],
   NativeLambdaApp
    (native_vector_sum_id,
    [id_of_string "vec"]))

(* Vector binary operations *)

(* Sets up a native lambda app for a function with name ident that takes two arguments:
 vector1 and vector2. Technically can be used for anything that expects two arguments
 even if it isn't strictly a binop. *)
let vec_binop: ident -> param list * expr =
    fun id ->
    ([Param (id_of_string "vector1"); Param (id_of_string "vector2")],
    NativeLambdaApp
        (id,
        [id_of_string "vector1"; id_of_string "vector2"]))

let vec_uop: ident -> param list * expr =
    fun id ->
    ([Param (id_of_string "vector1")],
    NativeLambdaApp
        (id,
        [id_of_string "vector1"]))

(* Colon *)
let native_vector_colon_id = native_id_of_string "vector.colon"
let nw_fun_vec_colon = vec_binop (native_vector_colon_id)

let native_vector_add_id : ident = native_id_of_string "vector.add"
let nw_fun_vec_add: param list * expr =
    vec_binop (native_vector_add_id)

let native_vector_mul_id = native_id_of_string "vector.mul"
let nw_fun_vec_mul = vec_binop (native_vector_mul_id)

let native_vector_div_id = native_id_of_string "vector.div"
let nw_fun_vec_div: param list * expr = vec_binop (native_vector_div_id)

let native_vector_sub_id = native_id_of_string "vector.sub"
let nw_fun_vec_sub = vec_binop (native_vector_sub_id)

let native_vector_mod_id = native_id_of_string "vector.mod"
let nw_fun_vec_mod = vec_binop (native_vector_mod_id)

let native_vector_exp_id = native_id_of_string "vector.exp"
let nw_fun_vec_exp = vec_binop (native_vector_exp_id)

(* Comparison *)
let native_vector_lt_id = native_id_of_string "vector.lt"
let nw_fun_vec_lt = vec_binop (native_vector_lt_id)

let native_vector_gt_id = native_id_of_string "vector.gt"
let nw_fun_vec_gt = vec_binop (native_vector_gt_id)

let native_vector_eq_id = native_id_of_string "vector.eq"
let nw_fun_vec_eq = vec_binop (native_vector_eq_id)

let native_vector_neq_id = native_id_of_string "vector.neq"
let nw_fun_vec_neq = vec_binop (native_vector_neq_id)

let native_vector_geq_id = native_id_of_string "vector.geq"
let nw_fun_vec_geq = vec_binop (native_vector_geq_id)

let native_vector_leq_id = native_id_of_string "vector.leq"
let nw_fun_vec_leq = vec_binop (native_vector_leq_id)

(* Logic *)
let native_vector_andvec_id = native_id_of_string "vector.andvec"
let nw_fun_vec_andvec = vec_binop (native_vector_andvec_id)

let native_vector_and_id = native_id_of_string "vector.and"
let nw_fun_vec_and = vec_binop (native_vector_and_id)

let native_vector_orvec_id = native_id_of_string "vector.orvec"
let nw_fun_vec_orvec = vec_binop (native_vector_orvec_id)

let native_vector_or_id = native_id_of_string "vector.or"
let nw_fun_vec_or = vec_binop (native_vector_or_id)

let native_vector_xor_id = native_id_of_string "vector.xor"
let nw_fun_vec_xor = vec_binop (native_vector_xor_id)

(* make.symbolic(4, "int") creates a symbolic vector of 4 integers *)
let native_vector_make_symbolic_id = id_of_rstring (Some "make.symbolic")
let nw_fun_vec_sym: (param list * expr) =
    ([Param (id_of_string "length"); Param (id_of_string "type")],
    NativeLambdaApp
        (native_vector_make_symbolic_id,
        [id_of_string "length"; id_of_string "type"]))

(* Conversion *)
let native_vector_intconv_id = id_of_rstring (Some "as.integer")
let nw_fun_vec_intconv = vec_uop native_vector_intconv_id
let native_vector_floatconv_id = id_of_rstring (Some "as.float")
let nw_fun_vec_floatconv = vec_uop native_vector_floatconv_id
let native_vector_complexconv_id = id_of_rstring (Some "as.complex")
let nw_fun_vec_complexconv = vec_uop native_vector_complexconv_id
let native_vector_strconv_id = id_of_rstring (Some "as.string")
let nw_fun_vec_strconv = vec_uop native_vector_strconv_id
let native_vector_boolconv_id = id_of_rstring (Some "as.bool")
let nw_fun_vec_boolconv = vec_uop native_vector_boolconv_id

(* Pairs to inject with *)
let native_injection_pairs : (ident * (param list * expr)) list =
  [
    (native_is_numeric_id, nw_fun_is_numeric);
    (native_is_integer_id, nw_fun_is_integer);
    (native_is_logical_id, nw_fun_is_logical);
    (native_is_character_id, nw_fun_is_character);
    (native_is_complex_id, nw_fun_is_complex);
    (native_is_null_id, nw_fun_is_null);
    (native_is_na_id, nw_fun_is_na);

    (native_vector_subscript_id, nw_fun_vec_subscript);
    (native_vector_subset_id, nw_fun_vec_subset);
    (native_vector_make_id, nw_fun_vec_make);
    (native_vector_length_id, nw_fun_vec_length);
    (native_vector_sum_id, nw_fun_vec_sum);
    (native_vector_colon_id, nw_fun_vec_colon);

    (native_vector_add_id, nw_fun_vec_add);
    (native_vector_mul_id, nw_fun_vec_mul);
    (native_vector_div_id, nw_fun_vec_div);
    (native_vector_sub_id, nw_fun_vec_sub);
    (native_vector_mod_id, nw_fun_vec_mod);
    (native_vector_exp_id, nw_fun_vec_exp);

    (native_vector_lt_id, nw_fun_vec_lt);
    (native_vector_gt_id, nw_fun_vec_gt);
    (native_vector_eq_id, nw_fun_vec_eq);
    (native_vector_neq_id, nw_fun_vec_neq);
    (native_vector_geq_id, nw_fun_vec_geq);
    (native_vector_leq_id, nw_fun_vec_leq);

    (native_vector_andvec_id, nw_fun_vec_andvec);
    (native_vector_and_id, nw_fun_vec_and);
    (native_vector_orvec_id, nw_fun_vec_orvec);
    (native_vector_or_id, nw_fun_vec_or);
    (native_vector_xor_id, nw_fun_vec_xor);

    (native_vector_make_symbolic_id, nw_fun_vec_sym);

    (native_vector_intconv_id, nw_fun_vec_intconv);
    (native_vector_floatconv_id, nw_fun_vec_floatconv);
    (native_vector_complexconv_id, nw_fun_vec_complexconv);
    (native_vector_strconv_id, nw_fun_vec_strconv);
    (native_vector_boolconv_id, nw_fun_vec_boolconv);
  ]

