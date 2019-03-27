(*
  rast_to_language.ml
  
  Converts from the richer parser AST from parser/rast.ml to the lower-level
  runtime representation in language/syntax.ml.
*)

module R = Rast
module L = Syntax
module T = Support
module N = Natives
open Complex
open Natives
open List

let ident_count = ref 32

(* Create new unique identifiers. $ is used in the name because $ is not a valid R
  identifier, so we can be sure this doesn't shadow some user-defined name. Used in 
  (ex.) for-loops to hold the index variable when translating down to while. 
  rident is for rast.ml identifiers, while lident is for syntax.ml ids. *)
let fresh_rident _: 'a R.ident =
    let next = !ident_count in
    let _ = incr ident_count in
    {R.pkg = None; R.name = "t$syn_" ^ string_of_int next; R.tag = None; R.src=None}

let fresh_lident _: 'a L.ident =
    let next = !ident_count in
    let _ = incr ident_count in
    {L.pkg = None; L.name = Some ("t$syn_" ^ string_of_int next); L.tag = None}

(* Convert a unary operator to a function call. *)
let uop_to_ident: R.unop -> 'a L.ident =
    fun u -> {L.pkg = Some N.native_rstring;
              L.name = Some (R.string_of_unop u);
              L.tag = None}

(* Convert a binary operator to a native function call.
  If there's no definted native function call for the operator,
  we just convert it by name with the printing code. *)
let bop_to_ident : R.binop -> 'a L.ident =
  fun b ->
    match b with
    | R.Plus -> native_vector_add_id
    | R.Mult -> native_vector_mul_id
    | R.Div -> native_vector_div_id
    | R.Minus -> native_vector_sub_id
    | R.Mod -> native_vector_mod_id
    | R.Pow -> native_vector_exp_id
    (*
    | IntDiv
    | MatrixMult
    | OuterProd
    | KronProd
    | Match
    *)
    | R.Gt -> native_vector_gt_id
    | R.Ge -> native_vector_geq_id
    | R.Lt -> native_vector_lt_id
    | R.Le -> native_vector_leq_id
    | R.Eq -> native_vector_eq_id
    | R.Neq -> native_vector_neq_id
    | R.AndVec -> native_vector_andvec_id
    | R.And -> native_vector_and_id
    | R.OrVec -> native_vector_orvec_id
    | R.Or -> native_vector_or_id
    (*
    | Form
    | Help
    | ObjAttr
    *)
    | _ -> native_id_of_string (R.string_of_binop b)

let convert_ident: 'a R.ident -> 'a L.ident =
    fun i -> match i.R.pkg with
      | None -> { L.pkg = None; L.name = Some i.R.name; L.tag = i.R.tag}
      | Some s -> {L.pkg = Some i.R.pkg; L.name = Some i.R.name; L.tag = i.R.tag}

let convert_numeric: R.numeric -> L.numeric =
    function
    | R.Int n   -> L.Int (Some n)
    | R.Float f -> L.Float (Some f)
    | R.Complex (f1, f2) -> L.Complex (Some {re = f1; im = f2})
    | R.Na      -> L.Int (None) (* TODO *)

(* TODO: ObjAttr (and convert idents found on the right into strings) *)
(* Convert a rast.ml expression to a syntax.ml expression.
    A parsed program is [R.expr], and we want to produce [L.expr] to run the
    interpreter on. *)
let rec convert_expr: 'a R.expr -> ('a, 'b) L.expr =
    function
    | R.NumericConst n -> L.Const (L.Num (convert_numeric n))
    | R.StringConst s  -> L.Const (L.Str (Some s))
    | R.BoolConst b    -> let b_num = if b then Some 1 else Some 0 in
                                L.Const (L.Bool b_num)
    | R.Null           -> L.Const (L.Num (L.Int (Some 0))) (* TODO *)
    | R.Ident i        -> L.Ident (convert_ident i)

    (* Some of the unary operators are implemented as native calls, which
        means invoking the native ids associated with those operators. *)
    | R.Uop (u, e) ->
        let u_ident = uop_to_ident u in
        let c_expr  = convert_expr e in
        begin match u with
        | R.UMinus ->
          L.LambdaApp (L.Ident native_vector_sub_id,
            [L.Arg (L.Const (L.Num (L.Int (Some 0))));
             L.Arg c_expr])

        | R.UPlus ->
          L.LambdaApp
            (L.Ident native_vector_mul_id,
            [L.Arg c_expr;
             L.Arg (L.LambdaApp
                    (L.Ident native_vector_geq_id,
                    [L.Arg c_expr; L.Arg (L.Const (L.Num (L.Int (Some 0))))]))])

        | R.Not ->
          L.LambdaApp (L.Ident native_vector_eq_id,
                      [L.Arg c_expr; L.Arg (L.Const (L.Num (L.Int (Some 0))))])

        | R.UForm -> L.LambdaApp (L.Ident u_ident,
            [L.Arg c_expr])
        | R.UHelp -> L.LambdaApp (L.Ident u_ident,
            [L.Arg c_expr])
    end
    (* Assignment special cases. When we do x[[2]] <- 3 in R, it doesn't happen like it
        does in many languages. In a C-like language, x[2] is an operation that gets a
        memory reference, and the assignment is done onto that memory location. Because
        R uses pass by deep-copy, x[[2]] is just a vector of length 1, and assigning 
        to it doesn't really do anything.
        To make this behavior work, R translates x[2] <- 3 differently: as a function call to the
        '[<-' function. When we do x[[2]] <- 3, the [<- function creates a copy of x, and assigns
        3 into the memory of the copy, then updates the name x to map to the copy. This also means
        that if you have y=x; x[[2]] <- 3, the operation will not affect y.
        The same hack is used for all operations that make it seem like you can assign to some
        part of an object in R. These translation cases handle that behavior by invoking
        native calls that will do the right thing. *)
    (* length<- *)
    | R.Bop (R.Assign,
        R.FuncCall (R.Ident {R.name="length";_}, args),e)
                            -> assign_special_body "length" args e None
    (* [<- *)
    | R.Bop (R.Assign,
        R.ListSub (e_vec, args), e_val)
                            -> assign_special_body "[" args e_vec (Some e_val)
    (* [[<- and $<- *) (* TODO: does $ really work the same way? *)
    | R.Bop (R.Assign,
        R.ListProj (e_vec, args), e_val)
                            -> assign_special_body "[[" args e_vec (Some e_val)
    (* dimnames<- *)
    | R.Bop (R.Assign,
        R.FuncCall (R.Ident {R.name="dimnames";_}, args), e)
                            -> assign_special_body "dimnames" args e None
    (* dim<- *)
    | R.Bop (R.Assign,
        R.FuncCall (R.Ident {R.name="dims";_}, args), e)
                            -> assign_special_body "dims" args e None
    (* names<- *)
    | R.Bop (R.Assign,
        R.FuncCall (R.Ident {R.name="names";_}, args), e)
                            -> assign_special_body "names" args e None
    (* levels<- *)
    | R.Bop (R.Assign,
        R.FuncCall (R.Ident {R.name="levels";_}, args), e)
                            -> assign_special_body "levels" args e None
    | R.Bop (R.Assign, R.Ident id, rhs) ->
                                L.Assign (L.Ident (convert_ident id),
                                          convert_expr rhs)
    | R.Bop (R.Assign, R.StringConst str, rhs) ->
                                L.Assign (L.Const (L.Str (T.rstring_of_string str)),
                                          convert_expr rhs)
    | R.Bop (R.Range, e1, e2) ->
                              L.LambdaApp (L.Ident native_vector_colon_id,
                                 map convert_arg [R.ExprArg e1; R.ExprArg e2])
                                      
    | R.Bop (op, e1, e2)    -> let b_ident = bop_to_ident op in
                                let c_e1 = convert_expr e1 in
                                let c_e2 = convert_expr e2 in
                                L.LambdaApp (L.Ident b_ident,
                                [L.Arg c_e1; L.Arg c_e2])
    | R.FuncCall (R.Ident { R.name = "return" }, []) ->
                                L.Return (L.Const L.Nil)
    | R.FuncCall (R.Ident { R.name = "return" }, arg :: []) ->
                                (match convert_arg arg with
                                | L.Arg e -> L.Return e
                                | _ -> failwith "default or variadic return")
    | R.FuncCall (R.Ident { R.name = "return" }, a :: b :: _) ->
                                failwith "too many arguments passed to return"
    | R.FuncCall (R.Ident { R.name = "c" }, args) ->
                                L.LambdaApp (L.Ident native_vector_make_id,
                                           map convert_arg args)
    | R.FuncCall (R.Ident { R.name = "length" }, args) ->
                                L.LambdaApp (L.Ident native_vector_length_id,
                                           map convert_arg args)
    | R.FuncCall (e, args)  -> let c_args = map convert_arg args in
                                let c_e = convert_expr e in
                                L.LambdaApp(c_e, c_args)
    | R.FuncDec (params, e) -> let c_params = map convert_param params in
                                let c_e = convert_expr e in
                                L.LambdaAbs (c_params, c_e)
    | R.Block []            -> convert_expr R.Null
    | R.Block es            -> begin match es with
                                | [hd]      -> convert_expr hd
                                | hd :: tl  -> L.Seq (map convert_expr es)
                                | []        -> convert_expr R.Null (* TODO: is this really what R does? *)
                                end
    | R.ListProj (e, args)  -> L.LambdaApp (L.Ident (native_vector_subscript_id),
            L.Arg (convert_expr e) :: (map convert_arg args))
    | R.ListSub (e, args)  -> L.LambdaApp (L.Ident (native_vector_subset_id),
            L.Arg (convert_expr e) :: (map convert_arg args))
    | R.If (e1, e2)         -> L.If (convert_expr e1, convert_expr e2, convert_expr R.Null) (* TODO ^ *)
    | R.IfElse (e1, e2, e3) -> L.If (convert_expr e1, convert_expr e2, convert_expr e3)
    (* for (x in y) body translates to the code:
        tmp_length = len(y);
        tmp_index = 1;
        while (tmp_index <= tmp_length)
        {
            x = y[tmp_index];
            tmp_index++;
            body
        }

        where tmp_index and tmp_length are special identifiers to avoid shadowing.
        *)
    | R.For ((i, e1), e2)   ->
        (* tmp_index  holds the INDEX in the vector we're iterating over *)
        let tmp_index = fresh_rident () in
        let init = R.Bop (R.Assign, R.Ident tmp_index, R.NumericConst (R.Int 1)) in
        (* tmp_length holds the LENGTH of the vector we're iterating over *)
        let tmp_length = fresh_rident () in
        let init2 = L.Assign (L.Ident (convert_ident tmp_length),
            L.LambdaApp(L.Ident native_vector_length_id, [L.Arg (convert_expr e1)])) in
        (* Use Le since the vector is 1-indexed in the R code *)
        let cond = R.Bop (R.Le, R.Ident tmp_index, R.Ident tmp_length) in
        let access = R.Bop (R.Assign, R.Ident i,
            R.ListProj(e1, [R.ExprArg (R.Ident tmp_index)])) in
        let incr = R.Bop (R.Assign, R.Ident tmp_index, R.Bop (R.Plus, R.Ident tmp_index, R.NumericConst (R.Int 1))) in
        let block = begin match e2 with
        | R.Block es        -> R.Block ([access; incr] @ es)
        | e                 -> R.Block [access; incr; e]
        end in
        let loop = R.While (cond, block) in
        L.Seq [convert_expr init; init2; convert_expr loop]
    | R.While (e1, e2)      -> L.While (convert_expr e1, convert_expr e2)
    (* Repeat translates to while (True). *)
    | R.Repeat e            -> L.While (L.Const (L.Bool (Some 1)), convert_expr e)
    | R.Next                -> L.Next
    | R.Break               -> L.Break

(* Converts a rast.ml argument into a syntax.ml argument.
    There's some confusion here because R arguments are complicated:
    1. You can refer to arguments using strings. So if foo = function (a) {},
    it's valid to do
        foo("a"=3)
    2. You can have "empty" arguments. From the example before
        foo(a=,2)
    is valid syntax, and binds a=2 in the arguments. As far was we can tell,
    the arguments are bound to values in order, so if we have bar = function (a,b) {},
        bar(3,b=,4,"a"=)
    will bind a=3 and b=4. It's unclear why R would allow you to pass more formal parameters
    than a function requires, and what the user is trying to do if they write this code.
    We do not implement this behavior.
    As far as we can tell
        foo(a=)
    will bind a=NULL in the arguments, but we don't implement this either.
    3. We're not entirely sure what the null assignment means semantically. There are
    some functions where you may pass NULL=, but the language definition doesn't give a clue
    as to how to interpret it. Possibly this is telling the function to replace returning a null with
    returning something else, but since we don't have any evidence for this, we do not implement
    this behavior. *)
and convert_arg: 'a R.arg -> ('a, 'b) L.arg =
    function
    | R.EmptyArg            -> L.Arg (L.Const L.Nil)
    | R.ExprArg (R.Ident { R.name = "..." }) -> L.VarArg
    | R.ExprArg e           -> let c_expr = convert_expr e in
                                L.Arg c_expr
    | R.IdentAssign (i, e)  -> L.Named (convert_ident i, convert_expr e)
    (* | R.IdentAssignEmpty i  -> L.Named (convert_ident i, convert_expr R.Null) *)
    | R.IdentAssignEmpty i  -> failwith "Empty assign not part of Core R!"

    | R.StringAssign (s, e) -> L.Named (convert_ident {R.default_ident with R.name = s}, convert_expr e)
    (* | R.StringAssignEmpty s -> L.Named (convert_ident {R.default_ident with name = s}, convert_expr R.Null) *)
    | R.StringAssignEmpty s -> failwith "Empty assign not part of Core R!"

    | R.NullAssign e        -> failwith "Null Assign not part of Core R!" (* TODO: what the heck *)
    | R.NullAssignEmpty     -> failwith "Null Assign Empty not part of Core R!" (* TODO: what the heck *)
    | R.ArgDots             -> L.VarArg

and convert_param: 'a R.param -> ('a, 'b) L.param =
    function
    | R.Param i             -> if i.R.name = "..." then
                                 L.VarParam
                               else
                                 L.Param (convert_ident i)
    | R.DefaultParam (i, e) -> L.Default (convert_ident i, convert_expr e)
    | R.ParamDots           -> L.VarParam

(* Helper function for convert_expr that creates a <- function from a normal name.
    so assign_special_body "length" will define the "length<-" function. *)
and assign_special_body: string -> 'a R.arg list -> 'a R.expr -> 'a R.expr option -> ('a, 'b) L.expr =
    fun s args e1 oe2 ->
    let c_args = map convert_arg args in
    let c_i = L.Ident {L.pkg=None; L.name=Some (s^"<-"); L.tag=None} in
    let c_e1 = convert_arg (R.ExprArg e1) in
    let c_args2 = match oe2 with
        | Some e2   -> let c_e2 = convert_arg (R.ExprArg e2) in
                        [c_e1] @ c_args @ [c_e2]
        | None      -> [c_e1] @ c_args in
    L.LambdaApp(c_i, c_args2)
