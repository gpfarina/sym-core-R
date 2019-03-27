(* langutils.ml

  Pretty-printing for the syntax.ml AST.
*)


open Annotations
open Syntax

open String
open List

let rec repeat : string -> int -> string =
  fun str n ->
    if n <= 0 then
      ""
    else if n = 1 then
      str
    else
      str ^ repeat str (n - 1)

let tab2 = repeat " " 2
let tab4 = tab2 ^ tab2
let tab8 = tab4 ^ tab4
let bar20 = repeat "----" 5
let bar40 = let b = bar20 in b ^ b
let bar60 = bar20 ^ bar40
let bar80 = let b = bar40 in b ^ b

let string_of_pair : ('a * 'b) -> (('a -> string) * ('b -> string)) -> string =
  fun (a, b) (fa, fb) ->
    "(" ^ fa a ^ "," ^ fb b ^ ")"

let string_of_list_semicolon : string list -> string =
  fun strs -> String.concat ";" strs

let string_of_list_comma : string list -> string =
  fun strs -> String.concat "," strs

let string_of_list_newline : string list -> string =
  fun strs -> String.concat "\n" strs


let string_of_list : ('a -> string) -> 'a list -> string =
  fun f xs ->
    "[" ^ String.concat "," (List.map f xs) ^ "]"

let string_of_source : source -> string =
  fun src ->
    "source {file:" ^ src.file ^ ";" ^
            "line:" ^ string_of_int src.line ^ ";" ^
            "col:" ^ string_of_int src.col ^ "}"

let string_of_rstring : rstring -> string =
  fun rstr -> match rstr with
     | None -> "NA_character_"
     | Some str -> str

(* Language *)
let string_of_id : 'a ident -> string =
  fun id -> match id.pkg with
    | None -> "ident {pkg:" ^ "_" ^ ";name:" ^ string_of_rstring id.name ^ "}"
    | Some p -> "ident {pkg:" ^ string_of_rstring p ^
                ";name:" ^ string_of_rstring id.name ^ "}"

let string_of_mem : memref -> string =
  fun mem ->
    "addr: " ^ string_of_int mem.addr

let string_of_numeric : numeric -> string =
  fun num -> match num with
    | Int (Some i)      -> "Int (" ^ string_of_int i ^ ")"
    | Int None          -> "Int (Na)"
    | Float (Some f)    -> "Float (" ^ string_of_float f ^ ")"
    | Float None        -> "Float (Na)"
    | Complex (Some c)  -> "Complex (" ^ string_of_float (c.Complex.re) ^ "," ^
                                 string_of_float (c.Complex.im) ^ ")"
    | Complex None      -> "Complex (Na)"
    
let string_of_const : const -> string =
  fun const -> match const with
    | Num n         -> "Num (" ^ string_of_numeric n ^ ")"
    | Str (Some s)  -> "Str (" ^ s ^ ")"
    | Str None      -> "Str (Na)"
    | Bool (Some b) -> "Bool (" ^ string_of_int b ^ ")"
    | Bool None     -> "Bool (Na)"
    | Nil           -> "Nil"

let rec string_of_param : ('a, 'b) param -> string =
  fun param -> match param with
    | VarParam -> "..."
    | Param i -> "Param (" ^ string_of_id i ^ ")"
    | Default (i, e) -> "Default (" ^ string_of_id i ^ "," ^
                                      string_of_expr e ^ ")"

and string_of_arg : ('a, 'b) arg -> string =
  fun arg -> match arg with
    | VarArg -> "..."
    | Arg e -> "Arg (" ^ string_of_expr e ^ ")"
    | Named (i, e) -> "Named (" ^ string_of_id i ^ "," ^
                                  string_of_expr e ^ ")"

and string_of_tick : 'b tick -> string =
  fun tick ->
    "tick {info:" ^ "_" ^ "}"

and string_of_expr : ('a, 'b) expr -> string =
  fun expr -> match expr with
    | Ident i -> "Ident (" ^ string_of_id i ^ ")"
    | MemRef m -> "MemRef (" ^ string_of_mem m ^ ")"
    | Const c -> "Const (" ^ string_of_const c ^ ")"
    | Seq es -> "Seq (" ^ string_of_list (string_of_expr) es ^ ")"
    | LambdaAbs (ps, e) -> "LambdaAbs (" ^
                               string_of_list (string_of_param) ps ^ "," ^
                               string_of_expr e ^ ")"
    | LambdaApp (e1, e2s) -> "LambdaApp (" ^
                                string_of_expr e1 ^ "," ^
                                string_of_list (string_of_arg) e2s ^ ")"
    | NativeLambdaApp (f, is) -> "NativeLambdaApp (" ^
                                    string_of_id f ^ "," ^
                                    string_of_list (string_of_id) is ^ ")"
    | Assign (e1, e2) -> "Assign (" ^ string_of_expr e1 ^ "," ^
                                      string_of_expr e2 ^ ")"
    | SuperAssign (e1, e2) -> "SuperAssign (" ^ string_of_expr e1 ^ "," ^
                                                string_of_expr e2 ^ ")"
    | If (e1, e2, e3) -> "If (" ^ string_of_expr e1 ^ "," ^
                                  string_of_expr e2 ^ "," ^
                                  string_of_expr e3 ^ ")"
    | While (e1, e2) -> "While (" ^ string_of_expr e1 ^ "," ^
                                    string_of_expr e2 ^ ")"
    | Break -> "Break"
    | Next -> "Next"
    | Return e -> "Return (" ^ string_of_expr e ^ ")"
    | Error -> "Error"

    | Tick (t, e) -> "Tick (" ^ string_of_tick t ^
                                string_of_expr e ^ ")"



let string_of_rstring: rstring -> string =
    function
    | Some s -> "\"" ^ s ^ "\""
    | None -> "NaString"

let string_of_rint: rint -> string =
    function
    | Some i -> (string_of_int i)
    | None -> "NaInt"

let string_of_rfloat: rfloat -> string =
    function
    | Some f -> (string_of_float f)
    | None -> "NaFloat"

let string_of_rcomplex: rcomplex -> string =
    function
    | Some c -> "Complex (" ^ (string_of_float c.Complex.re) ^ "," ^
        (string_of_float c.Complex.im) ^ ")"
    | None -> "NaComplex"

let string_of_rbool: rbool -> string =
    function
    | Some b -> if b = 0 then "False" else "True"
    | None -> "NaBool"


