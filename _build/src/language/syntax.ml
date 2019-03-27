(*
  syntax.ml

  Internal representation of the R program during execution.
  interpreter/preprocess/rast_to_language.ml is used to convert from the parsed AST in
  parser/rast.ml to this representation. This representation is simpler, so (ex.) for-loops
  are syntactic sugar for while-loops.
*)


type rint = int option
type rfloat = float option
type rcomplex = Complex.t option
type rbool = int option
type rstring = string option

type 'a ident = 
  { pkg : rstring option;
    name : rstring;
    tag : 'a option }

type memref =
  { addr : int }

type 'b tick =
  { annot : 'b }

type numeric =
    Int of rint
  | Float of rfloat
  | Complex of rcomplex

type const =
    Num of numeric
  | Str of rstring
  | Bool of rbool (* bools are stored as 0,1 *)
  | Nil (* for a[,1] *)

type ('a, 'b) param =
    Param of 'a ident
  | Default of 'a ident * ('a, 'b) expr
  | VarParam

and ('a, 'b) arg =
    Arg of ('a, 'b) expr
  | Named of 'a ident * ('a, 'b) expr
  | VarArg

and ('a, 'b) expr =
    Ident of 'a ident
  | MemRef of memref
  | Const of const
  | Seq of (('a, 'b) expr) list
  | LambdaAbs of (('a, 'b) param) list * ('a, 'b) expr
  | LambdaApp of ('a, 'b) expr * (('a, 'b) arg) list
  | NativeLambdaApp of 'a ident * ('a ident) list
  | Assign of ('a, 'b) expr * ('a, 'b) expr
  | SuperAssign of ('a, 'b) expr * ('a, 'b) expr
  | If of ('a, 'b) expr * ('a, 'b) expr * ('a, 'b) expr
  | While of ('a, 'b) expr * ('a, 'b) expr
  | Break
  | Next
  | Return of ('a, 'b) expr
  | Error
  | Tick of 'b tick * ('a, 'b) expr


