
open Syntax
open Support
open Engine
open Langutils
open Interputils
open OUnit

open List
open Sys

type expr = Support.expr


let rint_vec_of_ints : int list -> rvector =
  fun ints ->
    IntVec (Array.of_list (map rint_of_int ints))

let rfloat_vec_of_floats : float list -> rvector =
  fun floats ->
    FloatVec (Array.of_list (map rfloat_of_float floats))

let rcomplex_vec_of_complexes : Complex.t list -> rvector =
  fun complexes ->
    ComplexVec (Array.of_list (map rcomplex_of_complex complexes))

let rstr_vec_of_strs : string list -> rvector =
  fun strs ->
    StrVec (Array.of_list (map rstring_of_string strs))

let rbool_vec_of_bools : int list -> rvector =
  fun bools ->
    BoolVec (Array.of_list (map rbool_of_bool bools))

(* Ignores the attributes *)
let result_equals_rvector : (value * attributes) option -> rvector -> bool =
  fun res vec ->
    match res with
    | None -> false
    | Some (value, _) ->
      match value with
      | Vec vals -> vals = vec
      | _ -> false

let test_file : string -> int -> rvector -> unit -> unit =
  fun file n vec _ ->
    assert_bool ("running " ^ file ^ " with n=" ^ string_of_int n ^
                 " does not yield " ^ string_of_rvector vec)
                (result_equals_rvector (load_run_n_first_result file n) vec)

let test_test = "Proto" >:::
  [
    "foo1" >:: (fun _ -> assert_equal "x" "x");
    "foo2" >:: (fun _ -> assert_equal 1 2);
  ]

let test_simple_dir : unit -> string =
  fun _ -> getcwd () ^ "/simple"

let test_simple = "Simple" >:::
  [
    "assign.R" >::
      test_file (test_simple_dir () ^ "/assigns.R") 100
                (rint_vec_of_ints [4]);
    "dots.R" >::
      test_file (test_simple_dir () ^ "/dots.R") 100
                (rint_vec_of_ints [8; 9; 10]);
    "gt.R" >::
      test_file (test_simple_dir () ^ "/gt.R") 100
                (rbool_vec_of_bools [0]);
    "arithmetics.R" >::
      test_file (test_simple_dir () ^ "/arithmetics.R") 100
                (rint_vec_of_ints [600; 906; 604; 912]);
    "vecs.R" >::
      test_file (test_simple_dir () ^ "/vecs.R") 100
                (rint_vec_of_ints [9]);
    "branching.R" >::
      test_file (test_simple_dir () ^ "/branching.R") 100
                (rint_vec_of_ints [201]);
    "loops.R" >::
      test_file (test_simple_dir () ^ "/loops.R") 500
                (rint_vec_of_ints [15]);
    "var-dots.R" >::
      test_file (test_simple_dir () ^ "/var-dots.R") 100
                (rint_vec_of_ints [123]);
  ]

let test_base_dir : unit -> string =
  fun _ -> getcwd () ^ "/base-test"

let test_base = "base" >:::
  [
    "appending.R" >::
      test_file (test_base_dir () ^ "/appending.R") 150
                (rint_vec_of_ints [1; 2; 3; 3; 4; 5]);
  ]


let _ =
  let _ = run_test_tt test_simple in
  let _ = run_test_tt test_base in
    ()


