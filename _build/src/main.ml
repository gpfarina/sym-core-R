
open Syntax
open Absyn_generator
open Loader
open Engine
open Rules
open Interputils

open Sys
open List

let default_steps : int = 100

let main () =
  print_endline "SimpleR: complies!";
  (* dump_file_linearization "/home/celery/Desktop/base/R" "test.R"; *)
  (* dump_file_ast "/home/celery/foo/harvard/r-test/tutorial/Rdemo/Notes.R" *)
  (* dumpTokens "/home/celery/Desktop/base/R/aperm.R"; *)
  (* parseFile "/home/celery/Desktop/base/R/aperm.R" *)

  let state = load_file_guess Sys.argv.(1) in
  let n = if Array.length Sys.argv < 3 then
            default_steps
          else
            int_of_string Sys.argv.(2) in

  (*
  print_endline "INITIAL STATE";
  print_endline (string_of_state state);
  *)

  (*
  let ress = run_n_hist n [state] in
  print_endline (string_of_passresult_list ress);
  let _ = solve_comps_passresult (hd (rev ress)) in
  let solves = solve_stupid_comps_passresult (hd (rev ress)) in
  print_endline ("Total of " ^ string_of_int (length solves) ^ " complete states");
  iter (fun (rs, s, z3dump) ->
          begin
            print_endline "******************* START *******************";
            print_endline (string_of_rule_list rs);
            print_endline (string_of_state s);
            print_endline "Z3 RESULT:";
            print_endline z3dump;
            print_endline "******************* _END_ *******************";
            print_endline "\n\n\n";
          end
        ) solves;
  *)


  let res = run_n n [state] in
  let _ = print_endline (string_of_passresult res) in
  let solns = map (fun (_, s) -> solve_stupid_state state) res.pass_comps in
  let _ = List.iter (print_endline) solns in

  (*
  let res = run_n n [state] in
  print_endline (string_of_passresult res);
  *)

  (*
  let res = get_first_completed_after_n n [state] in
  print_endline (string_of_value_attr_opt_pair res);
  *)


  print_endline "SimpleR: done!";

  ;;
  

main ()

