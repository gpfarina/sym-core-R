(*
    engine.ml

    Actually runs the reduction rules on the state to evaluate a program.
*)
open Syntax
open Smtsyntax
open Smtutils
open Support
open Rules
open Native_calls
open Loader
open Smttrans
open Smt2
open Solver

open Stepper

open List
open Printf
open Sys

type redresult =
  | ReductionOkay of rule * state list
  | MultipleRulesMatch of (rule * state list) list
  | NoRulesMatch

(* Tries to apply every rule in the rule table to the passed state.
    The rules that yield at least one ouput state "match" the passed state, and
    ideally only one reduction rule matches at a time. *)
let step_rule : state -> redresult =
  fun state ->
    let res = map (fun (r, f) -> (r, f state)) rule_table in
      match res with
      | [] -> NoRulesMatch
      | _ -> match filter (fun (r, ss) -> length ss > 0) res with
        | [] -> NoRulesMatch
        | [(r, ss)] -> ReductionOkay (r, ss)
        | toomuch -> MultipleRulesMatch toomuch

(* Dereferences the ReturnSlot on top of the stack if it is the only slot on the stack. *)
let get_state_result : state -> (value * attributes) option =
  fun state ->
    (* If popping one value is a ReturnSlot, but we can't pop two values, then
        the ReturnSlot is the only thing on the stack. *)
    match (stack_pop_v state.stack, stack_pop_v2 state.stack) with
    | (Some (ReturnSlot mem, _, _), None) ->
      (match heap_find mem state.heap with
      | Some (DataObj (value, attrs)) -> Some (value, attrs)
      | _ -> None)
    | _ -> None

(* If the state's stack is only a ReturnSlot *)
let is_state_complete : state -> bool =
  fun state ->
    match get_state_result state with
    | Some _ -> true
    | None -> false

let is_state_not_complete : state -> bool =
  fun state -> not (is_state_complete state)

(* Runs one pass of the stepper on each (history, state) pair, and produces a passresutlt - defined in
    interp-commons/rules.ml *)
let run_pass : (rule list * state) list -> passresult =
  fun states ->
    let comps = filter (fun (a, b) -> is_state_complete b) states in
    let incomps = filter (fun (a, b) -> is_state_not_complete b) states in
    let (comps2, errs2, incomps2) =
      fold_left
        (fun (c, e, i) (hist, st) ->
          match step_rule st with
          | NoRulesMatch ->
              print_endline ("no rules matched at state " ^
                              string_of_int st.unique);
              (c, (hist, st) :: e, i)
          | ReductionOkay (r, sts) ->
              (* Append the matching rule to the states' history after filtering
                out only the complete states. *)
              let csts = map (fun s -> (r :: hist, s))
                             (filter is_state_complete sts) in
              let ncsts = map (fun s -> (r :: hist, s))
                              (filter is_state_not_complete sts) in
                (csts @ c, e, ncsts @ i)
          | MultipleRulesMatch ress ->
              print_endline ("multiple rules match at state " ^
                             string_of_int st.unique);
              print_endline ("pretending as though nothing happened!");
              (* Apply the same logic as above for each of the matched states. *)
              let expanded =
                    concat (map (fun (r, ss) ->
                                  map (fun s -> (r :: hist, s)) ss)
                                ress) in
              let csts = filter (fun (r, s) -> is_state_complete s)
                                 expanded in
              let ncsts = filter (fun (r, s) -> is_state_not_complete s)
                                  expanded in
                (csts @ c, e, ncsts @ i))
        (* This is a fold-left on the incomplete states, applying a single step of the
            production rules to each, and producing the new fringe of the execution tree by
            concatenating the children of each incomplete state. *)
        (comps, [], []) incomps in
      { (fresh_passresult ()) with
          pass_comps = comps2;
          pass_errs = errs2;
          pass_incomps = incomps2 }

(* Different versions of run functions for debugging, etc *)

(* Apply n production rules to the input state list. *)
let run_n : int -> state list -> passresult =
  fun n inits ->
    let raws = map (fun s -> ([], s)) inits in
    let ticks = ref n in
    let comps = ref [] in 
    let errs = ref [] in
    let incomps = ref raws in
      begin
        while (!ticks > 0) do
          if List.length !incomps <= 0 then
            begin ticks := -1; end
          else
            begin
              let pr2 = run_pass !incomps in
              comps := pr2.pass_comps @ !comps;
              errs := pr2.pass_errs @ !errs;
              incomps := pr2.pass_incomps;
              ticks := !ticks - 1
            end
        done;
        { (fresh_passresult ()) with
            pass_comps = !comps;
            pass_errs = !errs;
            pass_incomps = !incomps }
      end

let run_n_hist : int -> state list -> passresult list =
  fun n inits ->
    let raws = map (fun s -> ([], s)) inits in
    let ticks = ref n in
    let hist = ref [{ (fresh_passresult ()) with pass_incomps = raws }] in
    let incomps = ref raws in
      begin
        while (!ticks > 0) do
          if List.length !incomps <= 0 then
            begin ticks := -1; end
          else
            begin
              let pr2 = run_pass !incomps in
              hist := !hist @ [pr2];
              incomps := pr2.pass_incomps;
              ticks := !ticks - 1;
            end
        done;
        !hist
      end

let run_n_first_result :
  int -> state list ->  (value * attributes) option =
  fun n inits ->
    let pr = run_n n inits in
    let ress = map (fun (_, s) -> get_state_result s) pr.pass_comps in
      match ress with
      | (res :: ress_tail) ->
          begin
            let len = List.length ress_tail in
            if (len > 0) then
              print_endline ("omitting " ^ string_of_int len ^ " results");
            res
          end
      | _ -> None

let load_run_n_first_result : string -> int -> (value * attributes) option =
  fun file n ->
    let state = load_file_guess file in
      run_n_first_result n [state]

(* Creates a SMT-LIBv2 representation of the path constraints on the current state and
    runs the Z3 prover on it. *)
let solve_state : state -> smtprog =
  fun state ->
    let stmts = smtcmd_list_of_state state in
    let smt2 = smt2_of_smtcmd_list stmts in
    (*
    let _ = print_endline "querying z3 with:" in
    let _ = print_endline smt2 in
    *)
    let res = run_z3 smt2 in
    (*
    let _ = print_endline "z3 result:" in
    let _ = print_endline (string_of_smtprog res) in
    *)
      res

let solve_stupid_state : state -> string =
  fun state ->
    let stmts = smtcmd_list_of_state state in
    let smt2 = smt2_of_smtcmd_list stmts in
    let res = run_stupid_z3 smt2 in
      res

let rw_perms : unit -> int =
  fun _ -> 0o666

(*
(* Assumes a canonicalized directory *)
let dump_solve_state : string -> state -> unit =
  fun dir state ->
    let smtprog = solve_state state in
    let res = solve_state state in
    (* let _ = if file_exists dir then mkdir dir (rw_perms ()) in *)
    let _ = if file_exists dir then command ("mkdir " ^ dir) else -1 in
    let dump_file = dir ^ "/state-" ^ string_of_int state.unique in
    let dump_out = open_out dump_file in
    let _ = fprintf dump_out "%s" res in
    let _ = close_out dump_out in
      ()
*)

let solve_comps_passresult : passresult -> (rule list * state * smtprog) list =
  fun pass ->
    map (fun (rs, s) -> (rs, s, solve_state s)) pass.pass_comps

let solve_stupid_comps_passresult :
  passresult -> (rule list * state * string) list =
  fun pass ->
    map (fun (rs, s) -> (rs, s, solve_stupid_state s)) pass.pass_comps



(*
(* Assumes a canonicalized directory *)
let dump_solve_passresult : string -> passresult -> unit =
  fun dir (comps, errs, incomps) ->
    (* let _ = if not (file_exists dir) then
                  mkdir dir (rw_perms ()) in *)
    let _ = if not (file_exists dir) then command ("mkdir " ^ dir) else -1 in

    let comps_dir = dir ^ "/comps" in
    (* let _ = if not (file_exists comps_dir) then
                  mkdir comps_dir (rw_perms ()) in *)
    let _ = if not (file_exists comps_dir) then
              command ("mkdir " ^ comps_dir) else -1 in
    let _ = map (fun (_, s) -> dump_solve_state comps_dir s) comps in

    let errs_dir = dir ^ "/errs" in
    (* let _ = if not (file_exists errs_dir) then
                  mkdir errs_dir (rw_perms ()) in *)
    let _ = if not (file_exists errs_dir) then
              command ("mkdir " ^ errs_dir) else -1 in
    let _ = map (fun (_, s) -> dump_solve_state errs_dir s) errs in

    let incomps_dir = dir ^ "/incomps" in
    (* let _ = if not (file_exists incomps_dir) then
                  mkdir incomps_dir (rw_perms ()) in *)
    let _ = if not (file_exists incomps_dir) then
              command ("mkdir " ^ incomps_dir) else -1 in
    let _ = map (fun (_, s) -> dump_solve_state incomps_dir s) incomps in
      ()

*)

let inj_symbolics : unit list -> state -> state =
  fun _ s -> s

