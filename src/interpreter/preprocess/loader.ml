
(*
open Language
*)
module R = Rast
open Syntax
open Support
open Langutils
open Rast_to_language
open Absyn_generator
open Natives

module F = Filename
open List
open Sys

let base_dir : unit -> string =
  fun _ -> getcwd () ^ "/base/R"

let base_file : unit -> string =
  fun _ -> "custom.R"

type rastexpr = unit R.expr

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type ('a, 'b) either =
  | OptA of 'a
  | OptB of 'b

type exectree =
  | ExprLeaf of string * expr
  | ExprNode of string * exectree list

let unwrap_raw_string_const : string -> string =
  fun raw ->
    let len = String.length raw in
    if len > 0 && raw.[0] = '"' && raw.[len - 1] = '"' then
      String.sub raw 1 (len - 2)
    else
      raw

let canon_of_R_file : string -> string -> string =
  fun dir file ->
    if F.is_relative file then
      dir ^ "/" ^ file
    else
      file

let exprs_of_file : string -> expr list =
  fun file ->
    map convert_expr (parseFile file)

let cat_of_expr : expr -> (string, expr) either =
  fun expr ->
    match expr with
    | LambdaApp (Ident ({ name = Some "source" }),
                  [Arg (Const (Str (Some src)))]) ->
                    OptA (unwrap_raw_string_const src)
    | _ -> OptB expr

let cats_of_file : string -> string * ((string, expr) either) list =
  fun file ->
    (file, map cat_of_expr (exprs_of_file file))

let rec exectree_of_file : string -> string -> exectree =
  fun dir file ->
    let canon_file = canon_of_R_file dir file in
    let (f, opts) = cats_of_file canon_file in
    let nodelist =
        List.map (fun c -> match c with
                   | OptA child_file -> exectree_of_file dir child_file
                   | OptB expr -> ExprLeaf (f, expr)) opts in
      ExprNode (f, nodelist)

let rec linearization_of_exectree :
  exectree -> string list * (string * expr) list =
  fun tree ->
    match tree with
      | ExprLeaf (file, expr) -> ([], [(file, expr)])
      | ExprNode (file, currs) ->
          let level = map linearization_of_exectree currs in
          let (files, exprs) =
              List.fold_left
                (fun (acc_f, acc_e) (f, e) -> (acc_f @ f, acc_e @ e))
                ([], []) level in
            (files @ [file], exprs)

let linearize_source :
  string -> string -> string list * (string * expr) list =
  fun dir file ->
    linearization_of_exectree (exectree_of_file dir file)

let linearize_source_with_base :
  string -> string -> string list * (string * expr) list =
  fun dir file ->
    let canon_file = canon_of_R_file dir file in
    let base_tree = exectree_of_file (base_dir ()) (base_file ()) in
    let src_tree = exectree_of_file dir file in
    let joint_tree = (match src_tree with
                      | ExprLeaf (f, e) ->
                          ExprNode (f, [base_tree; ExprLeaf (f, e)])
                      | ExprNode (f, ets) ->
                          ExprNode (f, base_tree :: ets)) in
      linearization_of_exectree joint_tree

let dump_file_ast : string -> unit =
  fun file ->
    let absyn = parseFile file in
    print_endline (Rast.string_of_program absyn)

let dump_file_linearization : string -> string -> unit =
  fun dir file ->
    (* let (files, exprs) = linearize_source dir file in *)
    let (files, exprs) = linearize_source_with_base dir file in
      print_endline "--- files:";
      iter (fun f -> print_string f; print_newline ()) files;
      print_endline "--- exprs:";
      iter (fun (f, e) -> print_string ("[" ^ f ^ "]\n");
                         print_endline (string_of_expr e);
                         print_newline ()) exprs;
      print_endline "--- end dump";;

let inj_binds_to_env_heap : (ident * expr) list -> memref -> heap -> heap =
  fun binds env_mem heap ->
    match heap_find env_mem heap with
    | Some (DataObj (EnvVal env, attrs)) ->
        let ids = map (fun (i, _) -> i) binds in
        let proms = map (fun (_, e) -> PromiseObj (e, env_mem)) binds in
        let (mems, heap2) = heap_alloc_list proms heap in
        let env_binds = combine ids mems in
        let env2 = env_add_list env_binds env in
          heap_add env_mem (DataObj (EnvVal env2, attrs)) heap2
    | _ -> failwith ("inj_binds_to_heap_global: did not find env at " ^
                     string_of_mem env_mem)

let rec inj_heap_list : (memref * heapobj) list -> heap -> heap =
  fun binds heap ->
    match binds with
    | [] -> heap
    | ((mem, hobj) :: binds_tl) ->
        match heap_find mem heap with
        | None -> inj_heap_list binds_tl (heap_add mem hobj heap)
        | Some _ -> failwith ("inj_heap_list: binding exists at: " ^
                               string_of_mem mem)

(* The ones closer to the tail are closer to the super duper env *)
let alloc_file_envs :
  string list -> memref -> heap -> (string * memref) list * heap =
  fun files sup_env_mem heap ->
    if length files = 0 then
      ([], heap)
    else
      let envs = map (fun f -> DataObj (EnvVal (env_empty ()),
                                          attrs_empty ())) files in
      let (mems, heap2) = heap_alloc_list envs heap in
      let pred_mems = sup_env_mem :: mems in
      let pairs = combine mems (rev (tl (rev pred_mems))) in
      let heap3 = fold_left (fun h (e, s) -> match heap_find e h with
                            | Some (DataObj (EnvVal env, attrs)) ->
                                let env2 = { env with pred_mem = s } in
                                  heap_add e (DataObj (EnvVal env2, attrs)) h
                            | _ -> failwith ("alloc_file_envs: " ^
                                                string_of_mem e)
                          ) heap2 pairs in
        (* (combine files (rev mems), heap3) *)
        (combine files mems, heap3)

let stringmap_of_list : (string * 'a) list -> 'a StringMap.t =
  fun lst ->
    fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty lst

let frames_of_binds :
  (string * expr) list -> (string * memref) list -> frame list =
  fun exprs envs ->
    let env_map = stringmap_of_list envs in
      try
        map (fun (f, e) ->
                { frame_default with
                    slot = EvalSlot e;
                    env_mem = StringMap.find f env_map }) exprs
      with
        Not_found ->
          failwith ("stringmap_of_list: failed to initialize env map properly")

let raw_inits_of_file : string -> string -> stack * heap * memref * memref =
  fun dir file ->
    let heap1 = heap_empty_offset 1 in
    let null_env_mem = mem_null () in
    (* let (files, file_expr_binds) = linearize_source dir file in *)
    let (files, file_expr_binds) = linearize_source_with_base dir file in
    (* Append on the native stuff for the target allocation *)
    let files2 = "$native_file" :: files in
    let file_expr_binds2 = ("$native_file", Const Nil) :: file_expr_binds in
    (* let _ = dump_file_linearization dir file in *)
    let (file_env_binds, heap2) = alloc_file_envs files2 null_env_mem heap1 in
    let frames = frames_of_binds file_expr_binds2 file_env_binds in
      (* Need to get the last thing to check the global env *)
      match (file_env_binds, rev file_env_binds) with
      | ((_, nat_mem) :: _, (_, glbl_mem) :: _) ->
          (stack_push_list frames (stack_empty ()), heap2, glbl_mem, nat_mem)
      | _ -> failwith "raw_inits_of_file: failed to initialize envs correctly"

let make_native_binds : memref -> heap -> (ident * heapobj) list * heap =
  fun glbl_env_mem heap ->
    fold_left
      (fun (accs, hp) (id, (params, body)) ->
        let f_env = { (env_empty ()) with pred_mem = glbl_env_mem } in
        let f_env_obj = DataObj (EnvVal f_env, attrs_empty ()) in
        let (f_env_mem, hp2) = heap_alloc f_env_obj hp in
        let func = FuncVal (params, body, f_env_mem) in
        let f_obj = DataObj (func, attrs_empty ()) in
          (accs @ [(id, f_obj)], hp2))
      ([], heap) native_injection_pairs

let raw_init_state : string -> string -> state =
  fun dir file ->
    let (stack, heap, glbl_env_mem, nat_env_mem) = raw_inits_of_file dir file in
    let (native_binds, heap2) = make_native_binds nat_env_mem heap in
      match env_mem_bind_list native_binds nat_env_mem heap2 with
      | Some heap3 ->
          { state_default with
              stack = stack;
              heap = heap3;
              global_env_mem = glbl_env_mem }
      | None ->
          failwith "raw_init_state: could not inject native binds"

let guess_entry_info : string -> string * string =
  fun path -> (F.dirname path, F.basename path)

let load_file : string -> string -> state =
  fun dir file ->
    raw_init_state dir file

let load_file_guess : string -> state =
  fun path ->
    let (dir, file) = guess_entry_info path in
      load_file dir file



