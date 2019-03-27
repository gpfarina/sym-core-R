
let parseFile filename =
  print_string ("parseFile: opening " ^ filename ^ " ... ");
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let absyn =
   try
     Parser.prog (Lexer.tokenize (ref [])) lexbuf
   with _ ->
      let pos = lexbuf.Lexing.lex_curr_p in
      begin
        print_endline "Error!";
        print_string "Syntax error detected at line ";
        print_string (string_of_int pos.Lexing.pos_lnum);
        print_string " column ";
        print_string (string_of_int (pos.Lexing.pos_cnum -
                                     pos.Lexing.pos_bol));
        print_endline ".";
        failwith "Syntax error"
      end in
      let _ = close_in channel in
      print_endline ("Okay!");
      absyn;;

let dumpTokens : string -> unit =
  fun filename ->
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    let x = ref [] in 
      while true do
        let tok = Lexer.tokenize x lexbuf in
          print_endline (Lexer.string_of_token tok);
          flush stdout
      done
    
(*
let main () =
  let args = Array.to_list Sys.argv in
  let in_filename = match args with
                  | [] -> failwith "exactly one filename expected"
                  | (_ :: arg :: _) -> arg
                  | _ -> failwith "exactly one filename expected" in
  (* Parsing *)
  (* let _ = dumpTokens in_filename in *)
  let absyn = parseFile in_filename in
    print_endline (Rast.string_of_program absyn);
    ;;
main ()
*)

