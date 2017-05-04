(*
 * This code originated as part of the McGill COMP520 course requirements.
 * Any student examining the code or using any part of the code for their coursework must clearly
 * document what code they examined and give clear credit to any code that they used.
 *
 * Original authors:
 * Rohan Jacob-Rao (rohanjr)
 * Steven Thephsourinthone (stheph)
 * Shawn Otis 
 *)

(*exception UsageError of string*)
open Printf
open Lexing
open Pretty
module E = Error

let pptype = ref false
let pretty_print = ref true
let compile = ref true
let help = ref false
let version = ref false

let string_of_pos pos = sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let rec process_arg arg args = 
  match arg with
  | "-dumpsymtab" -> Check.SymTbl.dump := true; process_args args
  | "-pptype" -> pptype := true; process_args args
  | "-nopretty" -> pretty_print := false; process_args args
  | "-nocompile" -> compile := false; process_args args
  | "--help"
  | "-h" -> help := true; process_args args
  | "--version"
  | "-v" -> version := true; process_args args
  | _ -> process_args args

and process_args args =
  match args with
  | [] -> None
  | arg::args' -> 
      if (String.get arg 0) = '-'
      then process_arg arg args'
      else Some arg

let () = try
  (*if Array.length Sys.argv < 2 then ()
    raise (UsageError ("Missing command line argument. " ^
                       "Give base name for output files."))     
  else
    let basename = Sys.argv.(1) in*)
      let filenameOpt = process_args (List.tl (Array.to_list Sys.argv)) in      
      if !help then fprintf stdout 
      "Name: Group15 GoLite -> C Compiler\nSynopsis: ./main.native [option]... [file]\nDescription: Compile GoLite code into C11.\n-dumpsymtab : Dump the symbol table while compiling.\n-pptype : Pretty print with types.\n-nopretty : Suppress the creation of a pretty printed file.\n-nocompile : Suppress the compilation (for debug).\n--help, -h : Help.\n--version, -v : Compiler version.\n" 
      else if !version then fprintf stdout "Group15 GoLite v1 -> C11 Compiler v1" else       
      let filename = 
        begin
          match filenameOpt with
          | None -> raise (E.Error "Missing filename")
          | Some name -> name
        end
      in
      let _ = Check.SymTbl.filename := filename in
      let _ = if !Check.SymTbl.dump then Check.SymTbl.clear_file () else () in 
      let lexbuf = Lexing.from_channel (open_in(filename)) in
      let _ = lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename} in
      let p = Parser.prog Lexer.token lexbuf in 
      let weeded_one = Weeder.ForSwitchWeeding.weed p in
      let _ = if !pretty_print then
        let oc = open_out (String.sub filename 0 (String.rindex filename '.') ^ ".pretty.go") in  fprintf oc "%s" (Pretty.pretty weeded_one)
      in
      let _ = Check.check_prog weeded_one in      
      let weeded_two = Weeder.ReturnWeeding.weed weeded_one; weeded_one in
      (* Pretty Printing the file after type checking, given flag *)
      (if !pptype then let oc' = open_out (String.sub filename 0 (String.rindex filename '.') ^ ".pptype.go") in fprintf oc' "%s" (Pretty.pretty weeded_two));
      (if !compile then let oc' = open_out (String.sub filename 0 (String.rindex filename '.') ^ ".c") in fprintf oc' "%s" (Codegen.gen_prog weeded_two));
      flush stdout;
      (*Typecheck.type_of_prog p;
      output_string (open_out (basename ^ ".symbol.txt"))
                    (Typecheck.string_of_symtable Typecheck.symtable);
      output_string (open_out (basename ^ ".pretty.min")) (Pretty.pretty_prog p);
      output_string (open_out (basename ^ ".c")) (Codegen.code_of_prog p)*)
with
  (*| UsageError msg -> print_string ("Usage error. " ^ msg ^ "\n")*)
  | Lexer.LexerError msg -> fprintf stderr "Lexer error: %s\n" msg
  | Parser.Error -> fprintf stderr "Syntax error.\n"
  | E.ParsingError (s, p) -> fprintf stderr "%s\n" (string_of_pos p ^ ": error: " ^ s)
  | E.Error s -> fprintf stderr "Error: %s\n" s
  | Weeder.WeedError s -> fprintf stderr "error: %s\n" s
  | Check.TypeError s -> fprintf stderr "Type error: %s\n" s
  | Check.DeclError s -> fprintf stderr "Decl error: %s\n" s
  | Check.InternalError s -> fprintf stderr "Internal error: %s\n" s
  (*| Typecheck.TypeError msg -> print_string ("Type error. " ^ msg ^ "\n")
  | Typecheck.DeclError msg -> print_string ("Declaration error. " ^ msg ^ "\n")
  | Codegen.InternalError msg -> print_string ("Internal error. " ^ msg ^ "\n")*)
