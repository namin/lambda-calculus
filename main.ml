(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

let searchpath = ref [""]
let interactive = ref false
let typing = ref false                

let argDefs = [
  ("-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path");
  ("-i",
      Arg.Set interactive,
      "Continue with an interactive REPL");
  ("-t",
      Arg.Set typing,
      "Typing")
  ]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let order = ref (ApplicativeOrder)

let trace = ref (false)

let findprintty fi ctx t = (
  let (tyT,nextuvar',constr') = recon ctx uvargen t in
  let constr'' =
    unify fi ctx "Could not simplify constraints" constr' in
  print_break 1 2;
  pr ": ";
  open_hovbox 0;
  printty (applysubst constr'' tyT));
  close_box()

let rec process_command ctx cmd = match cmd with
  | Trace(fi, b) -> trace := b; ctx
  | Order(fi, o) -> order := o; ctx
  | Eval(fi,t) -> 
      let t' = eval (!trace) (!order) ctx t in
      printtm_ATerm true ctx t';
      (if (!typing) then (findprintty fi ctx t'));
      force_newline();
      ctx
  | Bind(fi,x,bind) -> 
      let bind' = evalbinding (!trace) (!order) ctx bind in
      addbinding ctx x bind'
  
let process_file f ctx =
  alreadyImported := f :: !alreadyImported;
  let cmds,_ = parseFile f ctx in
  let g ctx c =  
    open_hvbox 0;
    let results = process_command ctx c in
    print_flush();
    results
  in
    List.fold_left g ctx cmds

let repl begctx =
  let lexbuf = Lexing.from_channel stdin in
  let ctx = ref begctx in
  let is_done = ref false in
  while (not (!is_done)) do
    let (rescmd,resctx) =
      (try Parser.toplevel1 Lexer.main lexbuf with Parsing.Parse_error -> 
         error (Lexer.info lexbuf) "Parse error") (!ctx) in
    (match rescmd with
      None -> is_done := true
    | Some c -> ctx := process_command resctx c);
    print_flush()
  done

let main () = 
  let inFile = parseArgs() in
  let ctx = process_file inFile emptycontext in
  if (!interactive) then repl ctx

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
