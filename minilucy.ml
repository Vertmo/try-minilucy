open Js_of_ocaml
open Common
open Lexing

let editor = Ace.edit "editor"
let compile_results = Ace.edit "compil-results"

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%d:%d"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | Parser.Error ->
    Page.Console.error (Printf.sprintf "Syntax error in program at %s\n" (string_of_position lexbuf));
    raise Done

(** Format and print the program [p] using the [printer] function *)
let print_result printer p =
  Buffer.reset Format.stdbuf;
  printer Format.str_formatter p;
  compile_results##.session##setValue (Js.string (Buffer.contents Format.stdbuf))

let lex_and_parse_program () =
  let s = Js.to_string (editor##getValue ()) in
  let lexbuf = Lexing.from_string s in
  parse_with_error lexbuf

let compile_program step =
  (* reset the global counter *)
  Atom.counter := 0;

  let pfile = lex_and_parse_program () in
  let tfile = Typechecker.type_file pfile in
  let cfile = Clockchecker.elab_file tfile in

  if (step = Check)
  then (
    print_result Clockchecker.CPMinils.print_file cfile;
    raise Done
  );

  Buffer.reset Format.stdbuf;
  let file =
    try Kernelizer.kernelize_file ~formatter:Format.str_formatter step cfile
    with Done ->
      (compile_results##.session##setValue (Js.string (Buffer.contents Format.stdbuf));
       raise Done) in

  let nfile = Normalizer.norm_file file in

  if (step = Norm) then (
    print_result NMinils.print_file nfile;
    raise Done);

  Causalitychecker.check_file nfile;
  let nfile = Scheduler.schedule_file nfile in

  if (step = Sched) then (
    print_result NMinils.print_file nfile;
    raise Done);

  (* Translate *)
  let mfile = Translator.translate_file nfile in

  ()

let compile_and_exn step (_ : #Dom_html.event Js.t) =
  (try compile_program step with
   | Done -> ()
   | Typechecker.UnexpectedEquationError (id, loc) ->
     Page.Console.error
       (Printf.sprintf "Type checking error : UnexpectedEquation for %s at %s\n"
          id (string_of_loc loc))
   | Typechecker.TypeError (msg, loc) ->
     Page.Console.error
       (Printf.sprintf "Type checking error : %s at %s\n"
          msg (string_of_loc loc))
   | Clockchecker.ClockError (msg, loc) ->
     Page.Console.log
       (Printf.sprintf "Clock checking error : %s at %s\n"
          msg (string_of_loc loc))
   | Causalitychecker.CausalityError (msg, nodeid, loc) ->
     Page.Console.log
       (Printf.sprintf "Causality error : %s in node %s at %s\n"
          msg nodeid (string_of_loc loc))
  );
  Js._true

let init (_ : #Dom_html.event Js.t) =
  (Page.by_id "check")##.onclick := Dom_html.handler (compile_and_exn Check);
  (Page.by_id "last")##.onclick := Dom_html.handler (compile_and_exn Last);
  (Page.by_id "automaton")##.onclick := Dom_html.handler (compile_and_exn Automaton);
  (Page.by_id "reset")##.onclick := Dom_html.handler (compile_and_exn Reset);
  (Page.by_id "switch")##.onclick := Dom_html.handler (compile_and_exn Switch);
  (Page.by_id "block")##.onclick := Dom_html.handler (compile_and_exn Block);
  (Page.by_id "norm")##.onclick := Dom_html.handler (compile_and_exn Norm);
  (Page.by_id "sched")##.onclick := Dom_html.handler (compile_and_exn Sched);
  Js._true

let _ =
  editor##.session##setMode (Js.string "ace/mode/lustre");
  editor##setOption (Js.string "tabSize") (Js.string "2");
  compile_results##.session##setMode (Js.string "ace/mode/lustre");
  compile_results##setReadOnly (Js.bool true);
  Dom_html.window##.onload := Dom_html.handler init
