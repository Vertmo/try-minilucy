open Js_of_ocaml
open Common
open Lexing

let editor = Ace.edit "editor"
let compile_results = Ace.edit "compil-results"

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%d:%d"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(** Show an error with [text] at [loc] in the console as well as the editor *)
let show_error ?(in_console=false) text loc =
  if in_console then
    Page.Console.error (Printf.sprintf "%s at %s" text (string_of_loc loc));

  let (spos, epos) = loc in
  let s_row = spos.pos_lnum - 1 and e_row = epos.pos_lnum - 1
  and s_col = spos.pos_cnum - spos.pos_bol and e_col = epos.pos_cnum - epos.pos_bol in

  let ann : Ace.annotation Js.t = object%js
    val row = s_row
    val column = s_col;
    val text = Js.string text;
    val _type = Js.string "error";
  end in
  editor##.session##setAnnotations(Js.array [|ann|]);

  let range = new%js Ace.range s_row s_col e_row e_col in
  ignore (editor##.session##addMarker range (Js.string "error-marker") (Js.string "text") true);
  raise Done

let parse_with_error ?(in_console=false) lexbuf =
  try Parser.file Lexer.token lexbuf with
  | Parser.Error ->
    show_error ~in_console "Syntax error" (lexbuf.lex_start_p, lexbuf.lex_curr_p)

(** Execute a function while handling a possible compilation error *)
let handle_compile_error ?(in_console=false) f a =
  try f a with
  | Typechecker.UnexpectedEquationError (id, loc) ->
    show_error ~in_console (Printf.sprintf "Unexpected equation for %s" id) loc
  | Typechecker.TypeError (msg, loc) ->
    show_error ~in_console (Printf.sprintf "Type checking error : %s" msg) loc
  | Clockchecker.ClockError (msg, loc) ->
    show_error ~in_console (Printf.sprintf "Clock checking error : %s" msg) loc
  | Causalitychecker.CausalityError (msg, nodeid, loc) ->
    show_error ~in_console (Printf.sprintf "Causality error : %s in node %s" msg nodeid) loc
  | e -> raise e

(** Format and print the program [p] using the [printer] function *)
let print_result printer p =
  printer Format.str_formatter p;
  compile_results##.session##setValue (Js.string (Format.flush_str_formatter ()))

let lex_and_parse_program ?(in_console=false) () =
  let s = Js.to_string (editor##getValue ()) in
  let lexbuf = Lexing.from_string s in
  parse_with_error ~in_console lexbuf

(** Parse, type-check and clock-check a program *)
let parse_and_check_program ?(in_console=false) () =
  (* First, refresh annotations and markers *)
  editor##.session##clearAnnotations ();
  Ace.clear_markers editor##.session;

  lex_and_parse_program ~in_console () |>
  Typechecker.type_file |>
  Clockchecker.elab_file

(** Compile the program for visualisation purposes *)
let compile_program step =
  (* reset the global counter *)
  Atom.counter := 0;

  let cfile = parse_and_check_program ~in_console:true () in

  if (step = Check)
  then (
    print_result Clockchecker.CPMinils.print_file cfile;
    raise Done
  );

  let file =
    try Kernelizer.kernelize_file ~formatter:Format.str_formatter step cfile
    with Done ->
      (compile_results##.session##setValue (Js.string (Format.flush_str_formatter ()));
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

  if (step = Translate) then (
    print_result Obc.print_file mfile;
    raise Done)

let compile_and_exn step (_ : #Dom_html.event Js.t) =
  (try handle_compile_error ~in_console:true compile_program step
   with Done -> () | e -> raise e);
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
  (Page.by_id "translate")##.onclick := Dom_html.handler (compile_and_exn Translate);
  Js._true

let _ =
  editor##.session##setMode (Js.string "ace/mode/lustre");
  editor##setOption (Js.string "tabSize") (Js.string "2");
  editor##on (Js.string "change") (fun () ->
      Page.save_program (editor##getValue ());
      try ignore (handle_compile_error parse_and_check_program ())
      with Done -> () | e -> raise e);
  editor##.session##setValue (Page.get_saved_program ());

  compile_results##.session##setMode (Js.string "ace/mode/lustre");
  compile_results##setReadOnly true;
  Dom_html.window##.onload := Dom_html.handler init
