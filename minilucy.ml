open Js_of_ocaml
open Common
open Lexing

let editor_config = object%js
  val lineNumbers = Js.bool true
  val mode = Js.string "lustre"
  val readOnly = Js.bool false
end
let editor = Codemirror.fromTextArea (Dom_html.getElementById "editor") editor_config

let cr_config = object%js
  val lineNumbers = Js.bool true
  val mode = Js.string "lustre"
      val readOnly = Js.bool true
end
let compile_results = Codemirror.fromTextArea (Dom_html.getElementById "compil-result") cr_config

let by_id s = Dom_html.getElementById s

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | Parser.Error ->
    Printf.fprintf stderr "Syntax error in program at %a\n" print_position lexbuf;
    exit (-1)

(** Format and print the program [p] using the [printer] function *)
let print_result printer p =
  Buffer.reset Format.stdbuf;
  printer Format.str_formatter p;
  compile_results##setValue (Js.string (Buffer.contents Format.stdbuf))

let lex_and_parse_program () =
  let s = Js.to_string (editor##getValue ()) in
  let lexbuf = Lexing.from_string s in
  parse_with_error lexbuf

let compile_program step =
  let pfile = lex_and_parse_program () in
  let tfile = Typechecker.type_file pfile in
  let cfile = Clockchecker.elab_file tfile in

  if (step = Check)
  then (
    print_result Clockchecker.CPMinils.print_file cfile;
    raise Done
  );

  Buffer.reset Format.stdbuf;
  let kfile =
    try Kernelizer.kernelize_file ~formatter:Format.str_formatter step cfile
    with Done ->
      (compile_results##setValue (Js.string (Buffer.contents Format.stdbuf));
       raise Done) in ()

let compile_and_exn step (_ : #Dom_html.event Js.t) =
  (try compile_program step with
   | Done -> ());
  Js._true

let init (_ : #Dom_html.event Js.t) =
  (by_id "check")##.onclick := Dom_html.handler (compile_and_exn Check);
  (by_id "last")##.onclick := Dom_html.handler (compile_and_exn Last);
  Js._true

let _ =
  Dom_html.window##.onload := Dom_html.handler init
