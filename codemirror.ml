(* Js_of_ocaml bindings for CodeMirror *)

open Js_of_ocaml
open Js

class type codemirror_config = object
  method lineNumbers : bool t readonly_prop
  method mode : js_string t readonly_prop
  method readOnly : bool t readonly_prop
end

class type codemirror = object
  method getValue : unit -> js_string t meth
  method setValue : js_string t -> unit meth
end

(** Start a new CodeMirror instance in a given text area *)
let fromTextArea (textArea : Dom_html.element t) (config : codemirror_config t) : codemirror t =
  Unsafe.global##._CodeMirror##fromTextArea textArea config
