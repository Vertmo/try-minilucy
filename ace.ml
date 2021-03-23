(* Js_of_ocaml bindings for Ace-Editor *)

open Js_of_ocaml
open Js

class type ace_session = object
  method setMode : js_string t -> unit meth
  method setValue : js_string t -> unit meth
end

class type ace = object
  method session : ace_session t readonly_prop
  method getValue : unit -> js_string t meth
  method setValue : js_string t -> unit meth
  method setReadOnly : bool t -> unit meth
  method setOption : js_string t -> js_string t -> unit meth
  method on : js_string t -> (unit -> unit) -> unit meth
end

let edit (div : string) : ace Js.t =
  Unsafe.global##.ace##edit (Js.string div)
