(* Js_of_ocaml bindings for Ace-Editor *)

open Js_of_ocaml
open Js

type range

(** range startRow startColumn endRow endColumn *)
let range : (int -> int -> int -> int -> range t) Js.constr =
  Unsafe.global##.ace##._Range

type marker

class type annotation = object
  method row : int readonly_prop
  method column : int readonly_prop
  method text : js_string t readonly_prop
  method _type : js_string t readonly_prop
end

class type ace_session = object
  method setMode : js_string t -> unit meth
  method setValue : js_string t -> unit meth
  method addMarker : range t -> js_string t -> js_string t -> bool -> marker t meth
  method removeMarker : marker t -> unit meth
  method getMarkers : bool -> marker t js_array t meth
  method setAnnotations : annotation t js_array t -> unit meth
  method clearAnnotations : unit -> unit meth
end

(** Remove all the front markers in the session *)
let clear_markers (session : ace_session t) =
  let markers = session##getMarkers true in
  let markers = Js.Unsafe.global##._Object##keys markers in
  let markers = Js.to_array markers in
    Array.iter (fun m -> session##removeMarker m) markers

class type ace = object
  method session : ace_session t readonly_prop
  method getValue : unit -> js_string t meth
  method setValue : js_string t -> unit meth
  method setReadOnly : bool -> unit meth
  method setOption : js_string t -> js_string t -> unit meth
  method on : js_string t -> (unit -> unit) -> unit meth
end

let edit (div : string) : ace Js.t =
  Unsafe.global##.ace##edit (Js.string div)
