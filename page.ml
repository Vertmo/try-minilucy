(* Handling of the webpage *)

open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5

let by_id s = Dom_html.getElementById s
let of_node = Tyxml_js.To_dom.of_node

(** Manipulate the console *)
module Console = struct
  (** access to the console *)
  let console = by_id "console"

  (** Scroll the console to the bottom *)
  let scroll child =
    console##.scrollTop := child##.offsetTop

  (** Log a message to the console *)
  let log msg =
    let newLine = of_node (T.(li [txt msg])) in
    ignore (console##appendChild newLine);
    scroll (Js.Unsafe.coerce newLine)

  (** Log an error message to the console *)
  let error msg =
    let newLine = of_node (T.(li ~a:[a_class ["console-error"]] [txt msg])) in
    ignore (console##appendChild newLine);
    scroll (Js.Unsafe.coerce newLine)
end

