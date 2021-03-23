(* Handling of the webpage *)

open Js_of_ocaml
open Js_of_ocaml_tyxml
module T = Tyxml_js.Html5

let by_id s = Dom_html.getElementById s
let of_node = Tyxml_js.To_dom.of_node

(** Get the program kept in session storage *)
let get_saved_program () =
  Js.Optdef.case Dom_html.window##.sessionStorage
    (fun () -> Js.string "")
    (fun stor ->
       Js.Opt.case (stor##getItem (Js.string "saved_program"))
         (fun () -> Js.string "")
         (fun s -> s))

(** Save a program in session storage *)
let save_program prog =
  Js.Optdef.case Dom_html.window##.sessionStorage
    (fun () -> ())
    (fun stor ->
       stor##setItem (Js.string "saved_program") prog)

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
