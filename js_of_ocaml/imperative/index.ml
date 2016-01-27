let module H = Tyxml_js.Html5 in 
let module Html = Dom_html in 
(* Find the initial elements *)
let body = Html.getElementById "body" in 
let container = Html.getElementById "container" in 
(* pair template *)
let newPair () = H.(
  let left = div ~a:[a_class ["pair-elem"; "pair-left"]] [] in 
  let separator = div ~a:[a_class ["pair-separator"]] [pcdata ","] in 
  let right = div ~a:[a_class ["pair-elem"; "pair-right"]] [] in 
  div ~a:[a_class ["pair"]; a_contenteditable false] [
    left;
    separator;
    right
  ]
) in 
(* instructions *)
let instruction keybinding meaning = H.(
  div ~a:[a_class ["instruction"]] [
    span ~a:[a_class ["keybinding"]] [pcdata keybinding];
    span ~a:[a_class ["meaning"]] [pcdata meaning]
  ]
) in 
let instructions = H.(
  div ~a:[a_id "instructions"] [
    h3 [pcdata "Instructions"];
    div ~a:[a_class ["instruction"]] [
      span ~a:[a_class ["keybinding"]; a_style "visibility: hidden"] [];
      span ~a:[a_class ["meaning"]] [pcdata "Enter any expression."]
    ];
    (instruction "(" "nest pair.");
    (instruction "tab" "next expr.");
    (instruction "sh+tab" "prev expr.")
  ]
) in 
let source_link = H.(
  div ~a:[a_id "github"] [
    a ~a:[a_href "https://github.com/cyrus-/nestedpairs"] [pcdata "Source on Github"]
  ]
) in 
(* Convert hello to a DOM element and append it to body *)
let append_div parent div = Dom.appendChild parent (Tyxml_js.To_dom.of_div div) in 
Html.addEventListener body Html.Event.load
  (Html.handler (fun ev -> 
    List.iter (append_div body) [instructions; source_link];
    Js._true
  )) Js._true;
