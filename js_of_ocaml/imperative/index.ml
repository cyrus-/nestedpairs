let module H = Tyxml_js.Html5 in 
(* Find the initial elements *)
let body = Dom_html.getElementById "body" in 
let container = Dom_html.getElementById "container" in 
(* pair template *)
let template = H.(
  div ~a:[a_class ["pair"]; a_contenteditable false] [
    div ~a:[a_class ["pair-elem"; "pair-left"]] [];
    div ~a:[a_class ["pair-separator"]] [pcdata ","];
    div ~a:[a_class ["pair-elem"; "pair-right"]] []
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
begin 
  Dom.appendChild body (Tyxml_js.To_dom.of_div instructions); 
  Dom.appendChild body (Tyxml_js.To_dom.of_div source_link)
end
