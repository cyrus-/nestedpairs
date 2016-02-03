module X = Tyxml_js.Html5
module Html = Dom_html
module ToDom = Tyxml_js.To_dom

module JsUtil = struct 
  exception OptFail
  let getOpt x = Js.Opt.get x (fun _ -> raise OptFail)
end 

module HtmlUtil = struct 
  (* The API doesn't support focus() on non-input elements, so need to do this unsafely *)
  let emptyUnsafeArray = (Array.make 0 (Js.Unsafe.inject ()))
  let focus elem = 
    let _  = Js.Unsafe.meth_call elem "focus" emptyUnsafeArray in ()

  let js_string_of_bool b = if b then Js.string "true" else Js.string "false"

  let setContentEditable elem flag = 
    elem##setAttribute ((Js.string "contenteditable"), js_string_of_bool flag)
end

module KeyCodes = struct
  let kc_LPAREN = 40
end

module Pair = struct 
  exception NoTarget

  let separator () = ToDom.of_div X.(
    div ~a:[a_class ["pair-separator"]] [pcdata " , "]
  )

  let rec hole className contents = 
    let h = ToDom.of_div X.(
      div ~a:[
        a_class ["pair-elem"; className]; 
        a_contenteditable true] [pcdata contents]
    ) in 
    h##onkeypress <- (Html.handler (fun evt -> 
      let target = JsUtil.getOpt (evt##target) in 
      let keyCode = evt##keyCode in 
      if target = h then begin
        if keyCode = KeyCodes.kc_LPAREN then begin
          HtmlUtil.setContentEditable h false;
          let contents = Js.to_string (JsUtil.getOpt (h##textContent)) in 
          h##innerHTML <- (Js.string "");
          let p = newPair contents in 
          let p_left = Js.Opt.get (p##firstChild) (fun _ -> raise NoTarget) in 
          Dom.appendChild h p;
          HtmlUtil.focus (p_left);
          Js._false
        end else Js._true
      end else Js._true 
      ));
    h
  
  and leftHole contents = hole "pair-left" contents
  and rightHole () = hole "pair-right" ""

  and newPair left_contents = 
    let pair = ToDom.of_div X.(
      div ~a:[a_class ["pair"]; a_contenteditable false] []
    ) in 
    Dom.appendChild pair (leftHole left_contents);
    Dom.appendChild pair (separator ());
    Dom.appendChild pair (rightHole ());
    pair
end

(* Find the initial elements *)
let body = Html.getElementById "body" in 
let container = Html.getElementById "container" in 
(* pair template *)
(* instructions *)
let instruction keybinding meaning = X.(
  div ~a:[a_class ["instruction"]] [
    span ~a:[a_class ["keybinding"]] [pcdata keybinding];
    span ~a:[a_class ["meaning"]] [pcdata meaning]
  ]
) in 
let instructions = X.(
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
let source_link = X.(
  div ~a:[a_id "github"] [
    a ~a:[a_href "https://github.com/cyrus-/nestedpairs/tree/master/js_of_ocaml/imperative"] [pcdata "Source on Github"]
  ]
) in 
(* Convert hello to a DOM element and append it to body *)
let append_div parent div = Dom.appendChild parent (ToDom.of_div div) in 
Html.window##onload <- (Html.handler (fun ev -> 
    List.iter (append_div body) [
      instructions; 
      source_link];
    let rootPair = Pair.newPair "" in 
    Dom.appendChild container rootPair;
    HtmlUtil.focus (rootPair##firstChild);
    Js._true
  ))
