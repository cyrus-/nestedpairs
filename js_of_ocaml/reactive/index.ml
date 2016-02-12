module NestedPairs = struct
  exception InvariantViolated 

  (* expressions with holes *)
  module HExp = struct 
    type hexp = Pair of hexp * hexp | Hole of string
  end

  (* selections *)
  module Sel = struct
    type string_sel = {startIdx : int; endIdx : int}
    type direction = Left | Right
    type hexp_sel = 
      OutPair of direction (* ([], {}([], [])) = InSnd(OutPair Left) *)
    | PairSelected of direction (* ([], |{([], [])}) = InSnd(PairSelected Left) *)
    | InHole of string_sel 
    | InFst of hexp_sel 
    | InSnd of hexp_sel

    (* does the string_sel stay within the bounds of str? *) 
    let valid_string_sel {startIdx=startIdx; endIdx=endIdx} str = (
      let str_length = String.length str in
      (startIdx >= 0) &&
      (startIdx < str_length) &&
      (endIdx >= 0) &&
      (endIdx < str_length)
    )

    (* does the hexp_sel stay within the bounds of the hexp? *)
    (* invalid example: (InFst sel, Hole str) *)
    let rec valid_hexp_sel hexp_sel hexp = HExp.(match (hexp_sel, hexp) with 
    | (OutPair _, Pair _) -> true
    | (OutPair _, _) -> false
    | (PairSelected _, Pair _) -> true 
    | (PairSelected _, _) -> false
    | (InHole str_sel, Hole str) -> valid_string_sel str_sel str
    | (InHole _, _) -> false
    | (InFst hexp_sel', Pair (fst, _)) -> valid_hexp_sel hexp_sel' fst
    | (InFst _, _) -> false
    | (InSnd hexp_sel', Pair (_, snd)) -> valid_hexp_sel hexp_sel' snd
    | (InSnd _, _) -> false)
  end

  module Action = struct
    type action = 
      MoveSelection of Sel.hexp_sel
    | EnterString of string
    | NewPair

    (* invariant: valid_hex_sel hexp_sel hexp *)
    let rec is_valid action (hexp, hexp_sel) = match action with 
    | MoveSelection hexp_sel' -> Sel.valid_hexp_sel hexp_sel' hexp
    | EnterString _ -> Sel.(match hexp_sel with 
      | OutPair _ -> false
      | PairSelected _ -> true
      | InHole _ -> true
      | InFst hexp_sel' -> (match hexp with
        | HExp.Pair (fst, _) -> is_valid action (fst, hexp_sel')
        | _ -> raise InvariantViolated)
      | InSnd hexp_sel' -> (match hexp with 
        | HExp.Pair (_, snd) -> is_valid action (snd, hexp_sel')
        | _ -> raise InvariantViolated))
    | NewPair -> true

    exception InvalidAction
  end

  module type MODEL = sig
    type t

    val make : (HExp.hexp * Sel.hexp_sel) -> t
    val hexp_of : t -> HExp.hexp
    val sel_of : t -> Sel.hexp_sel

    val apply : t -> Action.action -> t
  end

  (* simplest implementation of MODEL *)
  module BasicModel : MODEL = struct 
    open HExp
    open Sel
    open Action

    type t = hexp * hexp_sel

    let make (hexp, hexp_sel) = (hexp, hexp_sel)
    let hexp_of (hexp, _) = hexp
    let sel_of (_, hexp_sel) = hexp_sel

    (* if not is_valid action (hexp, sel) then raises InvalidAction *)
    let rec apply (hexp, sel) action = match action with 
      | Action.MoveSelection sel' -> 
        if valid_hexp_sel sel' hexp then 
          (hexp, sel') 
        else raise InvalidAction
      | Action.EnterString str -> (match (hexp, sel) with 
        | (_, OutPair _) -> raise InvalidAction
        | (Pair _, PairSelected _) -> (Hole str, 
          let len = String.length str in 
          InHole {startIdx=len; endIdx=len})
        | (_, PairSelected _) -> raise InvariantViolated
        | (Hole str', InHole {startIdx=startIdx; endIdx=endIdx}) -> (
          let spliced_str = ""(* String.splice str' (startIdx, length) str *) in (* TODO: calculate spliced str *) 
          (Hole spliced_str, let loc = startIdx + (String.length str') in InHole {startIdx=loc; endIdx=loc}))
        | (_, InHole _) -> raise InvariantViolated
        | (Pair (fst, snd), InFst sel') -> 
          let (fst', sel'') = apply (fst, sel') action in 
          (Pair (fst', snd), InFst sel'') 
        | (_, InFst _) -> raise InvariantViolated
        | (Pair (fst, snd), InSnd sel') -> 
          let (snd', sel'') = apply (snd, sel') action in 
          (Pair (fst, snd'), InSnd sel'')
        | (_, InSnd _) -> raise InvariantViolated
      )
      | Action.NewPair -> (match (hexp, sel) with
        | (Pair _, OutPair Left) -> (Pair(Hole "", hexp), InFst(InHole {startIdx=0; endIdx=0}))
        | (Pair _, OutPair Right) -> (Pair(hexp, Hole ""), InSnd(InHole {startIdx=0; endIdx=0}))
        | (_, OutPair _) -> raise InvariantViolated
        | (Pair _, PairSelected Left) -> (Pair(Hole "", hexp), sel)
        | (Pair _, PairSelected Right) -> (Pair(hexp, Hole ""), sel)
        | (_, PairSelected _) -> raise InvariantViolated
        | (Hole str, InHole _) -> (Pair (hexp, Hole ""), InFst(sel))
        | (_, InHole _) -> raise InvariantViolated
        | (Pair (fst, snd), InFst sel') -> 
          let (fst', sel'') = apply (fst, sel') action in 
          (Pair (fst', snd), InFst sel'')
        | (_, InFst _) -> raise InvariantViolated
        | (Pair (fst, snd), InSnd sel') ->
          let (snd', sel'') = apply (snd, sel') action in 
          (Pair (fst, snd'), InSnd sel'')
        | (_, InSnd _) -> raise InvariantViolated
      )
  end

  module StringView(Model : MODEL) = struct
    exception NotImplemented
    let view (model : Model.t) : string = raise NotImplemented
  end

  module ReactiveStringView = struct 
    (* make an action stream *)
    (* make a model stream that reacts to action stream *)
    (* make a view stream that reacts to model stream *)
  end

(*   module ZipperModel : MODEL = struct
    type t = SelHole of string_sel (* e.g. [abc{def}ghi] where {} indicates selection *)
    | AtLeftOfPair of hexp * hexp (* e.g. ([], {}([], [])) *)
    | AtRightOfPair of hexp * hexp e.g. ([], ([], []){})
    | SelPair of hexp * hexp (* e.g. ([], {([], [])}) *)
    | InLeftOfPair of t * hexp (* selection is in left component *)
    | InRightOfPair of hexp * t (* selection is in right component *)
  end
 *)
(*   module ZipperModel : MODEL = struct 
    (* string with selection *)
    type string_sel = {
      beforeSel : string;
      inSel : string;
      afterSel : string
    }

    (* expressions with holes and a single selection *)
    type t = 
      SelHole of string_sel (* e.g. [abc{def}ghi] where {} indicates selection *)
    | AtLeftOfPair of hexp * hexp (* e.g. ([], {}([], [])) *)
    | AtRightOfPair of hexp * hexp (* e.g. ([], ([], []){}) *)
    | SelPair of hexp * hexp (* e.g. ([], {([], [])}) *)
    | InLeftOfPair of t * hexp (* selection is in left component *)
    | InRightOfPair of hexp * t (* selection is in right component *)

    let empty_selhole = SelHole {beforeSel=""; inSel=""; afterSel=""}

    (* remove_selection : hexp_sel -> hexp *)
    let rec remove_selection s = match s with 
    | SelHole {beforeSel=b; inSel=i; afterSel=a} -> Hole (b ^ i ^ a)
    | AtLeftOfPair (left, right) -> Pair (left, right)
    | AtRightOfPair (left, right) -> Pair (left, right)
    | SelPair (left, right) -> Pair (left, right)
    | InLeftOfPair (left, right) -> Pair ((remove_selection left), right)
    | InRightOfPair (left, right) -> Pair (left, (remove_selection right))

    let rec cur_path s = match s with 
    | SelHole {beforeSel=b; inSel=i; afterSel=a} -> InHole {startIdx=String.length b; length=String.length i}
    | AtLeftOfPair _ -> LeftOfPair
    | AtRightOfPair _ -> RightOfPair
    | SelPair (left, right) -> PairSelected
    | InLeftOfPair (left, _) -> InLeftComponent (cur_path left)
    | InRightOfPair (_, right) -> InRightComponent (cur_path right)
  end 

  module Action = struct
    open Model 

    (* still need movement actions *)
    type action = 
      ReplaceSelection of string
    | NewPair

    let rec is_valid_action s action = match s with 
    | SelHole _ -> true
    | AtLeftOfPair _ -> (match action with
      | ReplaceSelection _ -> false
      | NewPair -> true)
    | AtRightOfPair _ -> (match action with
      | ReplaceSelection _ -> false
      | NewPair -> true)
    | SelPair _ -> true
    | InLeftOfPair (left, _) -> is_valid_action left action
    | InRightOfPair (_, right) -> is_valid_action right action

    exception InvalidAction

    (* invariant: if (is_valid_action action) then 
                  apply s action does not raise InvalidAction. *)
    let rec apply s action = match action with 
    | ReplaceSelection newstr -> (match s with 
      | SelHole strsel -> SelHole {strsel with inSel=newstr}
      | AtLeftOfPair _ -> raise InvalidAction
      | AtRightOfPair _ -> raise InvalidAction
      | SelPair _ -> SelHole {beforeSel=newstr; inSel=""; afterSel=""}
      | InLeftOfPair (left, right) -> InLeftOfPair ((apply left action), right)
      | InRightOfPair (left, right) -> InRightOfPair (left, (apply right action)))
    | NewPair -> (match s with 
      | SelHole strsel -> InLeftOfPair (s, Hole "")
      | AtLeftOfPair _ -> InRightOfPair ((remove_selection s), empty_selhole)
      | AtRightOfPair _ -> InRightOfPair ((remove_selection s), empty_selhole)
      | SelPair _ -> InLeftOfPair (s, Hole "")
      | InLeftOfPair (left, right) -> InLeftOfPair ((apply left action), right)
      | InRightOfPair (left, right) -> InRightOfPair (left, (apply right action))) 
  end  *)
end 

