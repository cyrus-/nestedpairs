module NestedPairs = struct
  module Model = struct
    (* expressions with holes *)
    type hexp = 
      Hole of string
    | Pair of hexp * hexp

    (* string with selection *)
    type string_sel = {
      beforeSel : string;
      inSel : string;
      afterSel : string
    }

    (* expressions with holes and a single selection *)
    type hexp_sel = 
      SelHole of string_sel (* e.g. [abc{def}ghi] where {} indicates selection *)
    | AtLeftOfPair of hexp * hexp (* e.g. ([], {}([], [])) *)
    | AtRightOfPair of hexp * hexp (* e.g. ([], ([], []){}) *)
    | SelPair of hexp * hexp (* e.g. ([], {([], [])}) *)
    | InLeftOfPair of hexp_sel * hexp (* selection is in left component *)
    | InRightOfPair of hexp * hexp_sel (* selection is in right component *)

    let empty_selhole = SelHole {beforeSel=""; inSel=""; afterSel=""}

    (* remove_selection : hexp_sel -> hexp *)
    let rec remove_selection s = match s with 
    | SelHole {beforeSel=b; inSel=i; afterSel=a} -> Hole (b ^ i ^ a)
    | AtLeftOfPair (left, right) -> Pair (left, right)
    | AtRightOfPair (left, right) -> Pair (left, right)
    | SelPair (left, right) -> Pair (left, right)
    | InLeftOfPair (left, right) -> Pair ((remove_selection left), right)
    | InRightOfPair (left, right) -> Pair (left, (remove_selection right))

    (* in_unfilled_hole : hexp_sel -> bool *)
    let rec in_unfilled_hole s = match s with 
    | SelHole _ -> true
    | AtLeftOfPair _ -> false
    | AtRightOfPair _ -> false
    | SelPair _ -> false
    | InLeftOfPair (left, _) -> in_unfilled_hole left
    | InRightOfPair (_, right) -> in_unfilled_hole right
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
  end 
end 