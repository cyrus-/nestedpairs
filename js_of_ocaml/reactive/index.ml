module NestedPairs = struct
  exception NotImplemented

  (* expressions with holes *)
  module HExp = struct 
    type t = Pair of t * t | Hole of string

    let rec num_holes hexp = match hexp with
    | Pair (fst, snd) -> (num_holes fst) + (num_holes snd)
    | Hole _ -> 1

    let rec num_pairs hexp = match hexp with
    | Pair (fst, snd) -> (num_pairs fst) + (num_pairs snd) + 1
    | Hole _ -> 0

    let rec depth hexp = match hexp with
    | Pair (fst, snd) -> (max (depth fst) (depth snd)) + 1
    | Hole _ -> 0
  end

  module Sel = struct
    type direction = Left | Right

   (* selections within a string *)
    module StringSel = struct
      type t = {startIdx : int; endIdx : int}

      (* if startIdx > endIdx, then the cursor is to the left.
       * otherwise, it is to the right. *)
      let direction_of {startIdx=startIdx; endIdx=endIdx} = 
        if startIdx > endIdx then Left else Right

      (* does the string_sel stay within the bounds of str? *) 
      let valid_for (str, {startIdx=startIdx; endIdx=endIdx}) = (
        let str_length = String.length str in
        (startIdx >= 0) &&
        (startIdx < str_length) &&
        (endIdx >= 0) &&
        (endIdx < str_length)
      )

      (* absolute length of the selection *)
      let length {startIdx=startIdx; endIdx=endIdx} =
        if startIdx > endIdx then startIdx - endIdx else endIdx - startIdx
    end

    (* selections within an hexp *)
    module HExpSel = struct
      type t = 
        OutPair of direction (* ([], {}([], [])) = InSnd(OutPair Left) *)
      | PairSelected of direction (* ([], |{([], [])}) = InSnd(PairSelected Left) *)
      | InHole of StringSel.t 
      | InFst of t 
      | InSnd of t

      (* does the hexp_sel stay within the bounds of the hexp? *)
      (* invalid example: (InFst sel, Hole str) *)
      let rec valid_for v = HExp.(match v with 
      | (Pair _, OutPair _) -> true
      | (_, OutPair _) -> false
      | (Pair _, PairSelected _) -> true 
      | (_, PairSelected _) -> false
      | (Hole str, InHole str_sel) -> StringSel.valid_for(str, str_sel)
      | (_, InHole _) -> false
      | (Pair(fst, _), InFst hexp_sel') -> valid_for (fst, hexp_sel')
      | (_, InFst _) -> false
      | (Pair(_, snd), InSnd hexp_sel') -> valid_for (snd, hexp_sel')
      | (_, InSnd _) -> false)
    end

    module ZString = struct
      type selected_string = string * direction
      type t = {
        before : string;
        selected : selected_string;
        after : string
      }
    end
  end

  module Action = struct
    open Sel

    type t = 
      MoveSelection of HExpSel.t
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

  module Models = struct
    open Sel

    (* BModel and ZModel are isomorphic, mediated by of_zmodel and of_bmodel *)
    module rec BModel : sig
      type v = HExp.t * HExpSel.t
      type t
      exception Invalid

      (* make v raises Invalid if 
       * not HExpSel.valid_for v *)
      val make : v -> t

      (* invariant: HExpSel.valid_for(view b) *) 
      val view : t -> v

      val of_z : ZModel.t -> t
    end = struct
      type v = HExp.t * HExpSel.t
      type t = v
      exception Invalid

      let make v = 
        if HExpSel.valid_for v then v else raise Invalid

      let view v = v

      let rec of_z z = 
        let open HExp in
        let open HExpSel in 
        let open ZString in 
        let open StringSel in 
        let open ZModel in 
        match z with 
        | ZOutPair (direction, fst, snd) -> (Pair(fst, snd), OutPair direction)
        | ZPairSelected (direction, fst, snd) -> (Pair(fst, snd), PairSelected direction)
        | ZInHole {before=before; selected=(selected, direction); after=after} -> 
            (Hole (before ^ selected ^ after), 
             InHole {startIdx=0; endIdx=0} (* TODO *))
        | ZInFst (fst, snd) -> 
            let (hexp_fst, sel_fst) = of_z fst in 
            (Pair (hexp_fst, snd), InFst sel_fst)
        | ZInSnd (fst, snd) -> 
            let (hexp_snd, sel_snd) = of_z snd in 
            (Pair (fst, hexp_snd), InSnd sel_snd)
    end and ZModel : sig
      type t = 
        ZOutPair of direction * HExp.t * HExp.t
      | ZPairSelected of direction * HExp.t * HExp.t
      | ZInHole of ZString.t
      | ZInFst of t * HExp.t
      | ZInSnd of HExp.t * t

      (* of_v v raises BModel.Invalid if
        * not HExpSel.valid_for v *)
      val of_v : BModel.v -> t

      val of_b : BModel.t -> t
    end = struct
      type t = 
        ZOutPair of direction * HExp.t * HExp.t
      | ZPairSelected of direction * HExp.t * HExp.t
      | ZInHole of ZString.t
      | ZInFst of t * HExp.t
      | ZInSnd of HExp.t * t

      let rec of_v v = 
        let open HExp in
        let open HExpSel in 
        let open StringSel in 
        match v with 
        | (Pair (fst, snd), OutPair direction) -> ZOutPair (direction, fst, snd)
        | (_, OutPair _) -> raise BModel.Invalid
        | (Pair (fst, snd), PairSelected direction) -> ZPairSelected (direction, fst, snd)
        | (_, PairSelected _) -> raise BModel.Invalid
        | (Hole str, InHole {startIdx; endIdx}) -> 
            ZInHole (raise NotImplemented) (* TODO *)
        | (_, InHole _) -> raise BModel.Invalid
        | (Pair (fst, snd), InFst sel') -> ZInFst ((of_v (fst, sel')), snd)
        | (_, InFst _) -> raise BModel.Invalid
        | (Pair (fst, snd), InSnd sel') -> ZInSnd (fst, (of_v (snd, sel')))
        | (_, InSnd _) -> raise BModel.Invalid

      let rec of_b b = of_v (BModel.view b)
    end

    module type ABSMODEL = sig
      type t

      val of_b : BModel.t -> t
      val of_z : ZModel.t -> t
      val to_b : t -> BModel.t
      val to_z : t -> ZModel.t

      (*val apply : t -> Action.action -> t*)
    end

    module AbsBModel : ABSMODEL = struct
      type t = BModel.t

      let of_b b = b
      let of_z = BModel.of_z
      let to_b b = b
      let to_z = ZModel.of_b
    end

    module AbsZModel : ABSMODEL = struct
      type t = ZModel.t

      let of_b = ZModel.of_b
      let of_z z = z
      let to_b = BModel.of_z
      let to_z z = z
    end
  end


  (* simplest implementation of MODEL *)
  module AbsSelModel : ABSMODEL = struct 
    open HExp
    open Sel
    open Action

    type t = bmodel

    let bmake bmodel = bmodel
    let get_sel bmodel = bmodel
    let zmake zmodel = BModel.from_z zmodel
    let get_zip bmodel = ZipperModel.from_b bmodel

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

