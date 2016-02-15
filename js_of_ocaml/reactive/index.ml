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
      (* invalid example: (Hole str, InFst sel) *)
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

      exception Invalid
    end

    module ZString : sig
      type selected_string
      exception InvalidSS
      (* make_ss (str, direction) raises InvalidSS iff
       * str = "" and direction = Left *)
      val make_ss : string * direction -> selected_string
      val look_ss : selected_string -> string * direction
      
      type t = {
        before : string;
        selected : selected_string;
        after : string
      }

      val to_string : t -> string
    end = struct
      type selected_string = string * direction
      exception InvalidSS
      let make_ss ((str, direction) as ss) = if str == "" then 
        begin match direction with 
        | Right -> ss
        | Left -> raise InvalidSS
        end else ss
      let look_ss ss = ss
      type t = {
        before : string;
        selected : selected_string;
        after : string
      }

      let to_string {before; selected=(ss, _); after} = before ^ ss ^ after
    end
  end

  module Action = struct
    open Sel

    type t = 
      MoveSelection of HExpSel.t
    | EnterString of string
    | NewPair

    (* raises HExpSel.Invalid if not 
     * HExpSel.valid_for (hexp, hexp_sel) *)
    let rec valid_for action (hexp, hexp_sel) = match action with 
    | MoveSelection hexp_sel' -> HExpSel.valid_for (hexp, hexp_sel')
    | EnterString _ -> begin 
      match hexp_sel with 
      | HExpSel.OutPair _ -> false
      | HExpSel.PairSelected _ -> true
      | HExpSel.InHole _ -> true
      | HExpSel.InFst hexp_sel' -> (match hexp with
        | HExp.Pair (fst, _) -> valid_for action (fst, hexp_sel')
        | _ -> raise HExpSel.Invalid)
      | HExpSel.InSnd hexp_sel' -> (match hexp with 
        | HExp.Pair (_, snd) -> valid_for action (snd, hexp_sel')
        | _ -> raise HExpSel.Invalid) 
    end
    | NewPair -> true

    exception Invalid
  end

  module Models = struct
    open Sel

    (* BModel and ZModel are isomorphic, mediated by of_z and of_b *)
    module rec BModel : sig
      type u = HExp.t * HExpSel.t
      type t

      (* make u raises HExpSel.Invalid if 
       * not HExpSel.valid_for u *)
      val make : u -> t

      (* invariant: HExpSel.valid_for(look b) *) 
      val look : t -> u

      val of_z : ZModel.t -> t

      val hexp_of : t -> HExp.t
      val sel_of : t -> HExpSel.t

      (* apply b action raises Action.Invalid if 
       * not Action.valid_for action b *)
      val apply : t -> Action.t -> t
    end = struct
      exception BadInvariant

      type u = HExp.t * HExpSel.t
      type t = u

      let make u = 
        if HExpSel.valid_for u then u else raise HExpSel.Invalid

      let look u = u

      let rec of_z z = 
        let open HExp in
        let open HExpSel in 
        let open ZString in 
        let open StringSel in 
        let open ZModel in 
        match z with 
        | ZOutPair (direction, fst, snd) -> (Pair(fst, snd), OutPair direction)
        | ZPairSelected (direction, fst, snd) -> (Pair(fst, snd), PairSelected direction)
        | ZInHole {before=before; selected=selected; after=after} -> 
            let (ss, _) = ZString.look_ss selected in 
            (Hole (before ^ ss ^ after), 
             InHole {startIdx=0; endIdx=0} (* TODO *))
        | ZInFst (fst, snd) -> 
            let (hexp_fst, sel_fst) = of_z fst in 
            (Pair (hexp_fst, snd), InFst sel_fst)
        | ZInSnd (fst, snd) -> 
            let (hexp_snd, sel_snd) = of_z snd in 
            (Pair (fst, hexp_snd), InSnd sel_snd)

      let hexp_of (hexp, _) = hexp
      let sel_of (_, sel) = sel

      let rec apply (hexp, sel) action = 
        let open HExp in 
        let open HExpSel in 
        let open StringSel in 
        match action with 
        | Action.MoveSelection sel' -> 
          if HExpSel.valid_for (hexp, sel') then 
            (hexp, sel') 
          else raise Action.Invalid
        | Action.EnterString str -> begin
            match (hexp, sel) with 
            | (_, OutPair _) -> raise Action.Invalid
            | (Pair _, PairSelected _) -> (Hole str, 
              let len = String.length str in 
              InHole {startIdx=len; endIdx=len})
            | (_, PairSelected _) -> raise BadInvariant
            | (Hole str', InHole {startIdx=startIdx; endIdx=endIdx}) -> 
                (Hole "", 
                 InHole {startIdx=0; endIdx=0}) (* TODO *)
            | (_, InHole _) -> raise BadInvariant
            | (Pair (fst, snd), InFst sel') -> 
                let (fst', sel'') = apply (fst, sel') action in 
                (Pair (fst', snd), InFst sel'') 
            | (_, InFst _) -> raise BadInvariant
            | (Pair (fst, snd), InSnd sel') -> 
                let (snd', sel'') = apply (snd, sel') action in 
                (Pair (fst, snd'), InSnd sel'')
            | (_, InSnd _) -> raise BadInvariant
          end 
        | Action.NewPair -> begin
            match (hexp, sel) with
            | (Pair _, OutPair Left) -> (Pair(Hole "", hexp), InFst(InHole {startIdx=0; endIdx=0}))
            | (Pair _, OutPair Right) -> (Pair(hexp, Hole ""), InSnd(InHole {startIdx=0; endIdx=0}))
            | (_, OutPair _) -> raise BadInvariant
            | (Pair _, PairSelected Left) -> (Pair(Hole "", hexp), sel)
            | (Pair _, PairSelected Right) -> (Pair(hexp, Hole ""), sel)
            | (_, PairSelected _) -> raise BadInvariant
            | (Hole str, InHole _) -> (Pair (hexp, Hole ""), InFst(sel))
            | (_, InHole _) -> raise BadInvariant
            | (Pair (fst, snd), InFst sel') -> 
              let (fst', sel'') = apply (fst, sel') action in 
              (Pair (fst', snd), InFst sel'')
            | (_, InFst _) -> raise BadInvariant
            | (Pair (fst, snd), InSnd sel') ->
              let (snd', sel'') = apply (snd, sel') action in 
              (Pair (fst, snd'), InSnd sel'')
            | (_, InSnd _) -> raise BadInvariant
          end
    end and ZModel : sig
      type t = 
        ZOutPair of direction * HExp.t * HExp.t
      | ZPairSelected of direction * HExp.t * HExp.t
      | ZInHole of ZString.t
      | ZInFst of t * HExp.t
      | ZInSnd of HExp.t * t

      (* of_u u raises HExpSel.Invalid if
        * not HExpSel.valid_for u *)
      val of_u : BModel.u -> t

      val of_b : BModel.t -> t

      val hexp_of : t -> HExp.t
      val sel_of : t -> HExpSel.t

      val apply : t -> Action.t -> t
    end = struct
      type t = 
        ZOutPair of direction * HExp.t * HExp.t
      | ZPairSelected of direction * HExp.t * HExp.t
      | ZInHole of ZString.t
      | ZInFst of t * HExp.t
      | ZInSnd of HExp.t * t

      let rec of_u u = 
        let open HExp in
        let open HExpSel in 
        let open StringSel in 
        match u with 
        | (Pair (fst, snd), OutPair direction) -> ZOutPair (direction, fst, snd)
        | (_, OutPair _) -> raise HExpSel.Invalid
        | (Pair (fst, snd), PairSelected direction) -> ZPairSelected (direction, fst, snd)
        | (_, PairSelected _) -> raise HExpSel.Invalid
        | (Hole str, InHole {startIdx; endIdx}) -> 
            ZInHole (raise NotImplemented) (* TODO *)
        | (_, InHole _) -> raise HExpSel.Invalid
        | (Pair (fst, snd), InFst sel') -> ZInFst ((of_u (fst, sel')), snd)
        | (_, InFst _) -> raise HExpSel.Invalid
        | (Pair (fst, snd), InSnd sel') -> ZInSnd (fst, (of_u (snd, sel')))
        | (_, InSnd _) -> raise HExpSel.Invalid

      let rec of_b b = of_u (BModel.look b)

      let rec hexp_of z = HExp.(match z with 
      | ZOutPair (_, fst, snd) -> Pair (fst, snd)
      | ZPairSelected (_, fst, snd) -> Pair (fst, snd)
      | ZInHole ss -> Hole (ZString.to_string ss)
      | ZInFst (fst, snd) -> Pair ((hexp_of fst), snd)
      | ZInSnd (fst, snd) -> Pair (fst, (hexp_of snd)))

      let rec sel_of z = HExpSel.(match z with 
      | ZOutPair (direction, _, _) -> OutPair direction
      | ZPairSelected (direction, _, _) -> PairSelected direction
      | ZInHole ss -> InHole StringSel.({startIdx=0; endIdx=0}) (* TODO *)
      | ZInFst (fst, snd) -> InFst (sel_of fst)
      | ZInSnd (fst, snd) -> InSnd (sel_of snd))

      let rec apply z action = 
        match action with 
        | Action.MoveSelection sel' -> of_b (BModel.apply (BModel.of_z z) action)
        | Action.EnterString str -> begin
            let open ZString in 
            match z with 
            | ZOutPair _ -> raise Action.Invalid
            | ZPairSelected _ -> ZInHole {
                before=str;
                selected=ZString.make_ss ("", Right);
                after=""}
            | ZInHole {before; selected; after} -> ZInHole {before; selected; after} (* TODO *)
            | ZInFst (fst, snd) -> ZInFst ((apply fst action), snd)
            | ZInSnd (fst, snd) -> ZInSnd (fst, (apply snd action))
          end
        | Action.NewPair -> begin
            let open ZString in 
            match z with 
            | ZOutPair (Left, fst, snd) -> 
                ZInFst (
                  ZInHole {before=""; selected=ZString.make_ss ("", Right); after=""},
                  HExp.Pair (fst, snd))
            | ZOutPair (Right, fst, snd) -> 
                ZInSnd (
                  HExp.Pair (fst, snd),
                  ZInHole {before=""; selected=ZString.make_ss ("", Right); after=""})
            | ZPairSelected (Left, fst, snd) -> 
                ZPairSelected(Left, HExp.Hole "", HExp.Pair(fst, snd))
            | ZPairSelected (Right, fst, snd) ->
                ZPairSelected(Right, HExp.Pair(fst, snd), HExp.Hole "")
            | ZInHole _ -> ZInFst (z, HExp.Hole "")
            | ZInFst (fst, snd) -> 
                let fst' = apply fst action in 
                ZInFst (fst', snd)
            | ZInSnd (fst, snd) -> 
                let snd' = apply snd action in 
                ZInSnd (fst, snd')
          end
    end

    module type ABSMODEL = sig
      type t

      val of_b : BModel.t -> t
      val of_z : ZModel.t -> t
      val to_b : t -> BModel.t
      val to_z : t -> ZModel.t

      val hexp_of : t -> HExp.t
      val sel_of : t -> HExpSel.t

      val apply : t -> Action.t -> t
    end

    module AbsBModel : ABSMODEL = struct
      type t = BModel.t

      let of_b b = b
      let of_z = BModel.of_z
      let to_b b = b
      let to_z = ZModel.of_b

      let hexp_of = BModel.hexp_of
      let sel_of = BModel.sel_of

      let apply = BModel.apply
    end

    module AbsZModel : ABSMODEL = struct
      type t = ZModel.t

      let of_b = ZModel.of_b
      let of_z z = z
      let to_b = BModel.of_z
      let to_z z = z

      let hexp_of = ZModel.hexp_of
      let sel_of = ZModel.sel_of

      let apply = ZModel.apply
    end
  end

  module StringView(Model : Models.ABSMODEL) = struct
    let view (model : Model.t) : string = raise NotImplemented
  end

  module ReactiveStringView(Model : Models.ABSMODEL) = struct 
    (* make an action stream *)
    (* make a model stream that reacts to action stream *)
    (* make a view stream that reacts to model stream *)
  end
end 

