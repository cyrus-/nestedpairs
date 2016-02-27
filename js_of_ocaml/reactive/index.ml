module NestedPairs = struct
  exception NotImplemented

  module SSSS = String

  (* expressions with holes *)
  module HExp = struct 
    type t = Pair of t * t | Hole of string
   
    (* don't need the following helper functions yet, but will 
     * eventually for profiling... *)

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
      let direction_of {startIdx; endIdx} = 
        if startIdx > endIdx then Left else Right

      (* does the ssel stay within the bounds of str? *) 
      let valid_for (str, {startIdx; endIdx}) = 
        let str_length = String.length str in
        (startIdx >= 0) &&
        (startIdx < str_length) &&
        (endIdx >= 0) &&
        (endIdx < str_length)
      
      (* absolute length of the selection *)
      let length {startIdx; endIdx} =
        if startIdx > endIdx then startIdx - endIdx else endIdx - startIdx

      (* the left-most index *)
      let sub_start {startIdx; endIdx} = 
        if startIdx > endIdx then endIdx else startIdx

      (* the right-most index *)
      let sub_end {startIdx; endIdx} = 
        if startIdx > endIdx then startIdx else endIdx

      (* extracts the selection from the provided string.
       * May raise Invalid_argument if ssel is invalid *)
      let sub str ssel =
        let start = sub_start ssel in 
        let len = length ssel in 
        String.sub str start len

      (* extracts the string before the selection from the
       * provided string. May raise Invalid_argument if sel
       * is invalid. *)
      let before str ssel = 
        let start = sub_start ssel in 
        String.sub str 0 start

      (* extracts the string after the selection from the
       * provided string. May raise Invalid_argument if sel
       * is invalid *)
      let after str ssel = 
        let sub_end = sub_end ssel in 
        let len = String.length str in 
        let after_len = (len - sub_end) in 
        String.sub str sub_end after_len

      (* Constructs a new string by replacing the selection. 
       * May raise  Invalid_argument if sel is invalid. *)
      let splice str ssel str' = 
        let len = String.length str in 
        let before = String.sub str 0 (sub_start ssel) in 
        let subend = sub_end ssel in 
        let after = String.sub str subend (len - subend) in 
        before ^ str' ^ after
    end

    (* selections starting from the root of some unspecified hexp *)
    module HSel = struct
      type t = 
        OutPair of direction (* ([], {}([], [])) = InSnd(OutPair Left) *)
      | PairSelected of direction (* ([], |{([], [])}) = InSnd(PairSelected Left) *)
      | InHole of StringSel.t 
      | InFst of t 
      | InSnd of t

      (* does the hsel stay within the bounds of the hexp? *)
      (* invalid example: (Hole str, InFst sel) *)
      let rec valid_for v = HExp.(match v with 
      | (Pair _, OutPair _) -> true
      | (_, OutPair _) -> false
      | (Pair _, PairSelected _) -> true 
      | (_, PairSelected _) -> false
      | (Hole str, InHole ssel) -> StringSel.valid_for(str, ssel)
      | (_, InHole _) -> false
      | (Pair(fst, _), InFst hsel') -> valid_for (fst, hsel')
      | (_, InFst _) -> false
      | (Pair(_, snd), InSnd hsel') -> valid_for (snd, hsel')
      | (_, InSnd _) -> false)

      exception Invalid
    end

    module ZString : sig
      (* abstract type of strings paired with directions *)
      type sd
      exception InvalidSD
      (* make_sd (str, direction) raises InvalidSD iff
       * str = "" and direction = Left *)
      val make_sd : string * direction -> sd
      val show_sd : sd -> string * direction
      
      (* type of zstrings *)
      type t = {
        before : string;
        selected : sd;
        after : string
      }

      val to_string : t -> string
      val to_string_and_sel : t -> string * StringSel.t
      val of_string_and_sel : string * StringSel.t -> t
    end = struct
      type sd = string * direction
      exception InvalidSD
      let make_sd ((str, direction) as sd) = 
        if str == "" then 
          begin match direction with 
          | Right -> sd
          | Left -> raise InvalidSD
          end 
        else sd
      let show_sd sd = sd

      type t = {
        before : string;
        selected : sd;
        after : string
      }

      let to_string {before; selected=(sds, _); after} = before ^ sds ^ after
      let to_string_and_sel {before; selected=(sds, direction); after} = 
        let str = before ^ sds ^ after in 
        let len_before = String.length before in 
        let len_sds = String.length sds in 
        let string_sel = begin 
          match direction with 
          | Right -> 
              StringSel.{
                startIdx=len_before; 
                endIdx=len_before + len_sds}
          | Left -> 
              StringSel.{
                startIdx=len_before + len_sds; 
                endIdx=len_before}
        end in 
        (str, string_sel)
      
      let of_string_and_sel (str, ssel) = 
        let before = StringSel.before str ssel in
        let ss = StringSel.sub str ssel in 
        let direction = StringSel.direction_of ssel in 
        let after = StringSel.after str ssel in 
        {before; selected=(ss, direction); after}
    end
  end

  module Action = struct
    open Sel

    type t = 
      MoveSelection of HSel.t
    | EnterString of string
    | NewPair

    (* raises HSel.Invalid if not 
     * HSel.valid_for (hexp, hsel) *)
    let rec valid_for action (hexp, hsel) = match action with 
    | MoveSelection hsel' -> HSel.valid_for (hexp, hsel')
    | EnterString _ ->  
        begin match hsel with 
        | HSel.OutPair _ -> false
        | HSel.PairSelected _ -> true
        | HSel.InHole _ -> true
        | HSel.InFst hsel' -> 
            begin match hexp with
            | HExp.Pair (fst, _) -> valid_for action (fst, hsel')
            | _ -> raise HSel.Invalid
            end
        | HSel.InSnd hsel' -> 
            begin match hexp with 
            | HExp.Pair (_, snd) -> valid_for action (snd, hsel')
            | _ -> raise HSel.Invalid 
            end
        end
    | NewPair -> true

    exception Invalid
  end

  module Models = struct
    open Sel

    (* BModel and ZModel are isomorphic, mediated by of_z and of_b *)
    module rec BModel : sig
      type u = HExp.t * HSel.t
      type t

      (* make u raises HSel.Invalid if 
       * not HSel.valid_for u *)
      val make : u -> t

      (* invariant: HSel.valid_for(show b) *) 
      val show : t -> u

      val of_z : ZModel.t -> t

      val hexp_of : t -> HExp.t
      val hsel_of : t -> HSel.t

      (* execute b action raises Action.Invalid if 
       * not Action.valid_for action b *)
      val execute : t -> Action.t -> t
    end = struct
      exception BadInvariant

      type u = HExp.t * HSel.t
      type t = u

      let make u = 
        if HSel.valid_for u then u else raise HSel.Invalid

      let show u = u

      let rec of_z z = 
        let open HExp in
        let open HSel in 
        let open ZString in 
        let open StringSel in 
        let open ZModel in 
        match z with 
        | ZOutPair (direction, fst, snd) -> (Pair(fst, snd), OutPair direction)
        | ZPairSelected (direction, fst, snd) -> (Pair(fst, snd), PairSelected direction)
        | ZInHole ss -> 
            let (str, ssel) = ZString.to_string_and_sel ss in 
            (Hole str, InHole ssel)
        | ZInFst (fst, snd) -> 
            let (hexp_fst, sel_fst) = of_z fst in 
            (Pair (hexp_fst, snd), InFst sel_fst)
        | ZInSnd (fst, snd) -> 
            let (hexp_snd, sel_snd) = of_z snd in 
            (Pair (fst, hexp_snd), InSnd sel_snd)

      let hexp_of (hexp, _) = hexp
      let hsel_of (_, sel) = sel

      let rec execute (hexp, sel) action = 
        let open HExp in 
        let open HSel in 
        let open StringSel in 
        match action with 
        | Action.MoveSelection sel' -> 
          if HSel.valid_for (hexp, sel') then 
            (hexp, sel') 
          else raise Action.Invalid
        | Action.EnterString str -> begin
            match (hexp, sel) with 
            | (_, OutPair _) -> raise Action.Invalid
            | (Pair _, PairSelected _) -> (Hole str, 
              let len = String.length str in 
              InHole {startIdx=len; endIdx=len})
            | (_, PairSelected _) -> raise BadInvariant
            | (Hole str', InHole ssel) -> 
                let spliced = StringSel.splice str' ssel str in 
                let sub_start = StringSel.sub_start ssel in 
                let sub_len = String.length str' in 
                let new_cursor = sub_start + sub_len in 
                (Hole spliced, 
                 InHole {startIdx=new_cursor; endIdx=new_cursor})
            | (_, InHole _) -> raise BadInvariant
            | (Pair (fst, snd), InFst sel') -> 
                let (fst', sel'') = execute (fst, sel') action in 
                (Pair (fst', snd), InFst sel'') 
            | (_, InFst _) -> raise BadInvariant
            | (Pair (fst, snd), InSnd sel') -> 
                let (snd', sel'') = execute (snd, sel') action in 
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
              let (fst', sel'') = execute (fst, sel') action in 
              (Pair (fst', snd), InFst sel'')
            | (_, InFst _) -> raise BadInvariant
            | (Pair (fst, snd), InSnd sel') ->
              let (snd', sel'') = execute (snd, sel') action in 
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

      (* of_u u raises HSel.Invalid if
        * not HSel.valid_for u *)
      val of_u : BModel.u -> t

      val of_b : BModel.t -> t

      val hexp_of : t -> HExp.t
      val hsel_of : t -> HSel.t

      val execute : t -> Action.t -> t
    end = struct
      type t = 
        ZOutPair of direction * HExp.t * HExp.t
      | ZPairSelected of direction * HExp.t * HExp.t
      | ZInHole of ZString.t
      | ZInFst of t * HExp.t
      | ZInSnd of HExp.t * t

      let rec of_u u = 
        let open HExp in
        let open HSel in 
        let open StringSel in 
        match u with 
        | (Pair (fst, snd), OutPair direction) -> ZOutPair (direction, fst, snd)
        | (_, OutPair _) -> raise HSel.Invalid
        | (Pair (fst, snd), PairSelected direction) -> ZPairSelected (direction, fst, snd)
        | (_, PairSelected _) -> raise HSel.Invalid
        | (Hole str, InHole ssel) -> 
            ZInHole (ZString.of_string_and_sel (str, ssel))
        | (_, InHole _) -> raise HSel.Invalid
        | (Pair (fst, snd), InFst sel') -> ZInFst ((of_u (fst, sel')), snd)
        | (_, InFst _) -> raise HSel.Invalid
        | (Pair (fst, snd), InSnd sel') -> ZInSnd (fst, (of_u (snd, sel')))
        | (_, InSnd _) -> raise HSel.Invalid

      let rec of_b b = of_u (BModel.show b)

      let rec hexp_of z = HExp.(match z with 
      | ZOutPair (_, fst, snd) -> Pair (fst, snd)
      | ZPairSelected (_, fst, snd) -> Pair (fst, snd)
      | ZInHole ss -> Hole (ZString.to_string ss)
      | ZInFst (fst, snd) -> Pair ((hexp_of fst), snd)
      | ZInSnd (fst, snd) -> Pair (fst, (hexp_of snd)))

      let rec hsel_of z = HSel.(match z with 
      | ZOutPair (direction, _, _) -> OutPair direction
      | ZPairSelected (direction, _, _) -> PairSelected direction
      | ZInHole ss -> 
          let (_, sel) = ZString.to_string_and_sel ss in 
          InHole sel
      | ZInFst (fst, snd) -> InFst (hsel_of fst)
      | ZInSnd (fst, snd) -> InSnd (hsel_of snd))

      let rec execute z action = 
        match action with 
        | Action.MoveSelection sel' -> of_b (BModel.execute (BModel.of_z z) action)
        | Action.EnterString str -> begin
            let open ZString in 
            match z with 
            | ZOutPair _ -> raise Action.Invalid
            | ZPairSelected _ -> ZInHole {
                before=str;
                selected=ZString.make_sd ("", Right);
                after=""}
            | ZInHole {before; selected; after} -> ZInHole {
                before=before ^ str; 
                selected=ZString.make_sd ("", Right); 
                after=after}
            | ZInFst (fst, snd) -> ZInFst ((execute fst action), snd)
            | ZInSnd (fst, snd) -> ZInSnd (fst, (execute snd action))
          end
        | Action.NewPair -> begin
            let open ZString in 
            match z with 
            | ZOutPair (Left, fst, snd) -> 
                ZInFst (
                  ZInHole {before=""; selected=ZString.make_sd ("", Right); after=""},
                  HExp.Pair (fst, snd))
            | ZOutPair (Right, fst, snd) -> 
                ZInSnd (
                  HExp.Pair (fst, snd),
                  ZInHole {before=""; selected=ZString.make_sd ("", Right); after=""})
            | ZPairSelected (Left, fst, snd) -> 
                ZPairSelected(Left, HExp.Hole "", HExp.Pair(fst, snd))
            | ZPairSelected (Right, fst, snd) ->
                ZPairSelected(Right, HExp.Pair(fst, snd), HExp.Hole "")
            | ZInHole _ -> ZInFst (z, HExp.Hole "")
            | ZInFst (fst, snd) -> 
                let fst' = execute fst action in 
                ZInFst (fst', snd)
            | ZInSnd (fst, snd) -> 
                let snd' = execute snd action in 
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
      val hsel_of : t -> HSel.t

      val execute : t -> Action.t -> t
    end

    module AbsBModel : ABSMODEL = struct
      type t = BModel.t

      let of_b b = b
      let of_z = BModel.of_z
      let to_b b = b
      let to_z = ZModel.of_b

      let hexp_of = BModel.hexp_of
      let hsel_of = BModel.hsel_of

      let execute = BModel.execute
    end

    module AbsZModel : ABSMODEL = struct
      type t = ZModel.t

      let of_b = ZModel.of_b
      let of_z z = z
      let to_b = BModel.of_z
      let to_z z = z

      let hexp_of = ZModel.hexp_of
      let hsel_of = ZModel.hsel_of

      let execute = ZModel.execute
    end
  end

  module StringView(Model : Models.ABSMODEL) = struct
    open Models
    open Sel
    open ZString

    let viewStringSel (stringZSel : Sel.ZString.t) : string = 
      "'" ^  stringZSel.before
      ^ (match (show_sd stringZSel.selected) with
         | (str,dir)-> (match dir with 
                        | Left -> "{" ^ str ^ "|"
                        | Right -> "|" ^ str ^ "}")
        )
      ^ stringZSel.after ^ "'"

    let rec viewHExp (hexp : HExp.t) : string =
       match hexp with 
        | HExp.Pair (fst,snd) ->  "(" ^ (viewHExp fst) ^ "," ^ (viewHExp snd) ^ ")" 
        | HExp.Hole str -> "'" ^ str ^ "'" 

    let rec viewZ (modelZ : ZModel.t) : string = 
      match modelZ with
        | ZModel.ZOutPair (dir,fst,snd) -> 
            (match dir with
            | Left -> "|(" ^ (viewHExp fst) ^ "," ^(viewHExp snd) ^ ")"
            | Right -> "(" ^ (viewHExp fst) ^ "," ^(viewHExp snd) ^ ")|" )
        | ZModel.ZPairSelected (dir,fst,snd) -> 
            (match dir with
            | Left -> "|(" ^ (viewHExp fst) ^ "," ^(viewHExp snd) ^ ")}"
            | Right -> "{(" ^ (viewHExp fst) ^ "," ^(viewHExp snd) ^ ")|" )
        | ZModel.ZInHole str -> viewStringSel str
        | ZModel.ZInFst (fst,snd) -> "(" ^ (viewZ fst) ^ "," ^ (viewHExp snd) ^ ")"
        | ZModel.ZInSnd (fst,snd) -> "(" ^ (viewHExp fst) ^ "," ^ (viewZ snd) ^ ")"
    
    let rec view (model : Model.t) : string =
        viewZ (Model.to_z model)
  end

  module ReactiveStringView(Model : Models.ABSMODEL) = struct 
    (* make an action stream *)
    (* make a model stream that reacts to action stream *)
    (* make a view stream that reacts to model stream *)
  end
end 

