(* run `ocaml index.ml examples.ml` at shell *)

open NestedPairs.Model
open NestedPairs.Action

(* 
 * EXAMPLE 1 
 *)
let base_hole = SelHole {beforeSel="", inSel="", afterSel=""}

(* write hello *)
let write_action = ReplaceSelection "Hello, world!"
let exp_1 = apply base_hole type_action
assert(exp_1 = SelHole {beforeSel="Hello, world!", inSel="", afterSel=""})

(* nest a pair *)
let nest_action = NewPair
let exp_2 = apply exp_1 nest_action
assert (exp2 = InLeftOfPair(exp1, Hole ""))

(* nest another pair *)
let exp_3 = apply exp_2 nest_action
assert (exp3 = InLeftOfPair(exp2, Hole ""))

(* TODO: more examples *)
