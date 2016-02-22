open OUnit2;;
open Index.NestedPairs
(* Install OUnit via opam first *)

open HExp

let emptyHole = Hole "";;
let testHole = Hole "test";;

let test1 test_ctxt = assert_equal 1 1

let test2 test_ctxt = assert_equal emptyHole emptyHole

let test3 test_ctxt = assert_equal testHole testHole

let emptyPair = Pair (emptyHole,emptyHole) 

let test4 test_ctxt = assert_equal emptyPair (Pair ((Hole ""),(Hole "")))

let testPair = Pair (testHole,testHole)

let test5 test_ctxt = assert_equal testPair (Pair ((Hole "test"),(Hole "test")))

let nestedPair = Pair (Pair (testHole,testHole),emptyHole)

let test6 test_ctxt = assert_equal nestedPair (Pair (Pair ((Hole "test"),(Hole "test")),(Hole "")))

let nestedNestedPair = Pair (nestedPair,testHole)

let test7 test_ctxt = assert_equal nestedNestedPair  (Pair ((Pair (Pair ((Hole "test"),(Hole "test")),(Hole ""))),(Hole "test")))

let nestedNestedPair2 = Pair (nestedPair,nestedPair)

let test8 test_ctxt = assert_equal nestedNestedPair2  (Pair ((Pair (Pair ((Hole "test"),(Hole "test")),(Hole ""))),(Pair (Pair ((Hole "test"),(Hole "test")),(Hole "")))))

open Sel
open StringSel
 
let simpleStringSel = {startIdx = 0; endIdx = 1}

let testSel1 test_ctxt = assert_equal simpleStringSel {startIdx = 0; endIdx = 1}

let testSel2 test_ctxt = assert_equal (direction_of simpleStringSel) Right

open Models
(* Create BModel *)
let hexpSel = HSel.(InFst (OutPair Left))
let bmodel1 = BModel.make (nestedPair,hexpSel)  (* (Pair (testHole,testHole),emptyHole) *)
(* Sample output  (|("test","test"),"") *)
let testABSBMODEL test_ctxt = assert_equal (BModel.show bmodel1) (nestedPair,hexpSel)

module BModelStringView = StringView (AbsBModel)

let testViewHExpView1 test_ctxt = assert_equal (BModelStringView.viewHExp testHole) "'test'" 

let testViewHExpView2 test_ctxt = assert_equal (BModelStringView.viewHExp emptyPair) "('','')" 

(* (Pair ((Pair (Pair ((Hole "test"),(Hole "test")),(Hole ""))),(Hole "test"))) *)
let testViewHExpView3 test_ctxt = assert_equal (BModelStringView.viewHExp nestedNestedPair) "((('test','test'),''),'test')" 

let testViewHExpView test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "(|('test','test'),'')" (BModelStringView.view (AbsBModel.of_b bmodel1)) 
(* finish ViewString *)

(* TEST valid_of *)

(* Test actions *)

(* Test string selection methods *)

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "test3">:: test3;
  "test4">:: test4;
  "test5">:: test5;
  "test6">:: test6;
  "test7">:: test7;
  "test8">:: test8;
  "testSel1">:: testSel1;
  "testSel2">:: testSel2;
  "testABSBMODEL">:: testABSBMODEL;
  "testViewHExpView1">:: testViewHExpView1;
  "testViewHExpView2">:: testViewHExpView2;
  "testViewHExpView3">:: testViewHExpView3;
   "testABSMODELView1">:: testViewHExpView;
  (* "testStringView">:: testStringView *)
  ]
;;

let () =
  run_test_tt_main suite
;;
