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
let bmodel1 = BModel.make (nestedPair,hexpSel)  (* (Pair (testHole,testHole),emptyHole)    (|('test','test'),'') *)
(* Sample output  (|("test","test"),"") *)
let testABSBMODEL test_ctxt = assert_equal (BModel.show bmodel1) (nestedPair,hexpSel)

module BModelStringView = StringView (AbsBModel)

let testViewHExpView1 test_ctxt = assert_equal (BModelStringView.viewHExp testHole) "'test'" 

let testViewHExpView2 test_ctxt = assert_equal (BModelStringView.viewHExp emptyPair) "('','')" 

(* (Pair ((Pair (Pair ((Hole "test"),(Hole "test")),(Hole ""))),(Hole "test"))) *)
let testViewHExpView3 test_ctxt = assert_equal (BModelStringView.viewHExp nestedNestedPair) "((('test','test'),''),'test')" 

let testABSMODELView1 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "(|('test','test'),'')" (BModelStringView.view (AbsBModel.of_b bmodel1)) 

let hexpSel = HSel.(InSnd (OutPair Right))
(*   ((('test','test'),''),('test,'test'),'') *)
let bmodel2 = BModel.make (nestedNestedPair2,hexpSel)

let testABSMODELView2 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "((('test','test'),''),(('test','test'),'')|)" (BModelStringView.view (AbsBModel.of_b bmodel2)) 

let hexpSel3 = HSel.(InFst (InFst (OutPair Left)))
(*   ((('test','test'),''),('test,'test'),'') *)
let bmodel3 = BModel.make (nestedNestedPair2,hexpSel3)

let testABSMODELView3 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "((|('test','test'),''),(('test','test'),''))" (BModelStringView.view (AbsBModel.of_b bmodel3)) 



let hexpSel4 = HSel.(PairSelected Left)
(*   ((('test','test'),''),('test,'test'),'') *)
let bmodel4 = BModel.make (testPair,hexpSel4)

let testABSMODELView4 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "|('test','test')}" (BModelStringView.view (AbsBModel.of_b bmodel4)) 


let hexpSel5 = HSel.(InFst (InHole {startIdx=1; endIdx=2}))
(*   ('te|st,'test') *)
let bmodel5 = BModel.make (testPair,hexpSel5)

let testABSMODELView5 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "('t|e}st','test')" (BModelStringView.view (AbsBModel.of_b bmodel5)) 

let hexpSel6 = HSel.(InFst (InHole {startIdx=0; endIdx=4}))
(*   ('te|st,'test') *)
let bmodel6 = BModel.make (testPair,hexpSel6)

let testABSMODELView6 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "('|test}','test')" (BModelStringView.view (AbsBModel.of_b bmodel6)) 

let hexpSel7 = HSel.(InFst (InHole {startIdx=4; endIdx=0}))
(*   ('te|st,'test') *)
let bmodel7 = BModel.make (testPair,hexpSel7)

let testABSMODELView7 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "('{test|','test')" (BModelStringView.view (AbsBModel.of_b bmodel7)) 


(* finish ViewString DONE *)

let hexpS1 = HSel.(InFst (InHole {startIdx=1; endIdx=2}))
let testValidSel1 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS1)) 

let hexpS2 = HSel.(InHole {startIdx=(-1); endIdx=2})
let testValidSel2 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS2)) 

let hexpS3 = HSel.(InHole {startIdx=1; endIdx=6})
let testValidSel3 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS3)) 

let hexpS4 = HSel.(InHole {startIdx=1; endIdx=6})
let testValidSel4 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS4)) 

let hexpS5 = HSel.(OutPair Left)
let testValidSel5 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS5)) 

let hexpS6 = HSel.(PairSelected Left)
let testValidSel6 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS6)) 

let hexpS7 = HSel.(PairSelected Left)
let testValidSel7 test_ctxt = assert_equal true (HSel.valid_for (testPair , hexpS7)) 

let hexpS8 = HSel.(OutPair Left)
let testValidSel8 test_ctxt = assert_equal true (HSel.valid_for (testPair , hexpS8)) 

let hexpS9 = HSel.(InHole {startIdx=1; endIdx=3})
let testValidSel9 test_ctxt = assert_equal false (HSel.valid_for (testPair , hexpS9)) 

let hexpS10 = HSel.(InSnd (InFst (InHole {startIdx=1; endIdx=2})))
let testValidSel10 test_ctxt = assert_equal false (HSel.valid_for (nestedNestedPair2 , hexpS10)) 


(* Test actions *)
let hexpAction1 = HSel.(OutPair Left)
let bmodelAction1 = BModel.make (testPair,hexpAction1)
let testAction1 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)   "('|}',('test','test'))"  (BModelStringView.view (AbsBModel.execute (AbsBModel.of_b bmodelAction1) Action.NewPair))

let hexpAction2 = HSel.(OutPair Right)
let bmodelAction2 = BModel.make (testPair,hexpAction2)
let testAction2 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)   "(('test','test'),'|}')"  (BModelStringView.view (AbsBModel.execute (AbsBModel.of_b bmodelAction2) Action.NewPair))

let hexpAction3 = HSel.(InFst (InHole {startIdx=0; endIdx=4}))
let bmodelAction3 = BModel.make (testPair,hexpAction3)
let testAction3 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)   "('Ente|}rText','test')"  (BModelStringView.view (AbsBModel.execute (AbsBModel.of_b bmodelAction3) (Action.EnterString "EnterText")))


(* Test string selection methods *)

(* Name the test cases and group them together *)

(* add number to hexp *)
(* Add addition to evaluate expression *)

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
  "testABSMODELView1">:: testABSMODELView1;
  "testABSMODELView2">:: testABSMODELView2;
  "testABSMODELView3">:: testABSMODELView3;
  "testABSMODELView4">:: testABSMODELView4;
  "testABSMODELView5">:: testABSMODELView5;
  "testABSMODELView6">:: testABSMODELView6;
  "testABSMODELView7">:: testABSMODELView7;
  "testValidSel1">:: testValidSel1;
  "testValidSel2">:: testValidSel2;
  "testValidSel3">:: testValidSel3;
  "testValidSel4">:: testValidSel4;
  "testValidSel5">:: testValidSel5;
  "testValidSel6">:: testValidSel6;
  "testValidSel7">:: testValidSel7;
  "testValidSel8">:: testValidSel8;
  "testValidSel9">:: testValidSel9;
  "testValidSel10">:: testValidSel10;
  "testAction1">:: testAction1;
  "testAction2">:: testAction2;
  "testAction3">:: testAction3;
  (* "testStringView">:: testStringView *)
  ]
;;

let () =
  run_test_tt_main suite
;;
