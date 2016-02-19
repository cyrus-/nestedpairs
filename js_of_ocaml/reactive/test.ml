open OUnit2;;
open Index.NestedPairs.HExp;;
(* Install OUnit via opam first *)


let emptyHole = Hole "";;
let testHole = Hole "test";;

let test1 test_ctxt = assert_equal 1 1;;

let test2 test_ctxt = assert_equal emptyHole emptyHole;;

let test3 test_ctxt = assert_equal testHole testHole;;

let emptyPair = Pair (emptyHole,emptyHole)

let test4 test_ctxt = assert_equal emptyPair (Pair ((Hole ""),(Hole "")));;

let testPair = Pair (testHole,testHole)

let test5 test_ctxt = assert_equal testPair (Pair ((Hole "test"),(Hole "test")));;

let nestedPair = Pair (Pair (testHole,testHole),emptyHole)

let test6 test_ctxt = assert_equal nestedPair (Pair (Pair ((Hole "test"),(Hole "test")),(Hole "")))

let nestedNestedPair = Pair (nestedPair,testHole)

let test7 test_ctxt = assert_equal nestedNestedPair  (Pair ((Pair (Pair ((Hole "test"),(Hole "test")),(Hole ""))),(Hole "test")))


(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "test3">:: test3;
  "test4">:: test4;
  "test5">:: test5;
  "test6">:: test6;
  "test7">:: test7]
;;

let () =
  run_test_tt_main suite
;;