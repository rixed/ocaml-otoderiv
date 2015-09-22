open Otoderiv
open Algen_impl
open OUnit2

module I = IntField (Dim1)
module T1 = TupleArray (I) (struct let v = 1 end)
module A1 = AutoDeriv (I) (T1)
module T2 = TupleArray (I) (struct let v = 2 end)
module A2 = AutoDeriv (I) (T2)

let test1 _ =
  (* test function: x -> 3*x^2 *)
  let f x = A1.(mul (of_int 3) (square x)) in
  let v = A1.apply1 f I.zero in
  assert_equal ~msg:"f(0)" ~printer:I.to_string (T1.value v) I.zero ;
  assert_equal ~msg:"df/dx(0)" ~printer:I.to_string (T1.deriv 0 v) I.zero ;
  let v = A1.apply1 f I.one in
  assert_equal ~msg:"f(1)" ~printer:I.to_string (I.of_int 3) (T1.value v) ;
  assert_equal ~msg:"df/dx(1)" ~printer:I.to_string (I.of_int 6) (T1.deriv 0 v) ;
  let v = A1.apply1 f (I.of_int 3) in
  assert_equal ~msg:"f(3)" ~printer:I.to_string (I.of_int 27) (T1.value v) ;
  assert_equal ~msg:"df/dx(3)" ~printer:I.to_string (I.of_int 18) (T1.deriv 0 v)

let test2 _ =
  (* test function: x,y -> 3x+2y *)
  let f x y = A2.(add (mul (of_int 3) x)
                         (mul (of_int 2) y)) in
  (* Evaluate this at (1,2): *)
  let v = A2.apply2 f (I.of_int 1) (I.of_int 2) in
  assert_equal ~msg:"f(1, 2)" ~printer:I.to_string (I.of_int 7) (T2.value v) ;
  assert_equal ~msg:"df/dx(1, 2)" ~printer:I.to_string (I.of_int 3) (T2.deriv 0 v) ;
  assert_equal ~msg:"df/dy(1, 2)" ~printer:I.to_string (I.of_int 2) (T2.deriv 1 v)

let deriv_test =
  "derivation computation" >:::
    [ "1 param function" >:: test1 ;
      "2 params function" >:: test2 ]

let () =
  run_test_tt_main deriv_test
