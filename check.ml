open Otoderiv
open Algen_impl
open OUnit2

module I = IntField (Dim1)
module T1 = TupleArray (I) (Dim1)
module A1 = AutoDeriv (I) (T1)
module T2 = TupleArray (I) (Dim2)
module A2 = AutoDeriv (I) (T2)

let test_deriv1 _ =
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

let test_deriv2 _ =
  (* test function: x,y -> 3x+2y *)
  let f x y = A2.(add (mul (of_int 3) x)
                      (mul (of_int 2) y)) in
  (* Evaluate this at (1,2): *)
  let v = A2.apply2 f (I.of_int 1) (I.of_int 2) in
  assert_equal ~msg:"f(1, 2)" ~printer:I.to_string (I.of_int 7) (T2.value v) ;
  assert_equal ~msg:"df/dx(1, 2)" ~printer:I.to_string (I.of_int 3) (T2.deriv 0 v) ;
  assert_equal ~msg:"df/dy(1, 2)" ~printer:I.to_string (I.of_int 2) (T2.deriv 1 v)

module F = FloatField
module T2F = TupleArray (F) (Dim2)
module A2F = AutoDeriv (F) (T2F)

let test_min1 _ =
  (* test function: x,y -> x**2 + (y-1)**2 *)
  let f xs =
    A2F.(add (square xs.(0))
             (square (sub xs.(1) one))) in
  let xs0 = [| 5. ; 5. |]
  and e = [| 0. ; 1. |] (* expected result *)
  and eps = 0.1 in
  let m = A2F.minimize eps f xs0 in
  assert_bool "do not converge"
    (T2F.V.(norm (sub m e)) <= eps)

let deriv_test =
  "automatic derivation" >:::
    [ "1 param function" >:: test_deriv1 ;
      "2 params function" >:: test_deriv2 ;
      "minimization" >:: test_min1 ]

let () =
  run_test_tt_main deriv_test
