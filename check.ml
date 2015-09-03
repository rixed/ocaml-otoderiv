open Otoderiv
open Algen_impl
open OUnit2

module I = IntField (Dim1)
module T1 = TupleArray (I) (struct let dim = 1 end)
module A1 = AutoDeriv (I) (T1)
module T2 = TupleArray (I) (struct let dim = 2 end)
module A2 = AutoDeriv (I) (T2)

let _ =
  (* test function: x -> 3*x^2 *)
  let fun1 x = A1.(mul (of_int 3) (square x)) in
  let v = A1.apply fun1 I.zero in
  assert_equal (T1.value v) I.zero ;
  assert_equal (T1.deriv 0 v) I.zero ;
  let v = A1.apply fun1 I.one in
  Format.printf "%a\n" A1.print v ;
  assert_equal (I.of_int 3) (T1.value v) ;
  assert_equal (I.of_int 6) (T1.deriv 0 v) ;
  let v = A1.apply fun1 (I.of_int 3) in
  Format.printf "%a\n" A1.print v ;
  assert_equal (I.of_int 27) (T1.value v) ;
  assert_equal (I.of_int 18) (T1.deriv 0 v) ;
  (* test function: x,y -> 3x+2y *)
  let fun2 x y = A2.(add (mul (of_int 3) x)
                         (mul (of_int 2) y)) in
  (* Evaluate this at (1,2): *)
  let v = A2.apply2 fun2 (I.of_int 1) (I.of_int 2) in
  assert_equal (I.of_int 7) (T2.value v) ;
  assert_equal (I.of_int 3) (T2.deriv 0 v) ;
  assert_equal (I.of_int 2) (T2.deriv 1 v)
