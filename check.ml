open Otoderiv
open Algen_impl
open OUnit2

module I = IntField (Dim1)
module A = AutoDeriv (I)

let _ =
  (* test function : x -> 3*x^2 *)
  let fun1 x = A.mul (A.of_int 3) (A.square x) in
  assert_equal (I.zero, I.zero) (A.apply fun1 I.zero) ;
  Format.printf "%a\n" A.print (A.apply fun1 I.one) ;
  assert_equal (I.of_int 3, I.of_int 6)  (A.apply fun1 I.one) ;
  Format.printf "%a\n" A.print (A.apply fun1 (I.of_int 3)) ;
  assert_equal (I.of_int 27, I.of_int 18) (A.apply fun1 (I.of_int 3))
