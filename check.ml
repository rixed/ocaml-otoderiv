open Otoderiv
open Algen_impl
open OUnit2

module I = IntField (Dim1)
module A = AutoDeriv (I)(struct let dim = 1 end)
module A2 = AutoDeriv (I)(struct let dim = 2 end)

let _ =
  (* test function: x -> 3*x^2 *)
  let fun1 x = A.(mul (of_int 3) (square x)) in
  assert_equal (I.zero, [|I.zero|]) (A.apply fun1 I.zero) ;
  Format.printf "%a\n" A.print (A.apply fun1 I.one) ;
  assert_equal (I.of_int 3, [|I.of_int 6|])  (A.apply fun1 I.one) ;
  Format.printf "%a\n" A.print (A.apply fun1 (I.of_int 3)) ;
  assert_equal (I.of_int 27, [|I.of_int 18|]) (A.apply fun1 (I.of_int 3)) ;
  (* test function: x,y -> 3x+2y *)
  let fun2 x y = A2.(add (mul (of_int 3) x) (mul (of_int 2) y)) in
  (* Evaluate this at (1,2): *)
  assert_equal (I.of_int 7, [|I.of_int 3; I.of_int 2|])
               (A2.apply2 fun2 (I.of_int 1) (I.of_int 2))
