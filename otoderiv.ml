open Algen_intf

module type TUPLE =
sig
  type t
  type v

  (* Return the tuple made of the given value and all derivatives zeroed *)
  val deriv0 : v -> t
  (* Same as above, but nth derivative is one *)
  val deriv1 : v -> int -> t

  (* Extract the value out of the tuple *)
  val value : t -> v

  (* Extract the derivative along the nth direction *)
  val deriv : int -> t -> v

  (* Combine the two tuple with the given functions, one combining the values
   * and the other the derivatives. *)
  val combine : t -> t -> (v -> v -> v) -> (v -> v -> v) -> t

  (* Apply the given functions to the tuple. *)
  val apply : t -> (v -> v) -> (v -> v) -> t

  val print : Format.formatter -> t -> unit
end

module type TUPLE_CONFIG = sig val dim : int end
module TupleArray (B : CORE_RING) (Conf : TUPLE_CONFIG) :
  TUPLE with type v = B.t =
struct
  type v = B.t
  type t = v array (* 0 being the value and the rest for the derivatives *)

  let deriv1 v n =
    Array.init (succ Conf.dim) (fun i ->
      if i = 0 then v else if i = succ n then B.one else B.zero)
  let deriv0 v = deriv1 v ~-1

  let value t = t.(0)
  let deriv n t = t.(n+1)

  let combine t1 t2 fv fd =
    Array.init (succ Conf.dim) (fun i -> (if i = 0 then fv else fd) t1.(i) t2.(i))

  let apply t fv fd =
    Array.init (succ Conf.dim) (fun i -> (if i = 0 then fv else fd) t.(i))

  let print fmt a =
    Array.iteri (fun i v ->
      if i > 0 then Format.pp_print_space fmt () ;
      B.print fmt v) a
end

module AutoDeriv_Group (B : CORE_GROUP) (T : TUPLE with type v = B.t) :
  CORE_GROUP with type t = T.t =
struct
  type t = T.t

  let zero = T.deriv0 B.zero

  let add a b = T.combine a b B.add B.add

  let neg a = T.apply a B.neg B.neg

  let compare a b = B.compare (T.value a) (T.value b)

  let print fmt a =
    Format.fprintf fmt "%a" T.print a
end

module AutoDeriv_Ring (B : CORE_RING) (T : TUPLE with type v = B.t) :
  CORE_RING with type t = T.t =
struct
  include Group(AutoDeriv_Group(B)(T))

  let one = T.deriv0 B.one

  let mul a b =
    T.combine a b B.mul
      (fun a' b' -> B.add (B.mul (T.value a) b')
                          (B.mul a' (T.value b)))
end

module AutoDeriv_Field (B : CORE_FIELD) (T : TUPLE with type v = B.t) :
  CORE_FIELD with type t = T.t =
struct
  module F = Field(B)
  include Ring(AutoDeriv_Ring(B)(T))

  let inv a =
    T.apply a B.inv
      (fun a' -> F.div a' (B.neg (B.inv (B.square (T.value a)))))

  let sqrt a =
    T.apply a B.sqrt (fun a' ->
      F.div a' (B.double (B.sqrt (T.value a))))

  let half a = T.apply a B.half B.half

  let identity x = x
  let floor a = T.apply a B.floor identity
  let ceil a = T.apply a B.ceil identity
  let rand a = T.apply a B.rand B.rand

  let of_int n = T.deriv0 (B.of_int n)
  let to_int a = B.to_int (T.value a)
  let of_float f = T.deriv0 (B.of_float f)
  let to_float a = B.to_float (T.value a)
  let of_nativeint n = T.deriv0 (B.of_nativeint n)
  let to_nativeint a = B.to_nativeint (T.value a)
  let of_int64 n = T.deriv0 (B.of_int64 n)
  let to_int64 a = B.to_int64 (T.value a)
  let of_string n = T.deriv0 (B.of_string n)
  let to_string a = B.to_string (T.value a)
end

module AutoDeriv_Trigo (B : TRIGO) (T : TUPLE with type v = B.t) = struct
  type t = T.t
  let pi = T.deriv0 B.pi
  let sin a =
    T.apply a B.sin (fun a' -> B.mul a' (B.cos (T.value a)))
  let cos a =
    T.apply a B.cos (fun a' -> B.mul a' (B.neg (B.sin (T.value a))))

  let tan a =
    T.apply a B.tan (fun a' -> B.mul a' (B.sub (B.square (B.tan (T.value a))) B.one))
end

module AutoDeriv (B : CORE_FIELD) (T : TUPLE with type v = B.t) = struct
  include Field(AutoDeriv_Field(B)(T))

  (* FIXME: could take the order of derivation we want as a parameter:
   * 0 for f(x), 1 for f'(x), 2 for f''(x), and so on. We would then compute
   * lazily that derivative (with memoization). *)
  let apply f (x : B.t) = f (T.deriv1 x 0)

  let apply2 f (x : B.t) (y : B.t) =
    f (T.deriv1 x 0) (T.deriv1 y 1)
end
