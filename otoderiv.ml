open Algen_intf

module AutoDeriv_Group (B : CORE_GROUP) :
  CORE_GROUP with type t = B.t * B.t =
struct
  type t = B.t * B.t

  let zero = B.zero, B.zero

  let add (a, a') (b, b') =
    B.add a b, B.add a' b'

  let neg (a, a') = B.neg a, B.neg a'

  let compare (a, _) (b, _) = B.compare a b

  let print fmt (a, a') =
    Format.fprintf fmt "(%a, %a)" B.print a B.print a'
end

module AutoDeriv_Ring (B : CORE_RING) :
  CORE_RING with type t = B.t * B.t =
struct
  include Group(AutoDeriv_Group(B))

  let one = B.one, B.zero

  let mul (a, a') (b, b') =
    B.mul a b, B.add (B.mul a b') (B.mul a' b)
end

module AutoDeriv_Field (B : CORE_FIELD) :
  CORE_FIELD with type t = B.t * B.t =
struct
  module F = Field(B)
  include Ring(AutoDeriv_Ring(B))

  let inv (a, a') =
    B.inv a, F.div a' (B.neg (B.inv (B.square a)))

  let sqrt (a, a') =
    B.sqrt a, F.div a' (B.double (B.sqrt a))

  let half (a, a') = B.half a, B.half a'

  let floor (a, a') = B.floor a, a'
  let ceil (a, a') = B.ceil a, a'
  let rand (a, a') = B.rand a, B.rand a'

  let of_int n = B.of_int n, B.zero
  let to_int (a, _) = B.to_int a
  let of_float f = B.of_float f, B.zero
  let to_float (a, _) = B.to_float a
  let of_nativeint n = B.of_nativeint n, B.zero
  let to_nativeint (a, _) = B.to_nativeint a
  let of_int64 n = B.of_int64 n, B.zero
  let to_int64 (a, _) = B.to_int64 a
  let of_string n = B.of_string n, B.zero
  let to_string (a, _) = B.to_string a
end

module AutoDeriv_Trigo (B : TRIGO) = struct
  type t = B.t * B.t
  let pi = B.pi, B.zero
  let sin (a, a') = B.sin a, B.mul a' (B.cos a)
  let cos (a, a') = B.cos a, B.mul a' (B.neg (B.sin a))
  let tan (a, a') = B.tan a, B.mul a' (B.sub (B.square (B.tan a)) B.one)
end

module AutoDeriv (B : CORE_FIELD) = struct
  include Field(AutoDeriv_Field(B))

  (* FIXME: could take the order of derivation we want as a parameter:
   * 0 for f(x), 1 for f'(x), 2 for f''(x), and so on. We would then compute
   * lazily that derivative (with memoization). *)
  let apply f (x : B.t) = f (x, B.one)
end
