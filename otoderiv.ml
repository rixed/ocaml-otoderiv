open Algen_intf

module type CONFIG = sig
  val dim : int
end

module AutoDeriv_Group (B : CORE_GROUP) (Conf : CONFIG) :
  CORE_GROUP with type t = B.t * B.t array =
struct
  type t = B.t * B.t array

  let zero = B.zero, Array.make Conf.dim B.zero

  let add (a, a') (b, b') =
    B.add a b,
    Array.init Conf.dim (fun i -> B.add a'.(i) b'.(i))

  let neg (a, a') =
    B.neg a,
    Array.map B.neg a'

  let compare (a, _) (b, _) = B.compare a b

  let array_print fmt a =
    Array.iteri (fun i v ->
      if i > 0 then Format.pp_print_space fmt () ;
      B.print fmt v) a

  let print fmt (a, a') =
    Format.fprintf fmt "(%a, %a)" B.print a array_print a'
end

module AutoDeriv_Ring (B : CORE_RING) (Conf : CONFIG) :
  CORE_RING with type t = B.t * B.t array =
struct
  include Group(AutoDeriv_Group(B)(Conf))

  let one = B.one, Array.make Conf.dim B.zero

  let mul (a, a') (b, b') =
    B.mul a b,
    Array.init Conf.dim (fun i -> B.add (B.mul a b'.(i)) (B.mul a'.(i) b))
end

module AutoDeriv_Field (B : CORE_FIELD) (Conf : CONFIG) :
  CORE_FIELD with type t = B.t * B.t array =
struct
  module F = Field(B)
  include Ring(AutoDeriv_Ring(B)(Conf))

  let inv (a, a') =
    B.inv a,
    Array.init Conf.dim (fun i -> F.div a'.(i) (B.neg (B.inv (B.square a))))

  let sqrt (a, a') =
    B.sqrt a,
    Array.init Conf.dim (fun i -> F.div a'.(i) (B.double (B.sqrt a)))

  let half (a, a') = B.half a, Array.init Conf.dim (fun i -> B.half a'.(i))

  let floor (a, a') = B.floor a, a'
  let ceil (a, a') = B.ceil a, a'
  let rand (a, a') = B.rand a, Array.init Conf.dim (fun i -> B.rand a'.(i))

  let of_int n = B.of_int n, Array.make Conf.dim B.zero
  let to_int (a, _) = B.to_int a
  let of_float f = B.of_float f, Array.make Conf.dim B.zero
  let to_float (a, _) = B.to_float a
  let of_nativeint n = B.of_nativeint n, Array.make Conf.dim B.zero
  let to_nativeint (a, _) = B.to_nativeint a
  let of_int64 n = B.of_int64 n, Array.make Conf.dim B.zero
  let to_int64 (a, _) = B.to_int64 a
  let of_string n = B.of_string n, Array.make Conf.dim B.zero
  let to_string (a, _) = B.to_string a
end

module AutoDeriv_Trigo (B : TRIGO) (Conf : CONFIG) = struct
  type t = B.t * B.t array
  let pi = B.pi, Array.make Conf.dim B.zero
  let sin (a, a') = B.sin a, Array.init Conf.dim (fun _ -> B.mul a' (B.cos a))
  let cos (a, a') = B.cos a, Array.init Conf.dim (fun _ -> B.mul a' (B.neg (B.sin a)))
  let tan (a, a') = B.tan a, Array.init Conf.dim (fun _ -> B.mul a' (B.sub (B.square (B.tan a)) B.one))
end

module AutoDeriv (B : CORE_FIELD) (Conf : CONFIG) = struct
  include Field(AutoDeriv_Field(B)(Conf))

  (* FIXME: could take the order of derivation we want as a parameter:
   * 0 for f(x), 1 for f'(x), 2 for f''(x), and so on. We would then compute
   * lazily that derivative (with memoization). *)
  let apply f (x : B.t) =
    f (x, Array.init Conf.dim (fun i -> if i == 0 then B.one else B.zero))

  let apply2 f (x : B.t) (y : B.t) =
    f (x, [|B.one; B.zero|])
      (y, [|B.zero; B.one|])
end
