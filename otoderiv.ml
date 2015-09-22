open Algen_intf

module type TUPLE =
sig
  type t
  type v
  module K : FIELD with type t = v
  module V : VECTOR with module K = K

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

module TupleArray (K : FIELD) (Dim : CONF_INT) :
  TUPLE with type v = K.t =
struct
  type v = K.t
  type t = v array (* 0 being the value and the rest for the derivatives *)
  module K = K
  module V = Algen_vector.Make (K) (Dim)

  let deriv1 v n =
    Array.init (succ Dim.v) (fun i ->
      if i = 0 then v else if i = succ n then K.one else K.zero)
  let deriv0 v = deriv1 v ~-1

  let value t = t.(0)
  let deriv n t = t.(n+1)

  let combine t1 t2 fv fd =
    Array.init (succ Dim.v) (fun i -> (if i = 0 then fv else fd) t1.(i) t2.(i))

  let apply t fv fd =
    Array.init (succ Dim.v) (fun i -> (if i = 0 then fv else fd) t.(i))

  let print fmt a =
    Array.iteri (fun i v ->
      if i > 0 then Format.pp_print_space fmt () ;
      K.print fmt v) a
end

module AutoDeriv_Group (K : CORE_GROUP) (T : TUPLE with type v = K.t) :
  CORE_GROUP with type t = T.t =
struct
  type t = T.t

  let zero = T.deriv0 K.zero

  let add a b = T.combine a b K.add K.add

  let neg a = T.apply a K.neg K.neg

  let compare a b = K.compare (T.value a) (T.value b)

  let print fmt a =
    Format.fprintf fmt "%a" T.print a
end

module AutoDeriv_Field (K : CORE_FIELD) (T : TUPLE with type v = K.t) :
  CORE_FIELD with type t = T.t =
struct
  module F = Field(K)
  include Ring(struct
    include Group(AutoDeriv_Group(K)(T))

    let one = T.deriv0 K.one

    let mul a b =
      T.combine a b K.mul
        (fun a' b' -> K.add (K.mul (T.value a) b')
                            (K.mul a' (T.value b)))
  end)

  let inv a =
    T.apply a K.inv
      (fun a' -> F.div a' (K.neg (K.inv (K.square (T.value a)))))

  let sqrt a =
    T.apply a K.sqrt (fun a' ->
      F.div a' (K.double (K.sqrt (T.value a))))

  let half a = T.apply a K.half K.half

  let identity x = x
  let floor a = T.apply a K.floor identity
  let ceil a = T.apply a K.ceil identity
  let rand a = T.apply a K.rand K.rand

  let of_int n = T.deriv0 (K.of_int n)
  let to_int a = K.to_int (T.value a)
  let of_float f = T.deriv0 (K.of_float f)
  let to_float a = K.to_float (T.value a)
  let of_nativeint n = T.deriv0 (K.of_nativeint n)
  let to_nativeint a = K.to_nativeint (T.value a)
  let of_int64 n = T.deriv0 (K.of_int64 n)
  let to_int64 a = K.to_int64 (T.value a)
  let of_string n = T.deriv0 (K.of_string n)
  let to_string a = K.to_string (T.value a)
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

module AutoDeriv (K : CORE_FIELD) (T : TUPLE with type v = K.t) = struct
  include Field(AutoDeriv_Field(K)(T))

  (* FIXME: could take the order of derivation we want as a parameter:
   * 0 for f(x), 1 for f'(x), 2 for f''(x), and so on. We would then compute
   * lazily that derivative (with memoization). *)
  let apply1 f (x : K.t) = f (T.deriv1 x 0)

  let apply2 f (x : K.t) (y : K.t) =
    f (T.deriv1 x 0) (T.deriv1 y 1)

  let apply f (xs : K.t array) =
    let nb_parms = Array.length xs in
    f (Array.init nb_parms (fun i -> T.deriv1 xs.(i) i))

  (* Minimization function: *)
  let rec minimize eps f (x : T.V.t) =
    let v = apply f x in
    let search_dir = Array.init T.V.Dim.v (fun i -> K.neg (T.deriv i v)) in
    let search_dir_len = T.V.norm search_dir in
    let rec next_x c =
      if K.mul c search_dir_len < eps then x else
      let x' = T.V.add x (T.V.mul c search_dir) in 
      let v' = apply f x' in
      if T.value v' < K.sub (T.value v) (K.mul c search_dir_len) then x'
      else next_x (K.half c) in
    let x' = next_x (K.half K.one) in
    if x' == x then x else (* physical identity here *)
    minimize eps f x'
end
