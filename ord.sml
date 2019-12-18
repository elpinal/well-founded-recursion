signature ORD = sig
  type t
  val compare : t * t -> order
end

functor Compare (X : ORD) : sig
  include ORD where type t = X.t

  val op< : t * t -> bool
end = struct
  open X

  fun x < y =
    case compare (x, y) of
         LESS => true
       | _    => false
end

