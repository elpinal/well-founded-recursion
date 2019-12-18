signature SET = sig
  type t

  type elem

  val empty : t
  val singleton : elem -> t
  val insert : elem -> t -> t
  val delete : elem -> t -> t
  val union : t -> t -> t
  val member : elem -> t -> bool
  val is_empty : t -> bool
  val map_monotonic : (elem -> elem) -> t -> t
  val app : (elem -> unit) -> t -> unit
  val is_subset_of : t -> t -> bool
  val equal : t -> t -> bool
  val from_list : elem list -> t
end

(*
* 'Set' is implemented as a generative functor because Moscow ML's applicative
* functors are not "abstraction-safe".
*)
functor Set (X : ORD) :> SET where type elem = X.t = struct
  type elem = X.t

  structure C = Compare(X)
  open C

  datatype t
    = E
    | T of t * elem * t

  fun is_empty E     = true
    | is_empty (T _) = false

  val empty = E

  fun singleton k = T(E, k, E)

  fun insert k t =
  let
    fun f E             = T(E, k, E)
      | f (T(l, k0, r)) =
      case X.compare (k, k0) of
           LESS    => T(f l, k0, r)
         | GREATER => T(l, k0, f r)
         | EQUAL   => T(l, k, r)
  in
    f t
  end

  fun union m E            = m
    | union m (T(l, k, r)) = union (union (insert k m) l) r

  fun delete k t =
  let
    fun f E             = E
      | f (T(l, k0, r)) =
      case X.compare (k, k0) of
           LESS    => T(f l, k0, r)
         | GREATER => T(l, k0, f r)
         | EQUAL   => union l r
  in
    f t
  end

  fun member j E            = false
    | member j (T(l, k, r)) =
    case compare (j, k) of
         LESS    => member j l
       | GREATER => member j r
       | EQUAL   => true

  fun map_monotonic _ E            = E
    | map_monotonic f (T(l, k, r)) = T(map_monotonic f l, f k, map_monotonic f r)

  fun app _ E            = ()
    | app f (T(l, k, r)) =
    let
      val () = app f l
      val () = f k
      val () = app f r
    in
      ()
    end

  fun is_subset_of E _             = true
    | is_subset_of _ E             = false
    | is_subset_of (T(l, k, r)) s2 = member k s2 andalso is_subset_of l s2 andalso is_subset_of r s2

  fun equal E E   = true
    | equal s1 s2 = is_subset_of s1 s2 andalso is_subset_of s2 s1

  fun from_list [] = E
    | from_list (x :: xs) = from_list xs |> insert x
end

structure IntSet = Set struct
  type t = int

  val compare = Int.compare
end

structure StringSet = Set struct
  type t = string

  val compare = String.compare
end
