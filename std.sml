infix |>

infixr $

structure Std : sig
  exception Unreachable

  val |> : 'a * ('a -> 'b) -> 'b
  val $ : ('a -> 'b) * 'a -> 'b

  val id : 'a -> 'a
  val const : 'a -> 'b -> 'a
  val fst : 'a * 'b -> 'a

  val print_endline : string -> unit

  val zip : 'a list -> 'b list -> ('a * 'b) list
  val unzip : ('a * 'b) list -> 'a list * 'b list
end = struct
  exception Unreachable

  fun x |> f = f x

  fun f $ x = f x

  fun id x = x

  fun const x _ = x

  fun fst (x, _) = x

  fun print_endline s = s ^ "\n" |> print

  fun zip [] _ = []
    | zip _ [] = []
    | zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

  fun unzip []             = ([], [])
    | unzip ((x, y) :: zs) =
        case unzip zs of
             (xs, ys) => (x :: xs, y :: ys)
end

open Std

structure Option : sig
  include Option

  type 'a t = 'a option

  val map_or : ('a -> 'b) -> 'b -> 'a t -> 'b
  val unwrap_or : 'a -> 'a t -> 'a
  val unwrap_or_else : (unit -> 'a) -> 'a t -> 'a
end = struct
  open Option

  type 'a t = 'a option

  fun map_or _ x NONE     = x
    | map_or f _ (SOME x) = f x

  fun unwrap_or x NONE     = x
    | unwrap_or _ (SOME x) = x

  fun unwrap_or_else f NONE     = f ()
    | unwrap_or_else _ (SOME x) = x
end
