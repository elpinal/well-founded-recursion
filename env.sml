structure Env :> sig
  type t

  val empty : t

  structure Name : sig
    exception UnboundName of int

    val member : int -> t -> bool
    val member_exn : int -> t -> unit
    val insert : t -> t
  end

  structure Var : sig
    exception UnboundVariable of int

    local open Syntax in
      val lookup : int -> t -> Type.t
      val insert : Type.t -> t -> t
    end
  end
end = struct
  open Syntax

  type t = int * Type.t list

  val empty = (0, [])

  structure Name = struct
    exception UnboundName of int

    fun member n env = n < #1 env

    fun member_exn n env =
      if member n env
      then ()
      else raise UnboundName(n)

    fun insert (n, xs) = (n + 1, List.map (Type.shift 1) xs)
  end

  structure Var = struct
    exception UnboundVariable of int

    fun lookup n env = List.nth (#2 env, n) handle Subscript => raise UnboundVariable(n)

    fun insert ty (n, xs) = (n, ty :: xs)
  end
end
