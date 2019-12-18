signature Param = sig
  type var = int
  type name = int

  structure NameSet : SET where type elem = name
end

functor MakeSyntax (X : Param) : sig
  include Param

  type support

  structure Type : sig
    datatype t
      = Unit
      | Base of string
      | Prod of t * t
      | Arrow of t * support * t
      | Forall of t
      | Box of support * t
      | Comp of support * t
      | Ref of t
      | Cont of t

    val show : t -> string
    val shift : int -> t -> t
    val subst_top : NameSet.t -> t -> t
  end

  structure Term : sig
    datatype lit
      = Num of int

    datatype t
      = Var of var
      | Unit
      | Lit of lit
      | Prim of string
      | Pair of t * t
      | Fst of t
      | Snd of t
      | Abs of support * Type.t * t
      | App of t * t
      | NAbs of t
      | SApp of t * support
      | Box of support * t
      | Unbox of t
      | Rec of Type.t * t
      | Delay of support * t
      | Force of t
      | Ref of t
      | Get of t
      | Set of t * t
      | Callcc of Type.t * t
      | Throw of Type.t * t * t
      | Let of Type.t option * t * t
  end
end = struct
  open X

  type support = NameSet.t

  structure Type = struct
    datatype t
      = Unit
      | Base of string
      | Prod of t * t
      | Arrow of t * support * t
      | Forall of t
      | Box of support * t
      | Comp of support * t
      | Ref of t
      | Cont of t

    val show =
    let
      fun loop Unit       = "unit"
        | loop (Base s)   = s
        | loop (Prod _)   = "_ * _"
        | loop (Arrow _)  = "_ ->{...} _"
        | loop (Forall _) = "∀_. _"
        | loop (Box _)    = "box{...} _"
        | loop (Comp _)   = "comp{...} _"
        | loop (Ref _)    = "ref _"
        | loop (Cont _)   = "cont _"
    in
      loop
    end

    fun map f c =
    let
      fun loop c Unit             = Unit
        | loop c (Base s)         = Base s
        | loop c (Prod(x, y))     = Prod(loop c x, loop c y)
        | loop c (Arrow(x, s, y)) = Arrow(loop c x, f c s, loop c y)
        | loop c (Forall(x))      = Forall(loop (c + 1) x)
        | loop c (Box(s, x))      = Box(f c s, loop c x)
        | loop c (Comp(s, x))     = Comp(f c s, loop c x)
        | loop c (Ref x)          = Ref $ loop c x
        | loop c (Cont x)         = Cont $ loop c x
    in
      loop c
    end

    exception NegativeName

    fun shift_above c d =
    let
      fun f c s =
        NameSet.map_monotonic (fn name =>
        if c <= name
        then (if 0 <= name + d then name + d else raise NegativeName)
        else name
        ) s
    in
      map f c
    end

    fun shift d = shift_above 0 d

    fun subst j by =
    let
      fun f c s =
        if NameSet.member (c + j) s
        then NameSet.delete (c + j) s |> NameSet.union (NameSet.map_monotonic (fn x => x + c) by)
        else s
    in
      map f 0
    end

    fun subst_top by x =
      subst 0 (NameSet.map_monotonic (fn x => x + 1) by) x
        |> shift (~1)
  end

  structure Term = struct
    datatype lit
      = Num of int

    datatype t
      = Var of var
      | Unit
      | Lit of lit
      | Pair of t * t
      | Prim of string
      | Fst of t
      | Snd of t
      | Abs of support * Type.t * t
      | App of t * t
      | NAbs of t
      | SApp of t * support
      | Box of support * t
      | Unbox of t
      | Rec of Type.t * t
      | Delay of support * t
      | Force of t
      | Ref of t
      | Get of t
      | Set of t * t
      | Callcc of Type.t * t
      | Throw of Type.t * t * t
      | Let of Type.t option * t * t
  end
end

structure Syntax = MakeSyntax struct
  type var = int
  type name = int

  structure NameSet = Set struct
    type t = name
    val compare = Int.compare
  end
end
