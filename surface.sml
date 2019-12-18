structure Surface = struct
  structure Var :> sig
    type t

    val from_string : string -> t
    val equal : t -> t -> bool
  end = struct
    type t = string

    fun from_string s = s
    fun equal x y = x = y
  end

  type var = Var.t
  type name = string
  type support = name list

  val to_support_set : (name -> int) -> support -> Syntax.NameSet.t = fn f =>
    Syntax.NameSet.from_list o List.map f

  structure Type = struct
    datatype t
      = Unit
      | Base of string
      | Prod of t * t
      | Arrow of t * support * t
      | Forall of name * t
      | Box of support * t
      | Comp of support * t
      | Ref of t
      | Cont of t

    structure SType = Syntax.Type

    fun to_internal (nenv : string -> int) : t -> SType.t =
    let
      fun loop Unit             = SType.Unit
        | loop (Base s)         = SType.Base s
        | loop (Prod(x, y))     = SType.Prod(loop x, loop y)
        | loop (Arrow(x, s, y)) = SType.Arrow(loop x, to_support_set nenv s, loop y)
        | loop (Forall(n, x))   = SType.Forall(to_internal (fn s => if s = n then 0 else nenv s + 1) x)
        | loop (Box(s, x))      = SType.Box(to_support_set nenv s, loop x)
        | loop (Comp(s, x))     = SType.Comp(to_support_set nenv s, loop x)
        | loop (Ref x)          = SType.Ref(loop x)
        | loop (Cont x)         = SType.Cont(loop x)
    in
      loop
    end
  end

  structure Term = struct
    structure STerm = Syntax.Term

    datatype t
      = Var of var
      | Unit
      | Lit of STerm.lit
      | Prim of string
      | Pair of t * t
      | Fst of t
      | Snd of t
      | Abs of support * var * Type.t * t
      | App of t * t
      | NAbs of name * t
      | SApp of t * support
      | Box of support * t
      | Unbox of t
      | Rec of name * var * Type.t * t
      | Delay of support * t
      | Force of t
      | Ref of t
      | Get of t
      | Set of t * t
      | Callcc of Type.t * var * t
      | Throw of Type.t * t * t
      | Let of var * Type.t option * t * t

    fun extend v env z =
      if Var.equal z v
      then 0
      else env z + 1

    fun to_internal (nenv : string -> int) (env : var -> int) : t -> STerm.t =
    let
      fun loop (Var s)            = STerm.Var $ env s
        | loop Unit               = STerm.Unit
        | loop (Lit l)            = STerm.Lit l
        | loop (Prim s)           = STerm.Prim s
        | loop (Pair(x, y))       = STerm.Pair(loop x, loop y)
        | loop (Fst x)            = STerm.Fst $ loop x
        | loop (Snd x)            = STerm.Snd $ loop x
        | loop (Abs(s, v, ty, x)) = STerm.Abs(to_support_set nenv s, Type.to_internal nenv ty, to_internal nenv (extend v env) x)
        | loop (App(x, y))        = STerm.App(loop x, loop y)
        | loop (NAbs(n, x))       = STerm.NAbs(to_internal (fn s => if s = n then 0 else nenv s + 1) env x)
        | loop (SApp(x, s))       = STerm.SApp(loop x, to_support_set nenv s)
        | loop (Box(s, x))        = STerm.Box(to_support_set nenv s, loop x)
        | loop (Unbox(x))         = STerm.Unbox(loop x)
        | loop (Rec(n, v, ty, x)) = STerm.Rec(Type.to_internal nenv ty, to_internal (fn z => if z = n then 0 else nenv z + 1) (extend v env) x)
        | loop (Delay(s, x))      = STerm.Delay(to_support_set nenv s, loop x)
        | loop (Force x)          = STerm.Force(loop x)
        | loop (Ref x)            = STerm.Ref(loop x)
        | loop (Get x)            = STerm.Get(loop x)
        | loop (Set(x, y))        = STerm.Set(loop x, loop y)
        | loop (Callcc(ty, v, x)) = STerm.Callcc(Type.to_internal nenv ty, to_internal nenv (extend v env) x)
        | loop (Throw(ty, x, y))  = STerm.Throw(Type.to_internal nenv ty, loop x, loop y)
        | loop (Let(v, ty, x, y)) = STerm.Let(Option.map (Type.to_internal nenv) ty, loop x, to_internal nenv (extend v env) y)
    in
      loop
    end

    exception UnboundName     of name
    exception UnboundVariable of var

    val to_internal = to_internal
      (fn name => raise UnboundName(name))
      (fn v => raise UnboundVariable(v))
  end
end
