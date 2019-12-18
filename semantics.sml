structure Semantics : sig
  structure Type : sig
    local open Syntax in
      exception SupportMismatch
      exception NotEqual     of Type.t * Type.t
      exception BaseMismatch of string * string
    end
  end

  val type_of : Env.t -> Syntax.support -> Syntax.Term.t -> Syntax.Type.t
  val type_of_closed : Syntax.Term.t -> Syntax.Type.t
end = struct
  open Syntax
  open Term

  val inc_support = NameSet.map_monotonic (fn name => name + 1)

  structure Type = struct
    open Type

    fun well_formed env =
    let
      fun loop Unit             = ()
        | loop (Base _)         = ()
        | loop (Prod(x, y))     = loop x before loop y
        | loop (Arrow(x, s, y)) = NameSet.app (fn name => Env.Name.member_exn name env) s before loop x before loop y
        | loop (Forall(x))      = well_formed (Env.Name.insert env) x
        | loop (Box(s, x))      = NameSet.app (fn name => Env.Name.member_exn name env) s before loop x
        | loop (Comp(s, x))     = NameSet.app (fn name => Env.Name.member_exn name env) s before loop x
        | loop (Ref x)          = loop x
        | loop (Cont x)         = loop x
    in
      loop
    end

    exception SupportMismatch
    exception NotEqual     of Type.t * Type.t
    exception BaseMismatch of string * string

    fun equal env support =
    let
      fun loop Unit Unit                                       = ()
        | loop (Base s1) (Base s2)                             = if s1 = s2 then () else raise BaseMismatch(s1, s2)
        | loop (Prod(ty11, ty12)) (Prod(ty21, ty22))           = loop ty11 ty21 before loop ty12 ty22
        | loop (Arrow(ty11, s1, ty12)) (Arrow(ty21, s2, ty22)) =
          let
            val t1 = NameSet.union support s1
            val t2 = NameSet.union support s2
            val () =
              if NameSet.equal t1 t2
              then ()
              else raise SupportMismatch
          in
            equal env t1 ty11 ty21 before equal env t2 ty12 ty22
          end
        | loop (Forall(x)) (Forall(y))   = equal (Env.Name.insert env) (inc_support support) x y
        | loop (Box(s1, x)) (Box(s2, y)) =
          let
            val t1 = NameSet.union support s1
            val t2 = NameSet.union support s2
            val () =
              if NameSet.equal t1 t2
              then ()
              else raise SupportMismatch
          in
            equal env t1 x y
          end
        | loop (Comp(s1, x)) (Comp(s2, y)) =
          let
            val t1 = NameSet.union support s1
            val t2 = NameSet.union support s2
            val () =
              if NameSet.equal t1 t2
              then ()
              else raise SupportMismatch
          in
            equal env t1 x y
          end
        | loop (Ref x) (Ref y)   = loop x y
        | loop (Cont x) (Cont y) = loop x y
        | loop ty1 ty2           = raise NotEqual(ty1, ty2)
    in
      loop
    end
  end

  exception NotProduct      of Type.t
  exception NotArrow        of Type.t
  exception NotUniversal    of Type.t
  exception NotBox          of Type.t
  exception NotComputation  of Type.t
  exception NotRef          of Type.t
  exception NotContinuation of Type.t
  exception InadequateSupport

  fun type_of_lit (Num _) = Type.Base "int"

  fun type_of env support =
  let
    fun loop (Var v)      = Env.Var.lookup v env
      | loop Unit         = Type.Unit
      | loop (Lit l)      = type_of_lit l
      | loop (Prim s)     = type_of_prim s
      | loop (Pair(x, y)) = Type.Prod(loop x, loop y)
      | loop (Fst x) =
        (case loop x of
             Type.Prod(ty1, _) => ty1
           | ty                => raise NotProduct(ty))
      | loop (Snd x) =
        (case loop x of
             Type.Prod(_, ty2) => ty2
           | ty                => raise NotProduct(ty))
      | loop (Abs(s, ty1, x)) =
        let
          val () = Type.well_formed env ty1
          val ty2 = type_of (Env.Var.insert ty1 env) (NameSet.union support s) x
        in
          Type.Arrow(ty1, s, ty2)
        end
      | loop (App(x, y)) =
        let
          val (ty11, s, ty12) =
            case loop x of
                 Type.Arrow x => x
               | ty1          => raise NotArrow(ty1)
        in
          let
            val () = type_check env support y ty11
          in
            if NameSet.is_subset_of s support
            then ty12
            else raise InadequateSupport
          end
        end
      | loop (NAbs x)     = Type.Forall(type_of (Env.Name.insert env) (inc_support support) x)
      | loop (SApp(x, s)) =
        (case loop x of
             Type.Forall ty => Type.subst_top s ty
           | ty             => raise NotUniversal(ty))
      | loop (Box(s, x)) = Type.Box(s, loop x)
      | loop (Unbox x)   =
        (case loop x of
              Type.Box(s, ty) =>
                if NameSet.is_subset_of s support
                then ty
                else raise InadequateSupport
            | ty => raise NotBox(ty))
      | loop (Rec(ty1, x)) =
        let
          val () = Type.well_formed env ty1
          val support = inc_support support
          val ty1' = Type.shift 1 ty1
          val ty2 = type_of (Env.Name.insert env |> Env.Var.insert (Type.Box(NameSet.singleton 0, ty1'))) support x
          val () = Type.equal (Env.Name.insert env) (NameSet.insert 0 support) ty2 ty1'
        in
          ty1
        end
      | loop (Delay(s, x)) = Type.Comp(s, type_of env (NameSet.union support s) x)
      | loop (Force x)     =
        let
          val (s, ty) =
            case loop x of
                 Type.Comp x => x
               | ty          => raise NotComputation(ty)
        in
          if NameSet.is_subset_of s support
          then ty
          else raise InadequateSupport
        end
      | loop (Ref x) = Type.Ref $ loop x
      | loop (Get x) =
        (case loop x of
              Type.Ref ty => ty
            | ty          => raise NotRef(ty))
      | loop (Set(x, y)) =
        let
          val ty =
            case loop x of
                 Type.Ref ty => ty
               | ty          => raise NotRef(ty)
        in
          Type.Unit before type_check env support y ty
        end
      | loop (Callcc(ty, x)) =
        ty
        before Type.well_formed env ty
        before type_check (Env.Var.insert (Type.Cont ty) env) support x ty
      | loop (Throw(ty1, x, y)) =
        let
          val () =
            case loop x of
                 Type.Cont ty => type_check env support y ty
               | ty           => raise NotContinuation(ty)
        in
          ty1 before Type.well_formed env ty1
        end
      | loop (Let(oty, x, y)) =
        let
          val ty =
            case oty of
                 NONE    => loop x
               | SOME ty =>
                   ty
                     before Type.well_formed env ty
                     before type_check env support x ty
        in
          type_of (Env.Var.insert ty env) support y
        end
  in
    loop
  end

  and type_check env support x ty2 =
  let
    val ty1 = type_of env support x
  in
    Type.equal env support ty1 ty2
  end

  and type_of_prim s =
  let
    exception UnknownPrimitive of string

    open Type

    val int = Type.Base "int"
    fun arrow x y = Arrow(x, NameSet.empty, y)
  in
    case s of
         "*" => arrow int (arrow int int)
       | "+" => arrow int (arrow int int)
       | "-" => arrow int (arrow int int)
       | _   => raise UnknownPrimitive(s)
  end

  val type_of_closed = type_of Env.empty NameSet.empty
end
