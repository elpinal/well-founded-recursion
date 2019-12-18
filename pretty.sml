infixr 4 <>
infixr 4 <+>

structure Pretty : sig
  type t = string

  val <> : t * t -> t
  val <+> : t * t -> t

  val brace : t -> t
  val brack : t -> t
  val angle : t -> t
  val paren : bool -> t -> t
end = struct
  type t = string

  fun op<> (x, y) = x ^ y

  fun op<+> (x, y) = x <> " " <> y

  fun brace s = "{" <> s <> "}"

  fun brack s = "[" <> s <> "]"

  fun angle s = "<" <> s <> ">"

  fun paren true s  = "(" <> s <> ")"
    | paren false s = s
end
