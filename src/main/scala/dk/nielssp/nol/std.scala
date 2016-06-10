package dk.nielssp.nol

object std extends Module("std", Program(Seq.empty, Seq.empty)) {
  override val typeEnv: Map[String, Type] = Map(
    "Type" -> Monotype.Type,
    "Constraint" -> Monotype.Type,
    "Int" -> Monotype.Type,
    "Float" -> Monotype.Type,
    "String" -> Monotype.Type,
    "Bool" -> Monotype.Type,
    "List" -> Monotype.Function(Monotype.Type, Monotype.Type),
    "->" -> Monotype.dyadic(Monotype.Type, Monotype.Type, Monotype.Type),
    "Num" -> Monotype.Function(Monotype.Type, Monotype.Constraint),
    "Eq" -> Monotype.Function(Monotype.Type, Monotype.Constraint),
    "+" -> Monotype.dyadic(Monotype.Int, Monotype.Int, Monotype.Int),
    "-" -> Monotype.dyadic(Monotype.Int, Monotype.Int, Monotype.Int),
    "*" -> Monotype.dyadic(Monotype.Int, Monotype.Int, Monotype.Int),
    "/" -> Monotype.dyadic(Monotype.Int, Monotype.Int, Monotype.Int),
    "<" -> Monotype.dyadic(Monotype.Int, Monotype.Int, Monotype.Bool),
    ">" -> Monotype.dyadic(Monotype.Int, Monotype.Int, Monotype.Bool),
    "<=" -> Monotype.dyadic(Monotype.Int, Monotype.Int, Monotype.Bool),
    ">=" -> Monotype.dyadic(Monotype.Int, Monotype.Int, Monotype.Bool),
    "==" -> TypeScheme(List("a"), TypeContext(Seq(Constraint(Monotype.Eq, TypeVar("a"))), Monotype.dyadic(TypeVar("a"), TypeVar("a"), Monotype.Bool))),
    "::" -> TypeScheme(
      List("a"),
      Monotype.dyadic(TypeVar("a"), Monotype.List(TypeVar("a")), Monotype.List(TypeVar("a")))
    ),
    "neg" -> Monotype.Function(Monotype.Int, Monotype.Int),
    "head" -> TypeScheme(
      List("a"),
      Monotype.Function(Monotype.List(TypeVar("a")), TypeVar("a"))
    ),
    "tail" -> TypeScheme(
      List("a"),
      Monotype.Function(Monotype.List(TypeVar("a")), Monotype.List(TypeVar("a")))
    ),
    "true" -> Monotype.Bool,
    "false" -> Monotype.Bool
  )
}
