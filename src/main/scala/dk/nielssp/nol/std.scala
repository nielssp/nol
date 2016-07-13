package dk.nielssp.nol

import dk.nielssp.nol.ast.Program

object std extends Module("std", Program(Seq.empty, Seq.empty), SymbolTable.empty, SymbolTable.empty) {

  def monadic(f: PartialFunction[Value, Value]): LambdaValue = LambdaValue {
    case param => f.lift(param) match {
      case Some(value) => value
      case None => throw new DomainError(s"undefined for ${param.getClass.getSimpleName}")
    }
  }

  def dyadic(f: PartialFunction[(Value, Value), Value]): LambdaValue = LambdaValue {
    case a => LambdaValue {
      case b => f.lift(a, b) match {
        case Some(value) => value
        case None => throw new DomainError(s"undefined for (${a.getClass.getSimpleName}, ${b.getClass.getSimpleName})")
      }
    }
  }

  override val external = SymbolTable(
    Map(
      "Type" -> Monotype.Type,
      "Int" -> Monotype.Int,
      "Float" -> Monotype.Float,
      "String" -> Monotype.String,
      "Bool" -> Monotype.Bool,
      "List" -> monadic{
        case t: Monotype => Monotype.List(t)
      },
      "->" -> dyadic{
        case (a: Monotype, b: Monotype) => Monotype.Function(a, b)
      },
      "Num" -> monadic {
        case t: Monotype => Constraint(Monotype.Num, t)
      },
      "Eq" -> monadic {
        case t: Monotype => Constraint(Monotype.Eq, t)
      },
      "+" -> dyadic {
        case (IntValue(a), IntValue(b)) => IntValue(a + b)
      },
      "-" -> dyadic {
        case (IntValue(a), IntValue(b)) => IntValue(a - b)
      },
      "*" -> dyadic {
        case (IntValue(a), IntValue(b)) => IntValue(a * b)
      },
      "/" -> dyadic {
        case (IntValue(a), IntValue(b)) => IntValue(a / b)
      },
      "<" -> dyadic {
        case (IntValue(a), IntValue(b)) => BoolValue(a < b)
      },
      ">" -> dyadic {
        case (IntValue(a), IntValue(b)) => BoolValue(a > b)
      },
      "<=" -> dyadic {
        case (IntValue(a), IntValue(b)) => BoolValue(a <= b)
      },
      ">=" -> dyadic {
        case (IntValue(a), IntValue(b)) => BoolValue(a >= b)
      },
      "==" -> dyadic {
        case (a, b) => BoolValue(a == b)
      },
      "::" -> dyadic {
        case (x, ListValue(xs)) => ListValue(x :: xs)
      },
      "neg" -> monadic {
        case (IntValue(x)) => IntValue(-x)
      },
      "head" -> monadic {
        case (ListValue(x :: xs)) => x
      },
      "tail" -> monadic {
        case (ListValue(x :: xs)) => ListValue(xs)
      },
      "true" -> BoolValue(true),
      "false" -> BoolValue(false)
    ),
    Map(
      "Type" -> Monotype.Type,
      "Constraint" -> Monotype.Type,
      "Int" -> Monotype.Type,
      "Float" -> Monotype.Type,
      "String" -> Monotype.Type,
      "Bool" -> Monotype.Type,
      "List" -> Monotype.Function(Monotype.Type, Monotype.Type),
      "Tuple" -> Monotype.Function(Monotype.List(Monotype.Type), Monotype.Type),
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
      "==" -> TypeScheme(Set("a"), Set(Constraint(Monotype.Eq, TypeVar("a"))), Monotype.dyadic(TypeVar("a"), TypeVar("a"), Monotype.Bool)),
      "::" -> TypeScheme(
        Set("a"), Set.empty,
        Monotype.dyadic(TypeVar("a"), Monotype.List(TypeVar("a")), Monotype.List(TypeVar("a")))
      ),
      "neg" -> Monotype.Function(Monotype.Int, Monotype.Int),
      "head" -> TypeScheme(
        Set("a"), Set.empty,
        Monotype.Function(Monotype.List(TypeVar("a")), TypeVar("a"))
      ),
      "tail" -> TypeScheme(
        Set("a"), Set.empty,
        Monotype.Function(Monotype.List(TypeVar("a")), Monotype.List(TypeVar("a")))
      ),
      "true" -> Monotype.Bool,
      "false" -> Monotype.Bool
    ),
    Map(),
    Map()
  )
}
