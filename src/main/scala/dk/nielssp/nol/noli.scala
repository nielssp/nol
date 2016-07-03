package dk.nielssp.nol

import dk.nielssp.nol.ast.{Expr, Program}

import scala.tools.jline.console.ConsoleReader

object noli {

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

  val stdImplementation = Map(
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
  )

  def main(args: Array[String]): Unit = {
    val console = new ConsoleReader
    val loader = new ModuleLoader
    loader.modules("std") = std
    loader.includePath += "."
    val typeChecker = new TypeChecker(loader)
    val interpreter = new Interpreter(loader)
    typeChecker.modules("std") = TypeEnv(std.typeEnv)
    interpreter.modules("std") = stdImplementation
    var types = TypeEnv(std.typeEnv)
    var scope = stdImplementation
    while (true) {
      val line = console.readLine("> ")
      try {
        if (line == null) {
          System.exit(0)
        } else if (line.startsWith(":i")) {
          val name = line.drop(2).trim
          types = types.union(types.union(typeChecker(loader(name).program, types)))
          scope = scope ++ interpreter(loader(name).program, scope)
        } else if (line.startsWith(":t")) {
          val tokens = lex(line.drop(2))
          val ast = parse.repl(tokens)
          ast match {
            case p: Program =>
            case e: Expr =>
              val (s, context, t) = typeChecker(e, types)
              console.println(s" : ${types.generalize(context.map(_(s)), t(s)).prettify}")
          }
        } else if (line.startsWith(":k")) {
          val tokens = lex(line.drop(2))
          val ast = parse.kind(tokens)
//          val t = typeChecker(ast, types)
//          console.println(s" : ${t.prettify}")
        } else {
            val tokens = lex(line)
            val ast = parse.repl(tokens)
            ast match {
              case p: Program =>
                types = types.union(typeChecker(p, types))
                scope = scope ++ interpreter(p, scope)
              case e: Expr =>
                val (s, context, t) = typeChecker(e, types)
                val value = interpreter(e, scope)
                console.println(s"$value : ${types.generalize(context.map(_(s)), t(s)).prettify}")
  //              val (_, t) = typeChecker(e, TypeEnv(Map.empty))
  //              console.println(s" : ${TypeEnv(Map.empty).generalize(t).prettify}")
            }
        }
      } catch {
        case e: Error =>
          console.println(s"${e.getClass.getSimpleName}: ${e.getMessage} on line ${e.pos.line} column ${e.pos.column} in ${e.file}")
          console.println(e.pos.longString)
      }
    }
  }
}
