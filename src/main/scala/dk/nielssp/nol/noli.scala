package dk.nielssp.nol

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

  val stdlib = Map(
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
    val interpreter = new Interpreter
    val typeChecker = new TypeChecker
    interpreter.globals ++= stdlib
    var scope: interpreter.SymbolTable = interpreter.globals.toMap
    while (true) {
      val line = console.readLine("> ")
      if (line == null) {
        System.exit(0)
      } else if (line == ":r") {
        console.println("Refreshing imports...")
        interpreter.refreshImports()
      } else {
        try {
          val tokens = lex(line)
          val ast = parse.repl(tokens)
          ast match {
            case p: Program => scope = interpreter(p, scope).symbols
            case e: Expr =>
              val value = interpreter(e, scope)
              console.println(s"$value")
//              val (_, t) = typeChecker(TypeEnv(Map.empty), e)
//              console.println(s" : ${TypeEnv(Map.empty).generalize(t).prettify}")
          }
        } catch {
          case e: Error =>
            console.println(s"Error: ${e.getMessage} on line ${e.pos.line} column ${e.pos.column} in ${e.file}")
            console.println(e.pos.longString)
        }
      }
    }
  }
}
