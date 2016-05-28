package dk.nielssp.nol

import scala.tools.jline.console.ConsoleReader

object nolc {

  val compiler = new JsGenerator

  import compiler.{Inlinable, PrefixInlinable, InfixInlinable}

  val stdlib = Map[String, PartialFunction[Inlinable, String]](
    "+" -> {
      case InfixInlinable(a, b) => s"($a+$b)"
    },
    "-" -> {
      case InfixInlinable(a, b) => s"($a-$b)"
    },
    "*" -> {
      case InfixInlinable(a, b) => s"($a*$b)"
    },
    "/" -> {
      case InfixInlinable(a, b) => s"($a/$b|0)"
    },
    "<" -> {
      case InfixInlinable(a, b) => s"($a<$b)"
    },
    ">" -> {
      case InfixInlinable(a, b) => s"($a>$b)"
    },
    "<=" -> {
      case InfixInlinable(a, b) => s"($a<=$b)"
    },
    ">=" -> {
      case InfixInlinable(a, b) => s"($a>=$b)"
    },
    "==" -> compiler.none,
    "::" -> compiler.none,
    "neg" -> {
      case PrefixInlinable(a) => s"(-$a)"
    },
    "head" -> {
      case PrefixInlinable(a) => s"$a[0]"
    },
    "tail" -> {
      case PrefixInlinable(a) => s"$a.slice(1)"
    },
    "true" -> compiler.none,
    "false" -> compiler.none
  )

  def main(args: Array[String]): Unit = {
    val console = new ConsoleReader
    var scope: compiler.SymbolTable = stdlib
    console.println(
      s"""
        |function ${compiler.encode("::")}(x){return function(xs){var xs = xs.slice(0); xs.unshift(x); return xs; };}
        |function ${compiler.encode("==")}(a){return function(b){
        |  if (a === b) return true;
        |  if (typeof a !== "object" || typeof b !== "object") return false;
        |  var ap = Object.getOwnPropertyNames(a);
        |  var bp = Object.getOwnPropertyNames(b);
        |  if (ap.length != bp.length) return false;
        |  for (var i = 0; i < ap.length; i++) {
        |    var name = ap[i];
        |    if (a[name] !== b[name]) return false;
        |  }
        |  return true;
        |};}
      """.stripMargin)
    console.println(compiler.preamble(scope))
    while (true) {
      val line = console.readLine("> ")
      if (line == null) {
        System.exit(0)
      }
      try {
        val tokens = lex(line)
        val ast = parse.repl(tokens)
        ast match {
          case p: Program => compiler(p, scope) match {
            case (out, module) =>
              console.println(out)
              scope = module.symbols
          }
          case e: Expr =>
            val value = compiler(e, scope)
            console.println(s"$value")
        }
      } catch {
        case e: Error =>
          console.println(s"Error: ${e.getMessage} on line ${e.pos.line} column ${e.pos.column} in ${e.file}")
          console.println(e.pos.longString)
      }
    }
  }
}
