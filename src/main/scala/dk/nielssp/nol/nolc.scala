package dk.nielssp.nol

import java.io.FileWriter

import scala.tools.jline.console.ConsoleReader

object nolc {

  val loader = new ModuleLoader
  val generator = new JsGenerator(loader)

  import generator.{Inlinable, PrefixInlinable, InfixInlinable}

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
    "==" -> generator.none,
    "::" -> generator.none,
    "neg" -> {
      case PrefixInlinable(a) => s"(-$a)"
    },
    "head" -> {
      case PrefixInlinable(a) => s"$a[0]"
    },
    "tail" -> {
      case PrefixInlinable(a) => s"$a.slice(1)"
    },
    "true" -> generator.none,
    "false" -> generator.none
  )

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("usage: nolc MODULE")
    } else {
      val out = new FileWriter(s"${args.head}.js")
      out.write(
        s"""
           |function ${generator.encode("::")}(x){return function(xs){var xs = xs.slice(0); xs.unshift(x); return xs; };}
           |function ${generator.encode("==")}(a){return function(b){
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
      try {
        loader.modules("std") = std
        loader.includePath += "."
        val interpreter = new Interpreter(loader)
        val typeChecker = new TypeChecker(loader)
        typeChecker.modules("std") = std.external
        generator.modules("std") = stdlib
        val module = loader.load(args.head)
        typeChecker(module.program)
        out.write(generator.preamble(stdlib))
        out.write(generator(module.program)._1)
      } catch {
        case e: Error =>
          println(s"Error: ${e.getMessage} on line ${e.pos.line} column ${e.pos.column} in ${e.file}")
          println(e.pos.longString)
      }
      out.close()
    }
  }
}
