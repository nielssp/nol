package dk.nielssp.nol

import java.util

import dk.nielssp.nol.ast.{Expr, Program}

import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.completer.Completer

object noli {

  def main(args: Array[String]): Unit = {
    val console = new ConsoleReader
    val loader = new ModuleLoader
    loader.modules("std") = std
    loader.includePath += "."
    val interpreter = new Interpreter(loader)
    val typeChecker = new TypeChecker(loader)
    typeChecker.modules("std") = std.external
    interpreter.modules("std") = std.external
    var types = std.external
    var scope = std.external
    console.addCompleter(new Completer {
      override def complete(buffer: String, cursor: Int, candidates: util.List[CharSequence]): Int = {
        val s = buffer.take(cursor)
        scope.values.keys.filter(_.startsWith(s)).toSeq.sorted.foreach(cand => candidates.add(cand))
        0
      }
    })
    while (true) {
      val line = console.readLine("> ")
      try {
        if (line == null) {
          System.exit(0)
        } else if (line.startsWith(":i")) {
          val name = line.drop(2).trim
          types = types.union(types.union(typeChecker(loader(name).program, types)._2))
          scope = scope.union(interpreter(loader(name).program, scope)._2)
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
                types = types.union(typeChecker(p, types)._1)
                scope = scope .union(interpreter(p, scope)._1)
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
