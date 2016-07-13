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
    val interpreter = new Interpreter
    val typeChecker = new TypeChecker
    loader.phases ++= List(typeChecker, new TypeEvaluator, new DeclarationChecker, interpreter)
    var env = std.external
    console.addCompleter(new Completer {
      override def complete(buffer: String, cursor: Int, candidates: util.List[CharSequence]): Int = {
        val s = buffer.take(cursor)
        env.values.keys.filter(_.startsWith(s)).toSeq.sorted.foreach(cand => candidates.add(cand))
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
          env = env.union(loader.load(name).external)
        } else if (line.startsWith(":t")) {
          val tokens = lex(line.drop(2))
          val ast = parse.repl(tokens)
          ast match {
            case p: Program =>
            case e: Expr =>
              val (s, context, t) = typeChecker(e, env)
              console.println(s" : ${env.generalize(context.map(_(s)), t(s)).prettify}")
          }
        } else {
            val tokens = lex(line)
            val ast = parse.repl(tokens)
            ast match {
              case p: Program =>
                val module = new Module("(main)", p, env, SymbolTable.empty)
                env = loader.compile(module).internal
              case e: Expr =>
                val (s, context, t) = typeChecker(e, env)
                val value = interpreter(e, env)
                console.println(s"$value : ${env.generalize(context.map(_(s)), t(s)).prettify}")
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
