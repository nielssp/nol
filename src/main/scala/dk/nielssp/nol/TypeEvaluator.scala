package dk.nielssp.nol

import dk.nielssp.nol.ast._

import scala.collection.immutable.Stack
import scala.util.parsing.input.NoPosition

class TypeEvaluator extends (Module => Module) with Interpreters {
  def apply(module: Module): Module = {
    var internal = module.internal
    val external = module.program.definitions.foldLeft(module.external) {
      case (scope, Assignment(name, value)) =>
        scope.updated(name, LazyValue(() => apply(value, internal)))
      case (scope, TypeClassDefinition(name, parameters, _, _)) =>
        scope.updated(name, new TypeClass(name, parameters.length).toFunction)
      case (scope, _) => scope
    }
    internal = internal.union(external)
    module.program.definitions.foreach {
      case TypeClassDefinition(_, parameters, _, members) =>
        val env = internal.withValues(internal.values ++ parameters.map(name => name -> new TypeVar(name)))
        members.foreach {
          case d@Declaration(name, t) =>
            println(s"$name : ${apply(t, env)}")
        }
      case d@Declaration(name, t) =>
        println(s"$name : ${apply(t, internal)}")
      case _ =>
    }
    module.withInternal(internal).withExternal(external)
  }
}
