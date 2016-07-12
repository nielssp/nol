package dk.nielssp.nol

import dk.nielssp.nol.ast._

class Interpreter extends (Module => Module) with Interpreters {

  def apply(module: Module): Module = {
    var internal = module.internal
    val external = module.program.definitions.foldLeft(module.external) {
      case (scope, Assignment(name, value)) =>
        scope.updated(name, LazyValue(() => apply(value, internal)))
      case (scope, _) => scope
    }
    internal = internal.union(external)
    module.withInternal(internal).withExternal(external)
  }
}
