package dk.nielssp.nol

import dk.nielssp.nol.ast.AstNode

trait TypeCheckers {
  private var supply = 0

  def newTypeVar(prefix: String = "t"): TypeVar = {
    supply += 1
    TypeVar(prefix + supply)
  }

  def tryUnify(t1: Monotype, t2: Monotype, node: AstNode): Map[String, Monotype] =
    try {
      t1.unify(t2)
    } catch {
      case e: TypeError =>
        e.pos = node.pos
        throw e
    }
}
