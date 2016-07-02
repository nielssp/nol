package dk.nielssp.nol.ast

import dk.nielssp.nol.Type

sealed abstract class Expr extends AstNode {
  var typeAnnotation: Option[Type] = None
}

case class LetExpr(assigns: List[Assignment], body: Expr) extends Expr {
  override val free = assigns.flatMap(_.free).toSet ++ body.free -- assigns.map(_.name)
}
case class LambdaExpr(names: List[String], expr: Expr) extends Expr {
  override val free = expr.free -- names
}
case class IfExpr(cond: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr {
  override val free = cond.free ++ ifTrue.free ++ ifFalse.free
}
case class InfixExpr(op: Expr, left: Expr, right: Expr) extends Expr {
  override val free = op.free ++ left.free ++ right.free
}
case class PrefixExpr(op: Expr, arg: Expr) extends Expr {
  override val free = op.free ++ arg.free
}
case class GetExpr(record: Expr, field: String) extends Expr {
  override val free = record.free
}
case class SetExpr(record: Expr, assigns: RecordExpr) extends Expr {
  override val free = record.free ++ assigns.free
}
case class ListExpr(elements: List[Expr]) extends Expr {
  override val free = elements.flatMap(_.free).toSet
}
case class TupleExpr(elements: List[Expr]) extends Expr {
  override val free = elements.flatMap(_.free).toSet
}
case class RecordExpr(fields: Map[String, Expr]) extends Expr {
  override val free = fields.values.flatMap(_.free).toSet
}
case class PolytypeExpr(names: Set[String], constraints: List[Expr], t: Expr) extends Expr {
  override val free = t.free ++ constraints.flatMap(_.free) -- names
}
case class RecordTypeExpr(fields: Map[String, Expr], more: Option[String]) extends Expr {
  override val free = fields.values.flatMap(_.free).toSet ++ more
}
case class TupleTypeExpr(elements: List[Expr]) extends Expr {
  override val free = elements.flatMap(_.free).toSet
}
case class ListTypeExpr(element: Expr) extends Expr {
  override val free = element.free
}
case class NameNode(name: String) extends Expr {
  override val free = Set(name)
}
case class StringNode(value: String) extends Expr
case class IntNode(value: Int) extends Expr
case class FloatNode(value: Double) extends Expr