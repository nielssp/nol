package dk.nielssp.nol

import scala.util.parsing.input.Positional

abstract class AstNode extends Positional {
  val free = Set.empty[String]
}

case class Program(imports: Seq[Import], definitions: Seq[Definition]) extends AstNode {
  override val free = definitions.flatMap(_.free).toSet -- definitions.map(_.name)
}

case class Definition(name: String, value: Expr) extends AstNode {
  override val free = value.free
}
case class Import(name: String) extends AstNode

sealed abstract class Expr extends AstNode
case class LetExpr(assigns: Seq[Definition], body: Expr) extends Expr {
  override val free = assigns.flatMap(_.free).toSet ++ body.free -- assigns.map(_.name)
}
case class LambdaExpr(names: Seq[String], expr: Expr) extends Expr {
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
case class ListExpr(elements: Seq[Expr]) extends Expr {
  override val free = elements.flatMap(_.free).toSet
}
case class NameNode(name: String) extends Expr {
  override val free = Set(name)
}
case class StringNode(value: String) extends Expr
case class IntNode(value: Int) extends Expr
case class FloatNode(value: Double) extends Expr
