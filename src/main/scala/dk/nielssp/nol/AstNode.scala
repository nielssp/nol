package dk.nielssp.nol

import scala.util.parsing.input.Positional

abstract class AstNode extends Positional

case class Assign(name: String, value: Expr) extends AstNode

sealed abstract class Expr extends AstNode
case class LetExpr(assigns: Seq[Assign], body: Expr) extends Expr
case class LambdaExpr(names: Seq[String], expr: Expr) extends Expr
case class IfExpr(cond: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr
case class InfixExpr(op: Expr, left: Expr, right: Expr) extends Expr
case class PrefixExpr(op: Expr, arg: Expr) extends Expr
case class ListExpr(elements: Seq[Expr]) extends Expr
case class NameNode(name: String) extends Expr
case class StringNode(value: String) extends Expr
case class IntNode(value: Int) extends Expr
case class FloatNode(value: Double) extends Expr
