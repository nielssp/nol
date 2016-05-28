package dk.nielssp.nol

import scala.util.parsing.input.Positional

abstract class AstNode extends Positional

case class Program(statements: Seq[Statement]) extends AstNode

sealed abstract class Statement extends AstNode
case class Definition(name: String, value: Expr) extends Statement
case class Import(name: String) extends Statement

sealed abstract class Expr extends AstNode
case class LetExpr(assigns: Seq[Definition], body: Expr) extends Expr
case class LambdaExpr(names: Seq[String], expr: Expr) extends Expr
case class IfExpr(cond: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr
case class InfixExpr(op: Expr, left: Expr, right: Expr) extends Expr
case class PrefixExpr(op: Expr, arg: Expr) extends Expr
case class ListExpr(elements: Seq[Expr]) extends Expr
case class NameNode(name: String) extends Expr
case class StringNode(value: String) extends Expr
case class IntNode(value: Int) extends Expr
case class FloatNode(value: Double) extends Expr
