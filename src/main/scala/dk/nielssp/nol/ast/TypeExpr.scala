package dk.nielssp.nol.ast



case class PolytypeExpr(names: Set[String], constraints: Set[ConstraintExpr], t: MonotypeExpr) extends AstNode

case class ConstraintExpr(typeClass: String, parameters: List[MonotypeExpr]) extends AstNode

sealed abstract class MonotypeExpr extends AstNode

case class TypePrefixExpr(op: MonotypeExpr, parameters: List[MonotypeExpr]) extends MonotypeExpr

case class RecordTypeExpr(fields: Map[String, MonotypeExpr], more: Option[String]) extends MonotypeExpr

case class TypeNameNode(name: String) extends MonotypeExpr

