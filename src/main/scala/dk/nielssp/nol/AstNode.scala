package dk.nielssp.nol

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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

object Definition {
  def sort(definitions: Seq[Definition]): Seq[Definition] =
    definitions.sortWith {
      case (Definition(n1, e1), Definition(n2, e2)) => e2.free.contains(n1)
    }

  private case class Vertex(definition: Definition) {
    var index: Int = -1
    var lowLink: Int = -1
    var onStack = false
    var edges = Set.empty[Vertex]
  }

  /**
    * Implements Tarjan's strongly connected components algorithm
    *
    * https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
    *
    * @param definitions
    * @return
    */
  def group(definitions: Seq[Definition]): Seq[Seq[Definition]] = {
    val groups = new mutable.MutableList[Seq[Definition]]
    var index = 0
    val stack = new mutable.Stack[Vertex]
    val vertices = definitions.map(definition => definition.name -> Vertex(definition)).toMap
    vertices.foreach {
      case (name, v@Vertex(definition)) =>
        v.edges = definition.free.flatMap(vertices.get)
    }
    vertices.foreach {
      case (_, v) if v.index < 0 => strongConnect(v)
      case _ =>
    }

    def strongConnect(vertex: Vertex): Unit = {
      vertex.index = index
      vertex.lowLink = index
      index += 1
      stack.push(vertex)
      vertex.onStack = true

      vertex.edges.foreach {
        case w if w.index < 0 =>
          strongConnect(w)
          vertex.lowLink = math.min(vertex.lowLink, w.lowLink)
        case w if w.onStack =>
          vertex.lowLink = math.min(vertex.lowLink, w.index)
        case _ =>
      }

      if (vertex.lowLink == vertex.index) {
        val group = new ListBuffer[Definition]
        var w: Vertex = null
        do {
          w = stack.pop()
          w.onStack = false
          group += w.definition
        } while (w != vertex)
        groups += group
      }
    }
    groups
  }
}

case class Import(name: String) extends AstNode

sealed abstract class Expr extends AstNode {
  var typeAnnotation: Option[Type] = None
}
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
case class GetExpr(record: Expr, field: String) extends Expr {
  override val free = record.free
}
case class SetExpr(record: Expr, assigns: RecordExpr) extends Expr {
  override val free = record.free ++ assigns.free
}
case class ListExpr(elements: Seq[Expr]) extends Expr {
  override val free = elements.flatMap(_.free).toSet
}
case class TupleExpr(elements: Seq[Expr]) extends Expr {
  override val free = elements.flatMap(_.free).toSet
}
case class RecordExpr(fields: Map[String, Expr]) extends Expr {
  override val free = fields.values.flatMap(_.free).toSet
}
case class NameNode(name: String) extends Expr {
  override val free = Set(name)
}
case class StringNode(value: String) extends Expr
case class IntNode(value: Int) extends Expr
case class FloatNode(value: Double) extends Expr