package dk.nielssp.nol.ast

import scala.collection.mutable

abstract sealed class Definition extends AstNode {
  val name: String
}

case class ValueDefinition(name: String, value: Expr) extends Definition

case class TypeDefinition(name: String, t: PolytypeExpr) extends Definition

case class TypeClassDefinition(name: String, parameters: Seq[String], constraints: Set[ConstraintExpr], members: Seq[Definition]) extends Definition

case class InstanceDefinition(names: Set[String], constraints: Set[ConstraintExpr], instance: ConstraintExpr, members: Seq[Definition]) extends Definition {
  override val name = ???
}

object Definition {
  def sort(definitions: Seq[Definition]): Seq[Definition] =
    definitions.sortWith {
      case (def1, def2) => def2.free.contains(def1.name)
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
        val group = new mutable.ListBuffer[Definition]
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