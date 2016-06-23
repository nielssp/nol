package dk.nielssp.nol.ast

import scala.util.parsing.input.Positional

abstract class AstNode extends Positional {
  val free = Set.empty[String]
}
