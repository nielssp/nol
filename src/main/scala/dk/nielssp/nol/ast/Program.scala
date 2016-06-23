package dk.nielssp.nol.ast

case class Program(imports: Seq[Import], definitions: Seq[Definition]) extends AstNode {
  override val free = definitions.flatMap(_.free).toSet -- definitions.map(_.name)
}
