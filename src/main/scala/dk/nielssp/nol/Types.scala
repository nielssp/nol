package dk.nielssp.nol

trait Types {
  def ftv: Set[String]
  def apply(s: Map[String, Monotype]): Types
}
