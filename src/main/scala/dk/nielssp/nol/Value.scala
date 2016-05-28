package dk.nielssp.nol

sealed abstract class Value
case class IntValue(value: Int) extends Value {
  override def toString = value.toString
}
case class BoolValue(value: Boolean) extends Value {
  override def toString = value.toString
}
case class FloatValue(value: Double) extends Value {
  override def toString = value.toString
}
case class StringValue(value: String) extends Value {
  override def toString = value.toString
}
case class ListValue(elements: List[Value]) extends Value {
  override def toString = s"[${elements.map(_.toString).mkString(" ")}]"
}
case class LambdaValue(f: Value => Value) extends Value {
  override def toString = "#function"
}
case class LazyValue(value: () => Value) extends Value {
  override def toString = value().toString
}
