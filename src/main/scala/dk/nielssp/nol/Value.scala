package dk.nielssp.nol

sealed abstract class Value
case class IntValue(value: Int) extends Value
case class FloatValue(value: Double) extends Value
case class StringValue(value: String) extends Value
case class ListValue(elements: Seq[Value]) extends Value
case class LambdaValue(f: Value => Value) extends Value
