package dk.nielssp.nol

import scala.util.parsing.input.NoPosition

sealed abstract class Value {
  def getType: Type
}
case class IntValue(value: Int) extends Value {
  override def toString = value.toString
  override val getType = Type.Int
}
case class BoolValue(value: Boolean) extends Value {
  override def toString = value.toString
  override val getType = Type.Bool
}
case class FloatValue(value: Double) extends Value {
  override def toString = value.toString
  override val getType = Type.Float
}
case class StringValue(value: String) extends Value {
  override def toString = value.toString
  override val getType = Type.String
}
case class ListValue(elements: List[Value]) extends Value {
  override def toString = s"[${elements.map(_.toString).mkString(" ")}]"
  override def getType = ??? // TODO: add element type to constructor
}
case class LambdaValue(f: Value => Value) extends Value {
  override def toString = "#function"
  override def getType = ??? // TODO: add in/out types to constructor
}
case class LazyValue(value: () => Value) extends Value {
  override def toString = value().toString
  override def getType = value().getType
}

sealed class Type extends Value {
  override def getType = Type.Type

  def ftv = Set.empty[String]

  def apply(sub: Map[String, Type]): Type = this
  def unify(other: Type): Map[String, Type] = other match {
    case _ if other == this => Map.empty
    case TypeVar(name) => bind(name)
    case _ => throw new TypeError("types did not unify", NoPosition)
  }
  def bind(name: String): Map[String, Type] =
    if (ftv.contains(name)) throw new TypeError("occurs check failed", NoPosition)
    else Map(name -> this)
}

case class TypeVar(name: String) extends Type {
  override val ftv = Set(name)
  override def apply(sub: Map[String, Type]): Type = sub.get(name) match {
    case Some(t) => t.apply(sub)
    case None => this
  }
  override def unify(other: Type): Map[String, Type] = other.bind(name)
  override def bind(otherName: String): Map[String, Type] =
    if (name == otherName) Map.empty
    else Map(otherName -> this)
}

case class CompositeType(types: Type*) extends Type {
  override val ftv = types.flatMap(_.ftv).toSet
  override def apply(sub: Map[String, Type]): Type = CompositeType(types.map(_(sub)): _*)
  override def unify(other: Type): Map[String, Type] = other match {
    case CompositeType(otherTypes @ _*) if types.length == otherTypes.length =>
      otherTypes.zip(types).foldRight(Map.empty[String, Type]) {
        case ((t1, t2), sub) =>
          Type.compose(sub, t1.apply(sub).unify(t2.apply(sub)))
      }
    case _ => super.unify(other)
  }
}

object Type {
  val Type = new Type
  val Int = new Type
  val Float = new Type
  val Bool = new Type
  val String = new Type

  private val listTag = new Type
  def List(element: Type): Type = CompositeType(listTag, element)

  private val functionTag = new Type
  def Function(in: Type, out: Type): Type = CompositeType(functionTag, in, out)

  def compose(head: Map[String, Type], tail: Map[String, Type]*): Map[String, Type] =
    tail.foldRight(head)((s1, s2) => s1.mapValues(_.apply(s2)) ++ s2)
}