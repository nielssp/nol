package dk.nielssp.nol

import scala.util.parsing.input.NoPosition

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

sealed abstract class Type extends Value with Types {
  def apply(s: Map[String, Monotype]): Type

  def instantiate(newVar: String => TypeVar): Monotype

  def prettify: Type = this
}

case class TypeScheme(names: List[String], t: Monotype) extends Type {
  override def toString = if (names.isEmpty) t.toString else "forall " + names.mkString(", ") + s" . $t"
  override def ftv = t.ftv -- names
  override def apply(s: Map[String, Monotype]): TypeScheme = TypeScheme(names, t.apply(s -- names))

  override def instantiate(newVar: String => TypeVar): Monotype = {
    val newNames = names.map(_ => newVar("t"))
    t.apply(Map.empty ++ names.zip(newNames))
  }

  override def prettify: TypeScheme = {
    var letter = -1
    var number = 0
    def nextName(): String = {
      if (letter >= 25) {
        letter = -1
        number += 1
      }
      letter += 1
      if (names.length > 26)
        ('a' + letter).toChar.toString + number
      else
        ('a' + letter).toChar.toString
    }
    val newNames = names.map(_ => TypeVar(nextName()))
    val s: Map[String, Monotype] = Map.empty ++ names.zip(newNames)
    TypeScheme(newNames.map(_.name), t.apply(s))
  }
}

case class TypeContext(constraints: Seq[Constraint], t: Monotype) extends Types {
  override def toString = if (constraints.isEmpty) t.toString else constraints.mkString(", ") + s" => $t"

  def ftv = constraints.flatMap(_.ftv).toSet ++ t.ftv

  def apply(sub: Map[String, Monotype]): TypeContext = TypeContext(constraints.map(_(sub)), t(sub))
}


sealed class Monotype(name: String = "") extends Type {
  override def toString = if (name.nonEmpty) name else super.toString

  def ftv = Set.empty[String]

  def apply(sub: Map[String, Monotype]): Monotype = this
  def unify(other: Monotype): Map[String, Monotype] = other match {
    case _ if other == this => Map.empty
    case TypeVar(name) => bind(name)
    case _ => throw new TypeError(s"could not match type '$this' with type '$other'", NoPosition)
  }
  def bind(name: String): Map[String, Monotype] =
    if (ftv.contains(name)) throw new TypeError(s"occurs check failed: cannot bind '$name' to '$this'", NoPosition)
    else Map(name -> this)

  def instantiate(newVar: String => TypeVar): Monotype = this
}

case class TypeVar(name: String) extends Monotype(name) {
  override val ftv = Set(name)
  override def apply(sub: Map[String, Monotype]): Monotype = sub.get(name) match {
    case Some(t) if t == this => this // TODO: throw new TypeError(s"substitution cycle ($t)", NoPosition)
    case Some(t) => t.apply(sub)
    case None => this
  }
  override def unify(other: Monotype): Map[String, Monotype] = other.bind(name)
  override def bind(otherName: String): Map[String, Monotype] =
    if (name == otherName) Map.empty
    else Map(otherName -> this)
}

case class AppliedType(function: Monotype, parameters: Monotype*) extends Monotype {
  override val ftv = function.ftv ++ parameters.flatMap(_.ftv).toSet

  private def parenthesize(t: Monotype): String = t match {
    case t: AppliedType => s"($t)"
    case t => t.toString
  }

  override def toString = function.toString match {
    case "[]" if parameters.length == 1 => "[" + parameters.head.toString + "]"
    case n if parameters.length == 2 && lex.parse(lex.operator, n).successful =>
      s"${parenthesize(parameters.head)} $n ${parameters.tail.head}"
    case _ => (function +: parameters).map(parenthesize).mkString(" ")
  }
  override def apply(sub: Map[String, Monotype]): Monotype = AppliedType(function(sub), parameters.map(_(sub)): _*)
  override def unify(other: Monotype): Map[String, Monotype] = other match {
    case AppliedType(function2, parameters2 @ _*) if parameters.length == parameters2.length =>
      parameters2.zip(parameters).foldRight(function.unify(function2)) {
        case ((t1, t2), sub) =>
          Monotype.compose(sub, t1.apply(sub).unify(t2.apply(sub)))
      }
    case _ => super.unify(other)
  }
}

object Monotype {
  val Type = new Monotype("Type")
  val Constraint = new Monotype("Constraint")
  val Int = new Monotype("Int")
  val Float = new Monotype("Float")
  val Bool = new Monotype("Bool")
  val String = new Monotype("String")

  private val listTag = new Monotype("[]")
  def List(element: Monotype): Monotype = AppliedType(listTag, element)

  private val functionTag = new Monotype("->")
  def Function(in: Monotype, out: Monotype): Monotype = AppliedType(functionTag, in, out)

  def dyadic(a: Monotype, b: Monotype, out: Monotype): Monotype = Function(a, Function(b, out))

  def compose(head: Map[String, Monotype], tail: Map[String, Monotype]*): Map[String, Monotype] =
    tail.foldRight(head)((s1, s2) => s1.mapValues(_.apply(s2)) ++ s2)
}