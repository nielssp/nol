package dk.nielssp.nol

import scala.util.parsing.input.NoPosition

trait Callable {
  def call(arg: Value): Value
}

sealed abstract class Value extends Types {
  override def ftv = Set.empty
  override def apply(s: Map[String, Monotype]): Value = this
  def call(args: Seq[Value]): Value = args.foldLeft(this) {
    case (f, arg) =>
      f match {
        case g: Value with Callable => g.call(arg)
        case _ => throw new TypeError("expected a function", NoPosition)
      }
  }
}
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
case class TupleValue(elements: List[Value]) extends Value {
  override def toString = s"(${elements.map(_.toString).mkString(", ")})"
}
case class RecordValue(fields: Map[String, Value]) extends Value {
  override def toString = s"{${fields.map{ case (f, v) => s"$f = $v" }.mkString(", ")}}"
}
case class LambdaValue(f: Value => Value) extends Value with Callable {
  override def toString = "#function"
  override def call(arg: Value): Value = f(arg)
}
case class TypeClass(name: String = "", parameters: Int = 1) extends Value with Callable {
  override def toString = name

  override def call(arg: Value): Value = {
    def convert(n: Int, parameters: List[Value]): Value =
      if (n < 1) Constraint(this, parameters.reverse: _*)
      else LambdaValue {
        case parameter => convert(n - 1, parameter :: parameters)
      }
    convert(parameters - 1, List(arg))
  }
}
case class LazyValue(value: () => Value) extends Value {
  override def toString = value().toString
}

sealed abstract class Type extends Value {
  override def apply(s: Map[String, Monotype]): Type = this

  def instantiate(newVar: String => TypeVar): (Set[Constraint], Monotype)

  def prettify: Type = this
}

case class TypeScheme(names: Set[String], context: Set[Constraint], t: Monotype) extends Type {
  override def toString =
    if (names.isEmpty) t.toString else "forall " + names.mkString(", ") + s". " +
      (if (context.isEmpty) t.toString else context.mkString(", ") + s" => $t")
  override def ftv = context.flatMap(_.ftv) ++ t.ftv -- names
  override def apply(s: Map[String, Monotype]): TypeScheme = {
    val s2 = s -- names
    TypeScheme(names, context.map(_(s2)), t.apply(s2))
  }

  override def instantiate(newVar: String => TypeVar): (Set[Constraint], Monotype) = {
    val newNames = names.map(_ => newVar("t"))
    val s = names.zip(newNames).toMap
    (context.map(_(s)), t.apply(s))
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
      if (names.size > 26)
        ('a' + letter).toChar.toString + number
      else
        ('a' + letter).toChar.toString
    }
    val newNames = names.map(_ => TypeVar(nextName()))
    val s: Map[String, Monotype] = Map.empty ++ names.zip(newNames)
    TypeScheme(newNames.map(_.name), context.map(_(s)), t.apply(s))
  }
}


sealed class Monotype(name: String = "") extends Type {
  override def toString = if (name.nonEmpty) name else super.toString

  override def ftv = Set.empty[String]

  override def apply(sub: Map[String, Monotype]): Monotype = this
  def unify(other: Monotype): Map[String, Monotype] = other match {
    case _ if other == this => Map.empty
    case TypeVar(name) => bind(name)
    case _ => throw new TypeError(s"could not match type '$this' with type '$other'", NoPosition)
  }
  def bind(name: String): Map[String, Monotype] =
    if (ftv.contains(name)) throw new TypeError(s"occurs check failed: cannot bind '$name' to '$this'", NoPosition)
    else Map(name -> this)

  def instantiate(newVar: String => TypeVar): (Set[Constraint], Monotype) = (Set.empty, this)
}

case class Constraint(typeClass: TypeClass, parameters: Value*) extends Value with Types {
  override def toString = s"$typeClass ${parameters.mkString(" ")}"

  override val ftv = parameters.flatMap(_.ftv).toSet

  override def apply(sub: Map[String, Monotype]): Constraint = Constraint(typeClass, parameters.map(_(sub)): _*)
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

case class RecordType(fields: Map[String, Monotype], more: Option[String]) extends Monotype {
  override val ftv = fields.values.flatMap(_.ftv).toSet ++ more.toSet

  override def toString =
    "{" + fields.map{ case (k, t) => s"$k : $t" }.mkString(", ") + "}" + more.map(n => s" ∪ $n").getOrElse("")

  override def apply(sub: Map[String, Monotype]): Monotype = more match {
    case Some(name) if sub.contains(name) => sub(name) match {
      case RecordType(fields2, more2) =>
        RecordType((fields ++ fields2).mapValues(_(sub)), more2)
      case TypeVar(name2) =>
        RecordType(fields.mapValues(_(sub)), Some(name2))
      case t => throw new TypeError(s"could not compute union of record '$this' and type '$t'", NoPosition)
    }
    case _ => RecordType(fields.mapValues(_(sub)), more)
  }

  override def unify(other: Monotype): Map[String, Monotype] = other match {
    case RecordType(_, Some(name2)) if more.isDefined => bind(name2) ++ other.bind(more.get)
    case RecordType(fields2, more2) if fields2.keySet == fields.keySet || more.isDefined || more2.isDefined=>
      val ext1: Map[String, Monotype] = more.map(_ -> RecordType(fields2 -- fields.keySet, None)).toMap
      val ext2: Map[String, Monotype] = more2.map(_ -> RecordType(fields -- fields2.keySet, None)).toMap
      fields.foldRight(ext1 ++ ext2) {
        case ((name, t1), sub) if fields2.contains(name) =>
          Monotype.compose(sub, t1.apply(sub).unify(fields2(name).apply(sub)))
        case (_, sub) => sub
      }
    case _ => super.unify(other)
  }
}

case class AppliedType(function: Monotype, parameters: Monotype*) extends Monotype {
  override val ftv = function.ftv ++ parameters.flatMap(_.ftv).toSet

  private def parenthesize(t: Monotype): String = t match {
    case t: AppliedType => s"($t)"
    case t => t.toString
  }

  override def toString = function.toString match {
    case "[]" if parameters.length == 1 => "[" + parameters.head.toString + "]"
    case "()" => "(" + parameters.mkString(", ") + ")"
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

  val Num = new TypeClass("Num")
  val Eq = new TypeClass("Eq")

  private val listTag = new Monotype("[]")
  def List(element: Monotype): Monotype = AppliedType(listTag, element)

  private val tupleTag = new Monotype("()")
  def Tuple(elements: Monotype*): Monotype = AppliedType(tupleTag, elements: _*)

  private val functionTag = new Monotype("->")
  def Function(in: Monotype, out: Monotype): Monotype = AppliedType(functionTag, in, out)

  def dyadic(a: Monotype, b: Monotype, out: Monotype): Monotype = Function(a, Function(b, out))

  def compose(head: Map[String, Monotype], tail: Map[String, Monotype]*): Map[String, Monotype] =
    tail.foldRight(head)((s1, s2) => s1.mapValues(_.apply(s2)) ++ s2)
}
