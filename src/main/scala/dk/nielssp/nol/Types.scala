package dk.nielssp.nol

trait Types {
  def ftv: Set[String]
  def apply(s: Map[String, Type]): Types
}

case class TypeScheme(names: List[String], t: Type) extends Types {
  override def toString = if (names.isEmpty) t.toString else "forall " + names.mkString(", ") + s" . $t"
  override def ftv = t.ftv -- names
  override def apply(s: Map[String, Type]): TypeScheme = TypeScheme(names, t.apply(s -- names))

  def instantiate(newVar: String => TypeVar): Type = {
    val newNames = names.map(_ => newVar("t"))
    t.apply(Map.empty ++ names.zip(newNames))
  }

  def prettify: TypeScheme = {
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
    val s: Map[String, Type] = Map.empty ++ names.zip(newNames)
    TypeScheme(newNames.map(_.name), t.apply(s))
  }
}
case class TypeEnv(env: Map[String, TypeScheme]) extends Types {
  override def ftv = env.values.map(_.ftv).foldRight(Set.empty[String])(_.union(_))
  override def apply(s: Map[String, Type]): TypeEnv = TypeEnv(env.mapValues(_.apply(s)))
  def remove(name: String) = TypeEnv(env - name)
  def union(te: TypeEnv): TypeEnv = TypeEnv(env ++ te.env)
  def generalize(t: Type) = TypeScheme((t.ftv -- ftv).toList, t)
  def get(name: String) = env.get(name)
  def updated(name: String, t: TypeScheme) = TypeEnv(env.updated(name, t))
}