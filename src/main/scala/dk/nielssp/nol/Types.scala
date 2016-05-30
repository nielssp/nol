package dk.nielssp.nol

trait Types {
  def ftv: Set[String]
  def apply(s: Map[String, Type]): Types
}

case class TypeScheme(names: List[String], t: Type) extends Types {
  override def ftv = t.ftv -- names
  override def apply(s: Map[String, Type]): TypeScheme = TypeScheme(names, t.apply(s -- names))

  def instantiate(newVar: String => TypeVar): Type = {
    val newNames = names.map(_ => newVar("a"))
    t.apply(Map.empty ++ names.zip(newNames))
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