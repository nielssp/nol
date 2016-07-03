package dk.nielssp.nol

trait Types {
  def ftv: Set[String]
  def apply(s: Map[String, Monotype]): Types
}

case class TypeEnv(env: Map[String, Type]) extends Types {
  override def ftv = env.values.map(_.ftv).foldRight(Set.empty[String])(_.union(_))
  override def apply(s: Map[String, Monotype]): TypeEnv = TypeEnv(env.mapValues(_.apply(s)))
  def remove(name: String) = TypeEnv(env - name)
  def union(te: TypeEnv): TypeEnv = TypeEnv(env ++ te.env)
  def generalize(context: Set[Constraint], t: Monotype): Type =
    TypeScheme(context.flatMap(_.ftv) ++ t.ftv -- ftv, context, t)
  def get(name: String) = env.get(name)
  def updated(name: String, t: Type) = TypeEnv(env.updated(name, t))
}

object TypeEnv {
  val empty = TypeEnv(Map.empty)
}
