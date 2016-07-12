package dk.nielssp.nol

case class SymbolTable(values: Map[String, Value],
                       types: Map[String, Type],
                       instances: Map[Constraint, Map[String, Value]]) extends Types {
  def withValues(values: Map[String, Value]) = SymbolTable(values, types, instances)
  def withTypes(types: Map[String, Type]) = SymbolTable(values, types, instances)
  def withInstances(instances: Map[Constraint, Map[String, Value]]) = SymbolTable(values, types, instances)

  override def ftv = types.values.map(_.ftv).foldRight(Set.empty[String])(_.union(_))
  override def apply(s: Map[String, Monotype]): SymbolTable = withTypes(types.mapValues(_.apply(s)))

  def union(other: SymbolTable): SymbolTable =
    SymbolTable(values ++ other.values, types ++ other.types, instances ++ other.instances)
  def generalize(context: Set[Constraint], t: Monotype): Type =
    TypeScheme(context.flatMap(_.ftv) ++ t.ftv -- ftv, context, t)
  def get(name: String) = values.get(name)
  def getType(name: String) = types.get(name)
  def updated(name: String, t: Type) = withTypes(types.updated(name, t))
  def updated(name: String, v: Value) = withValues(values.updated(name, v))
  def updated(constraint: Constraint, instance: Map[String, Value]) =
    withInstances(instances.updated(constraint, instance))
}

object SymbolTable {
  val empty = SymbolTable(Map.empty, Map.empty, Map.empty)

  def values(values: Map[String, Value]): SymbolTable = SymbolTable(values, Map.empty, Map.empty)
  def types(types: Map[String, Type]): SymbolTable = SymbolTable(Map.empty, types, Map.empty)
  def instances(instances: Map[Constraint, Map[String, Value]]): SymbolTable = SymbolTable(Map.empty, Map.empty, instances)
}
