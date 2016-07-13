package dk.nielssp.nol

import dk.nielssp.nol.ast.{TypedAssignment, TypedTypeClassDefinition}

case class SymbolTable(values: Map[String, Value],
                       types: Map[String, Type],
                       typeClasses: Map[String, TypedTypeClassDefinition],
                       instances: Map[Constraint, Map[String, TypedAssignment]]) extends Types {
  def withValues(values: Map[String, Value]) = SymbolTable(values, types, typeClasses, instances)
  def withTypes(types: Map[String, Type]) = SymbolTable(values, types, typeClasses, instances)
  def withTypeClasses(typeClasses: Map[String, TypedTypeClassDefinition]) = SymbolTable(values, types, typeClasses, instances)
  def withInstances(instances: Map[Constraint, Map[String, TypedAssignment]]) = SymbolTable(values, types, typeClasses, instances)

  override def ftv = types.values.map(_.ftv).foldRight(Set.empty[String])(_.union(_))
  override def apply(s: Map[String, Monotype]): SymbolTable = withTypes(types.mapValues(_.apply(s)))

  def union(other: SymbolTable): SymbolTable =
    SymbolTable(values ++ other.values, types ++ other.types, typeClasses ++ other.typeClasses, instances ++ other.instances)
  def generalize(context: Set[Constraint], t: Monotype): Type =
    TypeScheme(context.flatMap(_.ftv) ++ t.ftv -- ftv, context, t)
  def get(name: String) = values.get(name)
  def getType(name: String) = types.get(name)
  def getTypeClass(name: String) = typeClasses.get(name)
  def updated(name: String, t: Type) = withTypes(types.updated(name, t))
  def updated(name: String, v: Value) = withValues(values.updated(name, v))
  def updated(name: String, typeClass: TypedTypeClassDefinition) = withTypeClasses(typeClasses.updated(name, typeClass))
  def updated(constraint: Constraint, instance: Map[String, TypedAssignment]) =
    withInstances(instances.updated(constraint, instance))
}

object SymbolTable {
  val empty = SymbolTable(Map.empty, Map.empty, Map.empty, Map.empty)

  def values(values: Map[String, Value]): SymbolTable =
    SymbolTable(values, Map.empty, Map.empty, Map.empty)
  def types(types: Map[String, Type]): SymbolTable =
    SymbolTable(Map.empty, types, Map.empty, Map.empty)
  def typeClasses(typeCLasses: Map[String, TypedTypeClassDefinition]): SymbolTable =
    SymbolTable(Map.empty, Map.empty, typeCLasses, Map.empty)
  def instances(instances: Map[Constraint, Map[String, TypedAssignment]]): SymbolTable =
    SymbolTable(Map.empty, Map.empty, Map.empty, instances)
}
