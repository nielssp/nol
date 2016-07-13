package dk.nielssp.nol

import dk.nielssp.nol.ast._

import scala.collection.immutable.Stack
import scala.util.parsing.input.NoPosition

class TypeEvaluator extends (Module => Module) with Interpreters {
  def apply(module: Module): Module = {
    var internal = module.internal
    val external = module.program.definitions.foldLeft(module.external) {
      case (scope, Assignment(name, value)) =>
        scope.updated(name, LazyValue(() => apply(value, internal)))
      case (scope, TypeClassDefinition(name, parameters, _, _)) =>
        scope.updated(name, TypeClass(name, parameters.length))
      case (scope, _) => scope
    }
    internal = internal.union(external)
    val program = Program(module.program.imports, apply(module.program.definitions, internal))
    module.withProgram(program).withInternal(internal).withExternal(external)
  }

  def apply(definitions: Seq[Definition], env: SymbolTable): Seq[TypedDefinition] =
    definitions.groupBy(_.name).flatMap {
      case ("", group) =>
        group.map {
          case InstanceDefinition(names, constraints, instance, members) =>
            val env2 = env.withValues(env.values ++ names.map(name => name -> new TypeVar(name)))
            TypedInstanceDefinition(
              names, constraints.map(toConstraint(_, env2)), toConstraint(instance, env2), members.map {
                case Assignment(name, value) =>
                  TypedAssignment(name, value, TypeScheme(Set("t"), Set.empty, TypeVar("t")))
              }
            )
          case d => throw new NameError("expected instance definition", d.pos)
        }
      case (_, Seq(Assignment(name, value))) =>
        Seq(TypedAssignment(name, value, TypeScheme(Set("t"), Set.empty, TypeVar("t"))))
      case (_, Seq(TypeClassDefinition(name, parameters, constraints, members))) =>
        val env2 = env.withValues(env.values ++ parameters.map(name => name -> new TypeVar(name)))
        Seq(TypedTypeClassDefinition(
          env.get(name).get.asInstanceOf[TypeClass], parameters, constraints.map(toConstraint(_, env2)), members.map {
            case Declaration(name, expr) => apply(expr, env2) match {
              case t: Type => TypedDeclaration(name, t)
              case _ => throw new TypeError("expected a type", expr.pos)
            }
          }
        ))
      case (name, defs) =>
        val (decl, assign) = defs.foldLeft((Option.empty[Type], Option.empty[Expr])) {
          case ((None, assign), Declaration(_, expr)) => apply(expr, env) match {
            case t: Type => (Some(t), assign)
            case _ => throw new TypeError("expected a type", expr.pos)
          }
          case ((decl, None), Assignment(_, expr)) => (decl, Some(expr))
          case (_, d) => throw new NameError(s"multiple definitions of $name", d.pos)
        }
        assign match {
          case Some(expr) => Seq(TypedAssignment(name, expr, decl.getOrElse { TypeScheme(Set("t"), Set.empty, TypeVar("t")) }))
          case None => throw new NameError(s"missing definition of $name", defs.head.pos)
        }
    }.toSeq

  def toConstraint(expr: Expr, env: SymbolTable): Constraint = apply(expr, env) match {
    case c: Constraint => c
    case _ => throw new TypeError("expected a constraint", expr.pos)
  }
}
