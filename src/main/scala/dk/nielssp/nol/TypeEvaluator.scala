package dk.nielssp.nol

import dk.nielssp.nol.ast._

import scala.collection.immutable.Stack
import scala.util.parsing.input.NoPosition

class TypeEvaluator {

  def apply(program: Program): TypedProgram = {
    ???
  }

  def apply(definitions: Seq[Definition], env: SymbolTable): (Seq[TypedDefinition], SymbolTable) = {
    val definitionMap = definitions.groupBy(_.name)
    definitionMap.foldLeft((List.empty[(Definition, Option[Expr])], env)) {
      case ((result, env2), (name, Seq(definition))) =>
        ((definition, None) :: result, env2)
      case ((result, env2), (name, group)) =>
        group.foldLeft((Option.empty[Declaration], Option.empty[Assignment])) {
          case ((decl, None), a: Assignment) => (decl, Some(a))
          case ((None, a), decl: Declaration) => (Some(decl), a)
          case ((Some(_), _), decl: Declaration) => throw new TypeError(s"multiple declarations for name '$name'", decl.pos)
          case (_, d) => throw new TypeError(s"multiple definitions for name '$name'", d.pos)
        } match {
          case (decl, Some(a)) => ((a, decl.map(_.t)) :: result, env2)
          case _ => throw new TypeError(s"missing definition for name '$name'", group.head.pos)
        }
    }
    ???
  }

  def apply(definition: Definition, env: SymbolTable): SymbolTable = definition match {
    case Assignment(name, value) => env.updated(name, apply(value, env))
    case Declaration(name, t) => apply(t, env) match {
      case t: Type => env.updated(name, t)
      case _ => throw new TypeError("expected type", t.pos)
    }
    case TypeClassDefinition(name, parameters, constraints, members) =>
      new TypeClass(name, parameters.length)
      env
    case InstanceDefinition(names, constraints, instance, members) =>
      apply(instance, env) match {
        case c: Constraint => env.updated(c, members.map {
          case Assignment(name, expr) => name -> apply(expr, env)
        }.toMap)
        case _ => throw new TypeError("expected constraint", instance.pos)
      }
  }

  def apply(expr: Expr, env: SymbolTable): Value = expr match {
    case LetExpr(assigns, body) =>
      var newScope: SymbolTable = env
      newScope = assigns.foldLeft(newScope){
        case (env, Assignment(name, value)) =>
          env.updated(name, LazyValue(() => apply(value, newScope)))
        case (env, _) => env
      }
      apply(body, newScope)
    case LambdaExpr(Nil, expr) =>
      apply(expr, env)
    case LambdaExpr(name :: names, expr) =>
      LambdaValue {
        case param =>
          apply(LambdaExpr(names, expr), env.updated(name, param))
      }
    case IfExpr(cond, ifTrue, ifFalse) => apply(cond, env) match {
      case BoolValue(true) => apply(ifTrue, env)
      case BoolValue(false) => apply(ifFalse, env)
      case _ => throw new TypeError("expected bool", cond.pos)
    }
    case InfixExpr(op, left, right) => apply(op, env) match {
      case LambdaValue(f) => f(apply(left, env)) match {
        case LambdaValue(g) =>
          try {
            g(apply(right, env))
          } catch {
            case e: DomainError if e.pos == NoPosition =>
              e.pos = op.pos
              throw e
          }
        case _ => throw new TypeError("expected dyadic function", op.pos)
      }
      case _ => throw new TypeError("expected function", op.pos)
    }
    case PrefixExpr(op, arg) => apply(op, env) match {
      case LambdaValue(f) =>
        try {
          f(apply(arg, env))
        } catch {
          case e: DomainError if e.pos == NoPosition =>
            e.pos = op.pos
            throw e
        }
      case _ => throw new TypeError("expected function", op.pos)
    }
    case ListExpr(elements) => ListValue(elements.map(apply(_, env)).toList)
    case TupleExpr(elements) => TupleValue(elements.map(apply(_, env)).toList)
    case RecordExpr(fields) => RecordValue(fields.mapValues(apply(_, env)))
    case GetExpr(record, field) => apply(record, env) match {
      case RecordValue(fields) if fields.contains(field) => fields(field)
      case RecordValue(_) => throw new TypeError(s"undefined field: $field", record.pos)
      case _ => throw new TypeError("expected a record", record.pos)
    }
    case SetExpr(record, assigns) => ???
    case NameNode(name) => env.get(name) match {
      case Some(value) => value
      case None => throw new NameError(s"undefined name: $name", expr.pos)
    }
    case StringNode(value) => StringValue(value)
    case IntNode(value) => IntValue(value)
    case FloatNode(value) => FloatValue(value)
    case PolytypeExpr(names, constraints, expr) =>
      val env2 = env.withValues(env.values ++ names.map(name => name -> new TypeVar(name)))
      val context = constraints.map {
        case constraint => apply(constraint, env2) match {
          case c: Constraint => c
          case _ => throw new TypeError("expected a constraint", constraint.pos)
        }
      }
      apply(expr, env2) match {
        case TypeScheme(names2, context2, t) => TypeScheme(names2 ++ names, context2 ++ context, t)
        case t: Monotype => TypeScheme(names, context.toSet, t)
        case _ => throw new TypeError("expected a type", expr.pos)
      }
    case TupleTypeExpr(elements) =>
      val (names, context, types) = elements.foldLeft((Set.empty[String], Set.empty[Constraint], Stack.empty[Monotype])) {
        case ((names, context, types), element) => apply(element, env) match {
          case TypeScheme(names2, context2, t) => (names ++ names2, context ++ context2, types :+ t)
          case t: Monotype => (names, context, types :+ t)
          case _ => throw new TypeError("expected a type", element.pos)
        }
      }
      TypeScheme(names, context, Monotype.Tuple(types: _*))
    case ListTypeExpr(element) =>
      apply(element, env) match {
        case TypeScheme(names, context, t) => TypeScheme(names, context, Monotype.List(t))
        case t: Monotype => Monotype.List(t)
        case _ => throw new TypeError("expected a type", element.pos)
      }
    case RecordTypeExpr(fields, more) =>
      val (names, context, types) = fields.foldLeft((more.toSet, Set.empty[Constraint], Map.empty[String, Monotype])) {
        case ((names, context, types), (name, field)) => apply(field, env) match {
          case TypeScheme(names2, context2, t) => (names ++ names2, context ++ context2, types.updated(name, t))
          case t: Monotype => (names, context, types.updated(name, t))
          case _ => throw new TypeError("expected a type", field.pos)
        }
      }
      TypeScheme(names, context, RecordType(types, more))
  }
}