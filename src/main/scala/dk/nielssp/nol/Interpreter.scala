package dk.nielssp.nol

import java.io.{File, IOException}

import dk.nielssp.nol.ast._

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.input.NoPosition

class Interpreter(moduleLoader: ModuleLoader) {

  type SymbolTable = Map[String, Value]

  val modules = mutable.HashMap.empty[String, SymbolTable]

  def apply(program: Program, scope: SymbolTable = Map.empty[String, Value]): SymbolTable = {
    var symbolTable = program.imports.foldLeft(scope) {
      case (scope, imp@Import(name)) =>
        try {
          scope ++ modules.getOrElseUpdate(name, {
            apply(moduleLoader(name).program)
          })
        } catch {
          case e: ImportError =>
            e.pos = imp.pos
            throw e
        }
    }
    val exports = program.definitions.foldLeft(Map.empty[String, Value]) {
      case (scope, Assignment(name, value)) =>
        scope.updated(name, LazyValue(() => apply(value, symbolTable)))
      case (scope, _) => scope
    }
    symbolTable = symbolTable ++ exports
    exports
  }

  def apply(expr: Expr, scope: SymbolTable): Value = (expr match {
    case LetExpr(assigns, body) =>
      var newScope: SymbolTable = scope
      newScope = assigns.foldLeft(newScope){
        case (scope, Assignment(name, value)) =>
          scope.updated(name, LazyValue(() => apply(value, newScope)))
        case (scope, _) => scope
      }
      apply(body, newScope)
    case LambdaExpr(Nil, expr) =>
      apply(expr, scope)
    case LambdaExpr(name :: names, expr) =>
      LambdaValue {
        case param =>
          apply(LambdaExpr(names, expr), scope.updated(name, param))
      }
    case IfExpr(cond, ifTrue, ifFalse) => apply(cond, scope) match {
      case BoolValue(true) => apply(ifTrue, scope)
      case BoolValue(false) => apply(ifFalse, scope)
      case _ => throw new TypeError("expected bool", cond.pos)
    }
    case InfixExpr(op, left, right) => apply(op, scope) match {
      case LambdaValue(f) => f(apply(left, scope)) match {
        case LambdaValue(g) =>
          try {
            g(apply(right, scope))
          } catch {
            case e: DomainError if e.pos == NoPosition =>
              e.pos = op.pos
              throw e
          }
        case _ => throw new TypeError("expected dyadic function", op.pos)
      }
      case _ => throw new TypeError("expected function", op.pos)
    }
    case PrefixExpr(op, arg) => apply(op, scope) match {
      case LambdaValue(f) =>
        try {
          f(apply(arg, scope))
        } catch {
          case e: DomainError if e.pos == NoPosition =>
            e.pos = op.pos
            throw e
        }
      case _ => throw new TypeError("expected function", op.pos)
    }
    case ListExpr(elements) => ListValue(elements.map(apply(_, scope)).toList)
    case TupleExpr(elements) => TupleValue(elements.map(apply(_, scope)).toList)
    case RecordExpr(fields) => RecordValue(fields.mapValues(apply(_, scope)))
    case GetExpr(record, field) => apply(record, scope) match {
      case RecordValue(fields) if fields.contains(field) => fields(field)
      case RecordValue(_) => throw new TypeError(s"undefined field: $field", record.pos)
      case _ => throw new TypeError("expected a record", record.pos)
    }
    case SetExpr(record, assigns) => ???
    case NameNode(name) => scope.get(name) match {
      case Some(value) => value
      case None => throw new NameError(s"undefined name: $name", expr.pos)
    }
    case StringNode(value) => StringValue(value)
    case IntNode(value) => IntValue(value)
    case FloatNode(value) => FloatValue(value)
  }) match {
    case LazyValue(value) => value()
    case value => value
  }
}
