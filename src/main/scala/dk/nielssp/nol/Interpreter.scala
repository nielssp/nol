package dk.nielssp.nol

import java.io.{File, IOException}

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.input.NoPosition

class Interpreter(moduleLoader: ModuleLoader) {

  type SymbolTable = Map[String, Value]

  val modules = mutable.HashMap.empty[String, SymbolTable]

  def apply(program: Program): SymbolTable = {
    var symbolTable = program.imports.foldLeft(Map.empty[String, Value]) {
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
      case (scope, Definition(name, value)) =>
        scope.updated(name, LazyValue(() => apply(value, symbolTable)))
    }
    symbolTable = symbolTable ++ exports
    exports
  }

  def apply(expr: Expr, scope: SymbolTable): Value = (expr match {
    case LetExpr(assigns, body) =>
      var newScope: SymbolTable = scope
      newScope = assigns.foldLeft(newScope){
        case (scope, Definition(name, value)) =>
          scope.updated(name, LazyValue(() => apply(value, newScope)))
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
