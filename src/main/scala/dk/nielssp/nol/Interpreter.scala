package dk.nielssp.nol

import java.io.{File, IOException}

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.input.NoPosition

class Interpreter {

  type SymbolTable = Map[String, Value]

  case class Module(symbols: SymbolTable, exports: SymbolTable)

  val globals = mutable.HashMap.empty[String, Value]

  val modules = mutable.HashMap.empty[String, Module]

  def refreshImports(): Unit = {
    val names = modules.keySet
    modules.clear()
    modules ++= names.map(name => name -> loadModule(name))
  }

  def loadModule(name: String): Module = {
    val file = new File(name + ".nol")
    try {
      apply(parse(lex(Source.fromFile(file).mkString)), globals.toMap)
    } catch {
      case e: Error =>
        if (e.file == null) {
          e.file = file
        }
        throw e
      case e: IOException =>
        throw new ImportError(s"module not found: $name", NoPosition)
    }
  }

  def apply(program: Program, scope: SymbolTable): Module = {
    var symbolTable: SymbolTable = scope
    symbolTable = program.imports.foldLeft(symbolTable) {
      case (scope, imp@Import(name)) =>
        try {
          scope ++ modules.getOrElseUpdate(name, {
            loadModule(name)
          }).exports
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
    symbolTable ++= exports
    Module(symbolTable, exports)
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
