package dk.nielssp.nol

import java.io.{File, IOException}

import scala.collection.mutable
import scala.io.Source

class JsGenerator(moduleLoader: ModuleLoader) {
  type SymbolTable = Map[String, PartialFunction[Inlinable, String]]

  val modules = mutable.HashMap.empty[String, SymbolTable]

  val none: PartialFunction[Inlinable, String] = PartialFunction.empty

  sealed abstract class Inlinable
  case class InfixInlinable(left: String, right: String) extends Inlinable
  case class PrefixInlinable(arg: String) extends Inlinable

  def encode(name: String): String =
    name.getBytes.map {
      case c if c >= 'a'.toByte && c <= 'z'.toByte => c.toChar.toString
      case c if c >= 'A'.toByte && c <= 'Z'.toByte => c.toChar.toString
      case c if c >= '0'.toByte && c <= '9'.toByte => c.toChar.toString
      case c if c == '_'.toByte => "_"
      case c => f"$$$c%x"
    }.mkString


  def preamble(symbols: SymbolTable): String = {
    val prefix = PrefixInlinable("a")
    val infix = InfixInlinable("a", "b")
    symbols.map {
      case (name, inliner) if inliner.isDefinedAt(infix) =>
        s"var ${encode(name)} = function(a){return function(b){return ${inliner(infix)};};};"
      case (name, inliner) if inliner.isDefinedAt(prefix) =>
        s"var ${encode(name)} = function(a){return ${inliner(prefix)};};"
      case _ => ""
    }.mkString
  }

  def apply(program: Program): (String, SymbolTable) = {
    val out = new StringBuilder
    var symbolTable = program.imports.foldLeft(Map.empty[String, PartialFunction[Inlinable, String]]) {
      case (symbols, imp@Import(name)) =>
        try {
          val module = moduleLoader(name)
          symbols ++ modules.getOrElseUpdate(name, {
            apply(module.program) match {
              case (moduleOut, module) =>
                out ++= s"// load module: $name\n$moduleOut\n"
                module
            }
          })
        } catch {
          case e: ImportError =>
            e.pos = imp.pos
            throw e
        }
    }
    val exports = program.definitions.map(_.name -> none).toMap
    symbolTable ++= exports
    Definition.sort(program.definitions).foreach {
      case Definition(name, value) =>
        out ++= s"var ${encode(name)} = ${apply(value, symbolTable)};\n"
    }
    if (exports.contains("main")) {
      out ++= s"main(function (str) { return process.stdout.write(str); });"
    }
    (out.toString(), exports)
  }


  def apply(expr: Expr, scope: SymbolTable): String = expr match {
    case LetExpr(assigns, body) =>
      val newScope = scope ++ assigns.map(_.name -> none)
      val bindings = Definition.sort(assigns).map {
        case Definition(name, value) =>
          s"var ${encode(name)} = ${apply(value, newScope)};"
      }.mkString
      s"(function(){${bindings}return ${apply(body, newScope)};})()"
    case LambdaExpr(Nil, expr) =>
      apply(expr, scope)
    case LambdaExpr(name :: names, expr) =>
      s"(function(${encode(name)}){return " +
        apply(LambdaExpr(names, expr), scope + (name -> none)) +
        ";})"
    case IfExpr(cond, ifTrue, ifFalse) =>
      "(" + apply(cond, scope) +
        "?" + apply(ifTrue, scope) +
        ":" + apply(ifFalse, scope) +")"
    case InfixExpr(op, left, right) =>
      val a = apply(left, scope)
      val b = apply(right, scope)
      val inlinable = InfixInlinable(a, b)
      op match {
        case NameNode(name) if scope.contains(name) && scope(name).isDefinedAt(inlinable) =>
          scope(name)(inlinable)
        case _ =>
          apply(op, scope) + s"($a)($b)"
      }
    case PrefixExpr(op, arg) =>
      val a = apply(arg, scope)
      val inlinable = PrefixInlinable(a)
      op match {
        case NameNode(name) if scope.contains(name) && scope(name).isDefinedAt(inlinable) =>
          scope(name)(inlinable)
        case _ =>
          apply(op, scope) + s"($a)"
      }
    case ListExpr(elements) =>
      "[" + elements.map(apply(_, scope)).mkString(",") + "]"
    case TupleExpr(elements) =>
      "[" + elements.map(apply(_, scope)).mkString(",") + "]"
    case RecordExpr(fields) =>
      "{" + fields.map{ case (f, v) => apply(StringNode(f), scope) + ":" + apply(v, scope) }.mkString(",") + "}"
    case GetExpr(record, field) =>
      apply(record, scope) + "[" + apply(StringNode(field), scope) + "]"
    case NameNode(name) => scope.contains(name) match {
      case true => encode(name)
      case false => throw new NameError(s"undefined name: $name", expr.pos)
    }
    case StringNode(value) => s""""$value""""
    case IntNode(value) => s"$value"
    case FloatNode(value) => s"$value"
  }
}
