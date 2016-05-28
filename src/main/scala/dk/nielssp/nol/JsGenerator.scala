package dk.nielssp.nol

import java.io.{File, IOException}

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.input.NoPosition

class JsGenerator {
  type SymbolTable = Map[String, PartialFunction[Inlinable, String]]

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

  case class Module(symbols: SymbolTable, exports: SymbolTable)

  val modules = mutable.HashMap.empty[String, Module]

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

  def apply(program: Program, scope: SymbolTable): (String, Module) = {
    var symbolTable: SymbolTable = scope
    val out = new StringBuilder
    symbolTable ++= program.imports.foldLeft(symbolTable) {
      case (scope, imp@Import(name)) =>
        scope ++ modules.getOrElseUpdate(name, {
          val file = new File(name + ".nol")
          try {
            apply(parse(lex(Source.fromFile(file).mkString)), scope) match {
              case (moduleOut, module) =>
                out ++= s"// begin module: $name\n$moduleOut// end module: $name\n"
                module
            }
          } catch {
            case e: Error =>
              if (e.file == null)
                e.file = file
              throw e
            case e: IOException =>
              throw new ImportError(s"module not found: $name", imp.pos)
          }
        }).exports
    }
    val exports = program.definitions.map(_.name -> none).toMap
    symbolTable ++= exports
    program.definitions.foreach {
      case Definition(name, value) =>
        out ++= s"var ${encode(name)} = ${apply(value, symbolTable)};\n"
    }
    (out.toString(), Module(symbolTable, exports))
  }


  def apply(expr: Expr, scope: SymbolTable): String = expr match {
    case LetExpr(assigns, body) =>
      val names = assigns.map(_.name)
      val signature = "(" + names.map(encode).mkString(",") + ")"
      val newScope = scope ++ names.map(_ -> none)
      val bindings = assigns.map {
        case Definition(name, value) =>
          s"var ${encode(name)} = function$signature{return ${apply(value, newScope)};};"
      }.mkString
      val bind = names.map {
        case name =>
          s"${encode(name)}$signature"
      }.mkString(",")
      s"(function(){${bindings}return (function$signature{return ${apply(body, newScope)};})($bind);})()"
    case LambdaExpr(Nil, expr) =>
      apply(expr, scope)
    case LambdaExpr(name :: names, expr) =>
      s"(function(${encode(name)}){return " +
        apply(LambdaExpr(names, expr), scope + (name -> none)) +
        ";})"
    case IfExpr(cond, ifTrue, ifFalse) =>
      "(function(){if(" + apply(cond, scope) +
        "){return " + apply(ifTrue, scope) +
        ";}else{return " + apply(ifFalse, scope) +";}})()"
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
    case NameNode(name) => scope.contains(name) match {
      case true => encode(name)
      case false => throw new NameError(s"undefined name: $name", expr.pos)
    }
    case StringNode(value) => s""""$value""""
    case IntNode(value) => s"$value"
    case FloatNode(value) => s"$value"
  }
}
