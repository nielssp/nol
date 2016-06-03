package dk.nielssp.nol

import java.io.{File, IOException}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.parsing.input.NoPosition

class Module(val name: String) {

  var program: Option[Program] = None

  val symbols = new mutable.HashMap[String, Symbol]
}

class Symbol {
  var node: Option[Expr] = None
  var typeScheme: Option[TypeScheme] = None
  var value: Option[Value] = None
}

class ModuleLoader {
  val includePath = ListBuffer.empty[String]

  val modules = mutable.HashMap.empty[String, Module]

  def apply(name: String): Module = modules.getOrElse(name, load(name))

  def load(name: String): Module = {
    val file = includePath.map(path => new File(s"$path/$name.nol")).find(_.exists()).getOrElse {
      throw new ImportError(s"module not found: $name", NoPosition)
    }
    try {
      val m = new Module(name)
      m.program = Some(parse(lex(Source.fromFile(file).mkString)))
      modules(name) = m
      m
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
}