package dk.nielssp.nol

import java.io.{File, IOException}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.parsing.input.NoPosition

class Module(val name: String, val program: Program) {

  def typeEnv: Map[String, Type] = program.definitions.flatMap {
    case Definition(name, value) => value.typeAnnotation.map(name -> _)
  }.toMap
}

object Module {

  def fromTypeEnv(name: String, typeEnv: Map[String, Type]): Module =
    new Module(name, Program(Seq.empty, typeEnv.map {
      case (name, t) =>
        val node = NameNode("undefined")
        node.typeAnnotation = Some(t)
        Definition(name, node)
    }.toSeq))
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
      val m = new Module(name, parse(lex(Source.fromFile(file).mkString)))
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