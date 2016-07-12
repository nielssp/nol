package dk.nielssp.nol

import java.io.{File, IOException}

import dk.nielssp.nol.ast._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.parsing.input.NoPosition

class Module(val name: String, val program: Program, val internal: SymbolTable, val external: SymbolTable) {
  def withProgram(program: Program): Module = new Module(name, program, internal, external)
  def withInternal(internal: SymbolTable): Module = new Module(name, program, internal, external)
  def withExternal(external: SymbolTable): Module = new Module(name, program, internal, external)
}

class Importer(moduleLoader: ModuleLoader) extends (Module => Module) {
  def apply(module: Module): Module = {
    val internal = module.program.imports.foldLeft(module.internal) {
      case (env, imp@Import(name)) =>
        try {
          env.union(moduleLoader(name).external)
        } catch {
          case e: ImportError =>
            e.pos = imp.pos
            throw e
        }
    }
    module.withInternal(internal)
  }
}

class ModuleLoader {
  val includePath = ListBuffer.empty[String]

  val modules = mutable.HashMap.empty[String, Module]

  val phases = ListBuffer[Module => Module](new Importer(this))

  def compile(module: Module): Module = phases.foldLeft(module) {
    case (module, phase) =>
      println(s"${module.name}: Running phase: ${phase.getClass.getSimpleName}")
      phase(module)
  }

  def apply(name: String): Module = modules.getOrElse(name, { load(name) })

  def load(name: String): Module = {
    val file = includePath.map(path => new File(s"$path/$name.nol")).find(_.exists()).getOrElse {
      throw new ImportError(s"module not found: $name", NoPosition)
    }
    try {
      val ast = parse(lex(Source.fromFile(file).mkString))
      val m1 = new Module(name, if (ast.imports.exists(_.name == "std")) {
        ast
      } else {
        Program(Import("std") +: ast.imports, ast.definitions)
      }, SymbolTable.empty, SymbolTable.empty)
      val m2 = compile(m1)
      modules(name) = m2
      m2
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