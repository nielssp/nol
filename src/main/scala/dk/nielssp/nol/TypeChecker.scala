package dk.nielssp.nol

import java.io.{File, IOException}

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.input.NoPosition

class TypeChecker {

  case class Module(symbols: TypeEnv, exports: TypeEnv)

  val globals = mutable.HashMap.empty[String, TypeScheme]

  val modules = mutable.HashMap.empty[String, Module]

  private var supply = 0

  def newTypeVar(prefix: String = "t"): TypeVar = {
    supply += 1
    TypeVar(prefix + supply)
  }

  def loadModule(name: String): Module = {
    val file = new File(name + ".nol")
    try {
      apply(TypeEnv(globals.toMap), parse(lex(Source.fromFile(file).mkString)))
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

  def apply(env: TypeEnv, program: Program): Module = {
    val imports = program.imports.foldLeft(env) {
      case (scope, imp@Import(name)) =>
        try {
          env.union(modules.getOrElseUpdate(name, {
            loadModule(name)
          }).exports)
        } catch {
          case e: ImportError =>
            e.pos = imp.pos
            throw e
        }
    }
    val exports = apply(imports, program.definitions)
    Module(imports.union(exports), exports)
  }

  def tryUnify(t1: Type, t2: Type, node: AstNode): Map[String, Type] =
    try {
      t1.unify(t2)
    } catch {
      case e: TypeError =>
        e.pos = node.pos
        throw e
    }

  def apply(env: TypeEnv, definitions: Seq[Definition]): TypeEnv = {
    val vars = definitions.map(_ -> newTypeVar())
    val env2 = TypeEnv(env.env ++ vars.map {
      case (Definition(name, _), v) => name -> TypeScheme(List.empty, v)
    })
    val inferred = vars.map {
      case (Definition(name, value), v) => apply(env2, value)
    }
    val subs1 = inferred.map{ case (s, t) => s }
    val s1 = Type.compose(subs1.head, subs1.tail: _*)
    val subs2 =  vars.zip(inferred).map {
      case ((d, v), (_, t)) => tryUnify(v.apply(s1), t, d)
    }
    val s2 = Type.compose(subs2.head, subs2.tail: _*)
    TypeEnv(env.env ++ vars.zip(inferred).map {
      case ((Definition(name, _), v), (_, t)) => name -> env2.generalize(t.apply(s2))
    })
  }

  def apply(env: TypeEnv, expr: Expr): (Map[String, Type], Type) =
    expr match {
      case LetExpr(assigns, body) => apply(apply(env, assigns), body)
      case LambdaExpr(Nil, expr) => apply(env, expr)
      case LambdaExpr(name :: names, expr) =>
        val v = newTypeVar()
        val env2 = env.updated(name, TypeScheme(List.empty, v))
        val (s1, t1) = apply(env2, LambdaExpr(names, expr))
        (s1, Type.Function(v.apply(s1), t1.apply(s1)))
      case IfExpr(cond, ifTrue, ifFalse) =>
        val (s1, t1) = apply(env, cond)
        val (s2, t2) = apply(env.apply(s1), ifTrue)
        val (s3, t3) = apply(env.apply(s2), ifFalse)
        val s4 = tryUnify(t1.apply(s3), Type.Bool, cond)
        val s5 = tryUnify(t2.apply(s4), t3.apply(s4), ifFalse)
        (Type.compose(s5, s4, s3, s2, s1), t3)
      case InfixExpr(op, left, right) =>
        val v = newTypeVar()
        val (s1, t1) = apply(env, op)
        val (s2, t2) = apply(env.apply(s1), left)
        val (s3, t3) = apply(env.apply(s2), right)
        val s4 = tryUnify(t1.apply(s3), Type.Function(t2, Type.Function(t3, v)), op)
        (Type.compose(s4, s3, s2, s1), v.apply(s4))
      case PrefixExpr(op, arg) =>
        val v = newTypeVar()
        val (s1, t1) = apply(env, op)
        val (s2, t2) = apply(env.apply(s1), arg)
        val s3 = tryUnify(t1.apply(s2), Type.Function(t2, v), arg)
        (Type.compose(s3, s2, s1), v.apply(s3))
      case ListExpr(Nil) => (Map.empty, Type.List(newTypeVar()))
      case ListExpr(head :: Nil) =>
        val (s1, t1) = apply(env, head)
        (s1, Type.List(t1))
      case ListExpr(head :: tail) =>
        val v = newTypeVar()
        val (s1, t1) = apply(env, head)
        val (s2, t2) = apply(env.apply(s1), ListExpr(tail))
        val s3 = tryUnify(t1.apply(s2), v, head)
        val s4 = tryUnify(t2.apply(s3), Type.List(v.apply(s3)), head)
        (Type.compose(s1, s2, s3, s4), t2)
      case NameNode(name) =>
        env.get(name) match {
          case Some(t) => (Map.empty, t.instantiate(newTypeVar))
          case None => throw new NameError(s"undefined name: $name", expr.pos)
        }
      case StringNode(value) =>
        (Map.empty, Type.String)
      case IntNode(value) =>
        (Map.empty, Type.Int)
      case FloatNode(value) =>
        (Map.empty, Type.Float)
    }
}
