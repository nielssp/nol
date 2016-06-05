package dk.nielssp.nol

import java.io.{File, IOException}

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.input.NoPosition

class TypeChecker(moduleLoader: ModuleLoader) {

  private var supply = 0

  val modules = mutable.HashMap.empty[String, TypeEnv]

  def newTypeVar(prefix: String = "t"): TypeVar = {
    supply += 1
    TypeVar(prefix + supply)
  }

  def apply(program: Program, env: TypeEnv): TypeEnv = {
    val imports = program.imports.foldLeft(env) {
      case (scope, imp@Import(name)) =>
        try {
          val module = moduleLoader(name)
          env.union(modules.getOrElseUpdate(name, {
            apply(module.program, env)
          }))
        } catch {
          case e: ImportError =>
            e.pos = imp.pos
            throw e
        }
    }
    val env2 = apply(program.definitions, imports)
    TypeEnv(program.definitions.flatMap(d => env2.get(d.name).map(d.name -> _)).toMap)
  }

  def tryUnify(t1: Monotype, t2: Monotype, node: AstNode): Map[String, Monotype] =
    try {
      t1.unify(t2)
    } catch {
      case e: TypeError =>
        e.pos = node.pos
        throw e
    }

  def apply(definitions: Seq[Definition], env: TypeEnv): TypeEnv =
    if (definitions.isEmpty) {
      TypeEnv(Map.empty)
    } else {
      val grouped = Definition.group(definitions)
      println(s"decl groups: ${grouped.map(_.map(_.name).mkString(",")).mkString(";")}")
      Definition.group(definitions).foldLeft(env) {
        case (env, definitions) =>
          val vars = definitions.map(_ -> newTypeVar())
          val env2 = TypeEnv(env.env ++ vars.map {
            case (Definition(name, _), v) => name -> TypeScheme(List.empty, v)
          })
          val inferred = vars.map {
            case (Definition(name, value), v) => apply(value, env2)
          }
          val subs1 = inferred.map{ case (s, t) => s }
          val s1 = Monotype.compose(subs1.head, subs1.tail: _*)
          val subs2 =  vars.zip(inferred).map {
            case ((d, v), (_, t)) => tryUnify(v.apply(s1), t.apply(s1), d)
          }
          val s2 = Monotype.compose(s1, subs2: _*)
          TypeEnv(env.env ++ vars.zip(inferred).map {
            case ((Definition(name, _), v), (_, t)) => name -> env2.generalize(t.apply(s2))
          })
      }
    }

  def apply(expr: Expr, env: TypeEnv): (Map[String, Monotype], Monotype) = {
    val (s: Map[String, Monotype], t) = expr match {
      case LetExpr(assigns, body) => apply(body, apply(assigns, env))
      case LambdaExpr(Nil, expr) => apply(expr, env)
      case LambdaExpr(name :: names, expr) =>
        val v = newTypeVar()
        val env2 = env.updated(name, TypeScheme(List.empty, v))
        val (s1, t1) = apply(LambdaExpr(names, expr), env2)
        (s1, Monotype.Function(v.apply(s1), t1.apply(s1)))
      case IfExpr(cond, ifTrue, ifFalse) =>
        val (s1, t1) = apply(cond, env)
        val (s2, t2) = apply(ifTrue, env.apply(s1))
        val (s3, t3) = apply(ifFalse, env.apply(s2))
        val s4 = tryUnify(t1.apply(s3), Monotype.Bool, cond)
        val s5 = tryUnify(t2.apply(s4), t3.apply(s4), ifFalse)
        (Monotype.compose(s5, s4, s3, s2, s1), t3)
      case InfixExpr(op, left, right) =>
        val v = newTypeVar()
        val (s1, t1) = apply(op, env)
        val (s2, t2) = apply(left, env.apply(s1))
        val (s3, t3) = apply(right, env.apply(s2))
        val s4 = tryUnify(t1.apply(s3), Monotype.Function(t2, Monotype.Function(t3, v)), op)
        (Monotype.compose(s4, s3, s2, s1), v.apply(s4))
      case PrefixExpr(op, arg) =>
        val v = newTypeVar()
        val (s1, t1) = apply(op, env)
        val (s2, t2) = apply(arg, env.apply(s1))
        val s3 = tryUnify(t1.apply(s2), Monotype.Function(t2, v), arg)
        (Monotype.compose(s3, s2, s1), v.apply(s3))
      case ListExpr(Nil) => (Map.empty, Monotype.List(newTypeVar()))
      case ListExpr(head :: Nil) =>
        val (s1, t1) = apply(head, env)
        (s1, Monotype.List(t1))
      case ListExpr(head :: tail) =>
        val v = newTypeVar()
        val (s1, t1) = apply(head, env)
        val (s2, t2) = apply(ListExpr(tail), env.apply(s1))
        val s3 = tryUnify(t1.apply(s2), v, head)
        val s4 = tryUnify(t2.apply(s3), Monotype.List(v.apply(s3)), head)
        (Monotype.compose(s4, s3, s2, s1), t2)
      case NameNode(name) =>
        env.get(name) match {
          case Some(t) => (Map.empty, t.instantiate(newTypeVar))
          case None => throw new NameError(s"undefined name: $name", expr.pos)
        }
      case StringNode(value) =>
        (Map.empty, Monotype.String)
      case IntNode(value) =>
        (Map.empty, Monotype.Int)
      case FloatNode(value) =>
        (Map.empty, Monotype.Float)
    }
    expr.typeAnnotation = Some(t)
    (s, t)
  }
}
