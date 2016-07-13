package dk.nielssp.nol

import java.io.{File, IOException}

import dk.nielssp.nol.ast._

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.input.NoPosition

class TypeChecker extends (Module => Module) with TypeCheckers {


  def apply(module: Module): Module = {
    val internal = apply(module.program.definitions, module.internal)
    val definitions = module.program.definitions ++ module.program.definitions.flatMap {
      case TypeClassDefinition(_, _, _, members) => members
      case _ => Seq.empty
    }
    val external = module.external.withTypes(definitions.flatMap(d => internal.getType(d.name).map(d.name -> _)).toMap)
    module.withInternal(internal).withExternal(external)
  }

  def apply(definitions: Seq[Definition], env: SymbolTable): SymbolTable =
    if (definitions.isEmpty) {
      SymbolTable.empty
    } else {
      val grouped = Definition.group(definitions)
      println(s"decl groups: ${grouped.map(_.map(_.name).mkString(",")).mkString(";")}")
      grouped.foldLeft(env) {
        case (env, definitions) =>
          val (_, _, env2) = defApply(definitions, env)
          env2
      }
    }

  private def defApply(definitions: Seq[Definition], env: SymbolTable): (Map[String, Monotype], Set[Constraint], SymbolTable) = {
    val definitions2 = definitions.map(_ -> SymbolTable.empty) ++ definitions.flatMap {
      case TypeClassDefinition(name, parameters, _, members) =>
        members.map(_ -> SymbolTable.empty.withTypes(parameters.map(_ -> newTypeVar()).toMap))
      case _ => Seq.empty
    }
    val vars = definitions2.map(_ -> newTypeVar())
    val env2 = env.withTypes(env.types ++ vars.map {
      case ((definition, _), v) => definition.name -> TypeScheme(Set.empty, Set.empty, v)
    })
    val inferred = vars.map {
      case ((definition, ext), _) => apply(definition, env2.union(ext))
    }
    val subs1 = inferred.map{ case (s, context, t) => s }
    val s1 = Monotype.compose(subs1.head, subs1.tail: _*)
    val subs2 =  vars.zip(inferred).map {
      case (((d, _), v), (_, _, t)) => tryUnify(v.apply(s1), t.apply(s1), d)
    }
    val s2 = Monotype.compose(s1, subs2: _*)
    val context = inferred.flatMap { case (_, context, _) => context }.toSet
    val env3 = env.apply(s1)
    (s2, context, env.withTypes(env.types ++ vars.zip(inferred).map {
      case (((definition, ext), v), (_, context, t)) => definition.name -> env3.union(ext).generalize(context.map(_(s2)), t.apply(s2))
    }))
  }

  def apply(definition: Definition, env: SymbolTable): (Map[String, Monotype], Set[Constraint], Monotype) = definition match {
    case Assignment(_, value) => apply(value, env)
    case Declaration(_, t) =>
      val (s1, context1, t1) = apply(t, env)
      val s2 = tryUnify(t1, Monotype.Type, t)
      (Monotype.compose(s2, s1), context1, newTypeVar())
    case TypeClassDefinition(_, parameters, constraints, members) =>
      val vs = parameters.map(_ -> newTypeVar())
      val env2 = env.withTypes(env.types ++ vs)
      val (s1, context1) = constraints.foldLeft((Map.empty[String, Monotype], Set.empty[Constraint])) {
        case ((s2, context2), e) =>
          val (s3, context3, t3) = apply(e, env2)
          val s4 = tryUnify(t3, Monotype.Constraint, e)
          (Monotype.compose(s4, s3, s2), context2 ++ context3)
      }
      val (s5, context5) = members.foldLeft((s1, context1)) {
        case ((s6, context6), member) =>
          val (s7, context7, _) = apply(member, env2)
          (Monotype.compose(s7, s6), context6 ++ context7)
      }
      (s5, context5, vs.foldRight(Monotype.Constraint) {
        case ((_, v), t) => Monotype.Function(v, t)
      })
    case InstanceDefinition(names, constraints, instance, members) =>
      val (s1, context1, t1) = apply(instance, env)
      val s2 = tryUnify(t1, Monotype.Constraint, instance)
      val (s3, context3) = constraints.foldLeft((Monotype.compose(s2, s1), context1)) {
        case ((s4, context4), e) =>
          val (s5, context5, t5) = apply(e, env)
          val s6 = tryUnify(t5, Monotype.Constraint, e)
          (Monotype.compose(s6, s5, s4), context5 ++ context4)
      }
      val (s7, context7) = members.foldLeft((s3, context3)) {
        case ((s8, context8), member) =>
          val (s9, context9, _) = apply(member, env)
          (Monotype.compose(s9, s8), context9 ++ context8)
      }
      (s7, context7, t1)
  }

  def apply(expr: Expr, env: SymbolTable): (Map[String, Monotype], Set[Constraint], Monotype) = try {
    val (s: Map[String, Monotype], context: Set[Constraint], t) = expr match {
      case LetExpr(assigns, body) =>
        val (s3, context3, env3) = Definition.group(assigns).foldLeft((Map.empty[String, Monotype], Set.empty[Constraint], env)) {
          case ((s1, context1, env1), definitions) =>
            val (s2, context2, env2) = defApply(definitions, env1)
            (Monotype.compose(s2, s1), (context1 ++ context2).map(_(s2)), env2.apply(s2))
        }
        val (s4, context4, t1) = apply(body, env3)
        (Monotype.compose(s4, s3), context3 ++ context4, t1)
      case LambdaExpr(Nil, expr) => apply(expr, env)
      case LambdaExpr(name :: names, expr) =>
        val v = newTypeVar()
        val env2 = env.updated(name, TypeScheme(Set.empty, Set.empty, v))
        val (s1, context1, t1) = apply(LambdaExpr(names, expr), env2)
        (s1, context1, Monotype.Function(v.apply(s1), t1))
      case IfExpr(cond, ifTrue, ifFalse) =>
        val (s1, context1, t1) = apply(cond, env)
        val (s2, context2, t2) = apply(ifTrue, env.apply(s1))
        val s3 = Monotype.compose(s2, s1)
        val (s4, context3, t3) = apply(ifFalse, env.apply(s3))
        val s5 = Monotype.compose(s4, s3)
        val s6 = tryUnify(t1.apply(s5), Monotype.Bool, cond)
        val s7 = Monotype.compose(s6, s5)
        val s8 = tryUnify(t2.apply(s7), t3.apply(s7), ifFalse)
        (Monotype.compose(s8, s7), context1 ++ context2 ++ context3,  t3)
      case InfixExpr(op, left, right) =>
        val v = newTypeVar()
        val (s1, context1, t1) = apply(op, env)
        val (s2, context2, t2) = apply(left, env.apply(s1))
        val (s3, context3, t3) = apply(right, env.apply(s2))
        val s4 = tryUnify(t1.apply(s3), Monotype.Function(t2, Monotype.Function(t3, v)), op)
        (Monotype.compose(s4, s3, s2, s1), context1 ++ context2 ++ context3,  v.apply(s4))
      case PrefixExpr(op, arg) =>
        val v = newTypeVar()
        val (s1, context1, t1) = apply(op, env)
        val (s2, context2, t2) = apply(arg, env.apply(s1))
        val s3 = tryUnify(t1.apply(s2), Monotype.Function(t2, v), arg)
        (Monotype.compose(s3, s2, s1), context1 ++ context2, v.apply(s3))
      case ListExpr(Nil) => (Map.empty, Set.empty, Monotype.List(newTypeVar()))
      case ListExpr(head :: Nil) =>
        val (s1, context1, t1) = apply(head, env)
        (s1, context1, Monotype.List(t1))
      case ListExpr(head :: tail) =>
        val v = newTypeVar()
        val (s1, context1, t1) = apply(head, env)
        val (s2, context2, t2) = apply(ListExpr(tail), env.apply(s1))
        val s3 = tryUnify(t1.apply(s2), v, head)
        val s4 = tryUnify(t2.apply(s3), Monotype.List(v.apply(s3)), head)
        (Monotype.compose(s4, s3, s2, s1), context1 ++ context2, t2)
      case TupleExpr(Nil) => (Map.empty, Set.empty, Monotype.Tuple())
      case TupleExpr(elements) =>
        val inferred = elements.map(apply(_, env))
        val subs = inferred.map(_._1)
        (Monotype.compose(subs.head, subs.tail: _*), inferred.flatMap(_._2).toSet, Monotype.Tuple(inferred.map(_._3): _*))
      case RecordExpr(fields) =>
        val inferred = fields.map { case (field, t) => field -> apply(t, env) } // Warning: Not possible to use mapValues here because of some weird lazy map view bug
        val subs = inferred.values.map(_._1).toSeq
        (Monotype.compose(subs.head, subs.tail: _*), inferred.flatMap(_._2._2).toSet, RecordType(inferred.mapValues(_._3), None))
      case GetExpr(record, field) =>
        val v1 = newTypeVar()
        val v2 = newTypeVar()
        val (s1, context1, t1) = apply(record, env)
        val t2 = RecordType(Map(field -> v1), Some(v2.name))
        val s2 = tryUnify(t1, t2, expr)
        (Monotype.compose(s2, s1), context1, v1.apply(s2))
      case SetExpr(record, assigns) => ???
      case NameNode(name) =>
        env.getType(name) match {
          case Some(ts) =>
            val (context, t) = ts.instantiate(newTypeVar)
            (Map.empty, context, t)
          case None => throw new NameError(s"undefined name: $name", expr.pos)
        }
      case StringNode(value) =>
        (Map.empty, Set.empty, Monotype.String)
      case IntNode(value) =>
        (Map.empty, Set.empty, Monotype.Int)
      case FloatNode(value) =>
        val v = newTypeVar()
        (Map.empty, Set(Constraint(Monotype.Num, v)), v)
      case PolytypeExpr(names, constraints, expr) =>
        val vs = names.map(_ -> newTypeVar())
        val env2 = env.withTypes(env.types ++ vs)
        val (s1, context1, t1) = apply(expr, env2)
        val (s5, context5) = constraints.foldLeft((s1, context1)) {
          case ((s2, context2), e) =>
            val (s3, context3, t3) = apply(e, env2)
            val s4 = tryUnify(t3, Monotype.Constraint, e)
            (Monotype.compose(s4, s3, s2), context2 ++ context3)
        }
        val s6 = tryUnify(t1, Monotype.Type, expr)
        (Monotype.compose(s6, s5), context5, t1)
      case TupleTypeExpr(elements) =>
        val (s1, context1) = elements.foldLeft((Map.empty[String, Monotype], Set.empty[Constraint])) {
          case ((s2, context2), e) =>
            val (s3, context3, t3) = apply(e, env)
            val s4 = tryUnify(t3, Monotype.Type, e)
            (Monotype.compose(s4, s3, s2), context2 ++ context3)
        }
        (s1, context1, Monotype.Type)
      case ListTypeExpr(element) =>
        val (s1, context1, t1) = apply(element, env)
        val s2 = tryUnify(t1, Monotype.Type, element)
        (Monotype.compose(s2, s1), context1, Monotype.Type  )
      case RecordTypeExpr(fields, more) =>
        val (s1, context1) = more match {
          case Some(name) =>
            val (s2, context2, t2) = apply(NameNode(name), env)
            val s3 = tryUnify(t2, Monotype.Type, expr)
            (Monotype.compose(s3, s2), context2)
          case None => (Map.empty[String, Monotype], Set.empty[Constraint])
        }
        val (s4, context4) = fields.foldLeft((s1, context1)) {
          case ((s5, context5), (_, e)) =>
            val (s6, context6, t6) = apply(e, env)
            val s7 = tryUnify(t6, Monotype.Type, e)
            (Monotype.compose(s7, s6, s5), context5 ++ context6)
        }
        (s4, context4, Monotype.Type)
    }
    expr.typeAnnotation = Some(t)
    (s, context, t)
  } catch {
    case e: Error if e.pos == NoPosition =>
      e.pos = expr.pos
      throw e
  }
}
