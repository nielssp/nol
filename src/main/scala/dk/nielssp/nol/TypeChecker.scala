package dk.nielssp.nol

class TypeChecker {

  private var supply = 0

  def newTypeVar(prefix: String = "t"): TypeVar = {
    supply += 1
    TypeVar(prefix + supply)
  }

  def apply(env: TypeEnv, expr: Expr): (Map[String, Type], Type) = expr match {
    case LetExpr(assigns, body) =>
      val vars = assigns.map(_ -> newTypeVar())
      val env2 = TypeEnv(env.env ++ vars.map {
        case (Definition(name, _), v) => name -> TypeScheme(List.empty, v)
      })
      val inferred = vars.map {
        case (Definition(name, value), v) => apply(env2, value)
      }
      val subs1 = inferred.map{ case (s, t) => s }
      val s1 = Type.compose(subs1.head, subs1.tail: _*)
      val subs2 =  vars.zip(inferred).map {
        case ((_, v), (_, t)) => v.apply(s1).unify(t)
      }
      val s2 = Type.compose(subs2.head, subs2.tail: _*)
      val env3 = TypeEnv(env.env ++ vars.zip(inferred).map {
        case ((Definition(name, _), v), (_, t)) => name -> env2.generalize(t.apply(s2))
      })
      val (s3, t3) = apply(env3, body)
      (Type.compose(s2, s3), t3)
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
      val s4 = t1.apply(s3).unify(Type.Bool)
      val s5 = t2.apply(s4).unify(t3.apply(s4))
      (Type.compose(s5, s4, s3, s2, s1), t3)
    case InfixExpr(op, left, right) =>
      val v = newTypeVar()
      val (s1, t1) = apply(env, op)
      val (s2, t2) = apply(env.apply(s1), left)
      val (s3, t3) = apply(env.apply(s2), right)
      val s4 = t1.apply(s3).unify(Type.Function(t2, Type.Function(t3, v)))
      (Type.compose(s4, s3, s2, s1), v.apply(s4))
    case PrefixExpr(op, arg) =>
      val v = newTypeVar()
      val (s1, t1) = apply(env, op)
      val (s2, t2) = apply(env.apply(s1), arg)
      val s3 = t1.apply(s2).unify(Type.Function(t2, v))
      (Type.compose(s3, s2, s1), v.apply(s3))
    case ListExpr(Nil) => (Map.empty, Type.List(newTypeVar()))
    case ListExpr(head :: tail) =>
      val v = newTypeVar()
      val (s1, t1) = apply(env, head)
      val (s2, t2) = apply(env.apply(s1), ListExpr(tail))
      val s3 = t1.apply(s2).unify(v)
      val s4 = t2.apply(s3).unify(Type.List(v.apply(s3)))
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
