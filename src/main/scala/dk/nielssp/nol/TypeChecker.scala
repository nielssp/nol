package dk.nielssp.nol

class TypeChecker {

  private var supply = 0

  def newTypeVar(prefix: String): TypeVar = {
    supply += 1
    TypeVar(prefix + supply)
  }

  def apply(env: TypeEnv, expr: Expr): (Map[String, Type], Type) = expr match {
    case LetExpr(assigns, body) =>
      ???
    case LambdaExpr(Nil, expr) => apply(env, expr)
    case LambdaExpr(name :: names, expr) =>
      val v = newTypeVar("a")
      val env2 = env.updated(name, TypeScheme(List.empty, v))
      val (s1, t1) = apply(env2, LambdaExpr(names, expr))
      (s1, Type.Function(v.apply(s1), t1))
    case IfExpr(cond, ifTrue, ifFalse) =>
      val (s1, t1) = apply(env, cond)
      val (s2, t2) = apply(env.apply(s1), ifTrue)
      val (s3, t3) = apply(env.apply(s2), ifFalse)
      val s4 = t1.apply(s3).unify(Type.Bool)
      val s5 = t2.apply(s4).unify(t3.apply(s4))
      (Type.compose(s5, s4, s3, s2, s1), t3)
    case InfixExpr(op, left, right) =>
      val v = newTypeVar("a")
      val (s1, t1) = apply(env, op)
      val (s2, t2) = apply(env.apply(s1), left)
      val (s3, t3) = apply(env.apply(s2), right)
      val s4 = t1.apply(s3).unify(Type.Function(t2, Type.Function(t3, v)))
      (Type.compose(s4, s3, s2, s1), v.apply(s4))
    case PrefixExpr(op, arg) =>
      val v = newTypeVar("a")
      val (s1, t1) = apply(env, op)
      val (s2, t2) = apply(env.apply(s1), arg)
      val s3 = t1.apply(s2).unify(Type.Function(t2, v))
      (Type.compose(s3, s2, s1), v.apply(s3))
    case ListExpr(Nil) => (Map.empty, Type.List(newTypeVar("a")))
    case ListExpr(head :: tail) =>
      val (s, t) = tail.foldRight(apply(env, head)) {
        case (e, (s1, t1)) =>
          val (s2, t2) = apply(env, e)
          val s3 = t1.apply(s2).unify(t2)
          (Type.compose(s1, s2, s3), t2)
      }
      (s, Type.List(t))
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
