package dk.nielssp.nol

import dk.nielssp.nol.ast.{TypedAssignment, TypedDeclaration, TypedInstanceDefinition, TypedTypeClassDefinition}

class DeclarationChecker extends (Module => Module) with TypeCheckers {

  def apply(module: Module): Module = {
    val external = module.program.definitions.foldLeft(module.external) {
      case (env, TypedAssignment(name, expr, t)) =>
        val (context1, t1) = expr.typeAnnotation.get.instantiate(newTypeVar)
        val (context2, t2) = t.instantiate(newTypeVar)
        val s1 = tryUnify(t1, t2, expr)
        val context = context1 ++ context2
        val scheme = module.internal.generalize(context.map(_(s1)), t1.apply(s1)).prettify
        println(s"$name : $scheme")
        env.updated(name, scheme)
      case (env, tc@TypedTypeClassDefinition(typeClass, parameters, constraints, members)) =>
        val context1 =
          Set(Constraint(typeClass, parameters.map(TypeVar): _*)) ++ constraints
        env.withTypes(env.types ++ members.map {
          case TypedDeclaration(name, t1) =>
            val t2 = t1 match {
              case TypeScheme(names, context2, t) => TypeScheme(names ++ parameters, context1 ++ context2, t)
              case t: Monotype => TypeScheme(parameters.toSet, context1, t)
            }
            println(s"$name : $t2")
            name -> t2
        }).updated(typeClass.name, tc)
      case (env, _) => env
    }
    val internal = module.internal.union(external)
    val instances = module.program.definitions.flatMap {
      case d@TypedInstanceDefinition(names, context1, instance, members1) =>
        // TODO: bind parameter somehow
        internal.getTypeClass(instance.typeClass.name) match {
          case Some(TypedTypeClassDefinition(typeClass, parameters, context2, members2)) =>
            if (instance.parameters.length != parameters.length)
              throw new TypeError(s"incorrect number of parameters in implementation of '${typeClass.name}'", d.pos)
            val s0 = parameters.zip(instance.parameters).toMap
            val members3 = members2.map {
              case TypedDeclaration(name, t1) =>
                members1.find(_.name == name) match {
                  case Some(a@TypedAssignment(_, expr, t2)) =>
                    val (context3, t3) = expr.typeAnnotation.get.apply(s0).instantiate(newTypeVar)
                    val (context4, t4) = t2.apply(s0).instantiate(newTypeVar)
                    val (context5, t5) = t1.apply(s0).instantiate(newTypeVar)
                    val s1 = tryUnify(t4, t3, expr)
                    val s2 = tryUnify(t5, t3.apply(s1), expr)
                    val s3 = Monotype.compose(s2, s1)
                    val context = context3 ++ context4 ++ context5
                    val scheme = module.internal.generalize(context.map(_(s3)), t3.apply(s3)).prettify
                    expr.typeAnnotation = Some(scheme)
                    name -> TypedAssignment(name, expr, scheme)
                  case None =>
                    throw new TypeError(s"missing implementation for method '$name'", d.pos)
                }
            }
            Seq(instance -> members3.toMap)
          case None =>
            throw new NameError(s"undefined type class: '${instance.typeClass.name}'", d.pos)
        }
      case _ => Seq.empty
    }
    val external2 = external.withInstances(external.instances ++ instances)
    module.withInternal(internal.union(external2)).withExternal(external2)
  }
}
