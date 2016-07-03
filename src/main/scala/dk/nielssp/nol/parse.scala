package dk.nielssp.nol

import dk.nielssp.nol.ast._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Reader}


object parse extends Parsers {
  type Elem = Token

  case class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    def atEnd = tokens.isEmpty
    def first = tokens.head
    def pos =
      if (atEnd)
        NoPosition
      else
        tokens.head.pos
    def rest = TokenReader(tokens.tail)
  }

  def program: Parser[Program] = phrase(rep(importStmt) ~ rep(definition)) ^^ {
    case imports ~ definitions => Program(imports, definitions)
  }

  def definition: Parser[Definition] = positioned(assignment | declaration | typeClassDefinition | instanceDefinition)

  def assignment: Parser[Assignment] = keyword("let") ~> assign

  def declaration: Parser[Declaration] = keyword("let") ~> (monadicName | dyadicName) ~ (punctuation(":") ~> typeScheme) ^^ {
    case name ~ t => Declaration(name.name, t)
  }

  def typeClassDefinition: Parser[TypeClassDefinition] =
    keyword("typeclass") ~> monadicName ~ rep1(monadicName) ~
      opt(keyword("extends") ~> rep1sep(constraint, punctuation(","))) ~
      opt(punctuation("{") ~> rep(declaration) <~ punctuation("}")) ^^ {
      case NameNode(name) ~ parameters ~ constraints ~ members =>
        TypeClassDefinition(name, parameters.map(_.name), constraints.getOrElse(List.empty), members.getOrElse(List.empty))
    }

  def instanceDefinition: Parser[InstanceDefinition] =
    keyword("instance") ~> opt(keyword("forall") ~> rep1sep(monadicName, punctuation(",")) <~ operator(".")) ~
      constraints ~ constraint ~
      opt(punctuation("{") ~> rep(assignment) <~ punctuation("}")) ^^ {
      case names ~ context ~ instance ~ members =>
        InstanceDefinition(names.getOrElse(List.empty).map(_.name).toSet, context, instance, members.getOrElse(List.empty))
    }

  def importStmt: Parser[Import] = positioned(keyword("import") ~> acceptMatch("a string", {
    case StringToken(name) => Import(name)
  }))

  def typeScheme: Parser[PolytypeExpr] =
    opt(keyword("forall") ~> rep1sep(monadicName, punctuation(",")) <~ operator(".")) ~ constraints ~ typeInfix ^^ {
      case Some(names) ~ context ~ t => PolytypeExpr(names.map(_.name).toSet, context, t)
      case None ~ context ~ t => PolytypeExpr(Set.empty, context, t)
  }

  def constraints: Parser[List[Expr]] = opt(rep1sep(constraint, punctuation(",")) <~ operator("=>")) ^^ {
    case Some(constraints) => constraints
    case None => List.empty
  }

  def constraint: Parser[Expr] = positioned(monadicName ~ rep1(typeAtom) ^^ {
    case typeClass ~ parameters => (typeClass :: parameters).reduceLeft(PrefixExpr)
  })

  def typeInfix: Parser[Expr] = typePrefix ~ dyadicName ~ typeInfix ^^ {
    case left ~ op ~ right => InfixExpr(op, left, right)
  } | typePrefix

  def typePrefix: Parser[Expr] = rep1(typeAtom) ^^ {
    case List(atom) => atom
    case atoms => atoms.reduceLeft(PrefixExpr)
  }

  def typeAtom: Parser[Expr] =  positioned(
    tupleType |
    recordType |
    listType |
    monadicName
  )

  def tupleType: Parser[Expr] = punctuation("(") ~> repsep(typeInfix, punctuation(",")) <~ punctuation(")")  ^^ {
    case Nil => TupleTypeExpr(List.empty)
    case x :: Nil => x
    case xs => TupleTypeExpr(xs)
  }

  def listType: Parser[Expr] =
    punctuation("[") ~> typeAtom <~ punctuation("]") ^^ {
      case t => ListTypeExpr(t)
    }

  def recordType: Parser[Expr] =
    (punctuation("{") ~> rep1sep(fieldType, punctuation(",")) <~ punctuation("}")) ~ opt(punctuation("∪") ~> monadicName) ^^ {
      case fields ~ name => RecordTypeExpr(fields.toMap, name.map(_.name))
    }

  def fieldType: Parser[(String, Expr)] = monadicName ~ (punctuation(":") ~> typeInfix) ^^ {
    case NameNode(name) ~ value => name -> value
  }

  def replExpr: Parser[Expr] = phrase(expr)

  def replType: Parser[PolytypeExpr] = phrase(typeScheme)

  def expr: Parser[Expr] = positioned(letExpr | lambdaExpr | ifExpr)

  def letExpr: Parser[Expr] = keyword("let") ~> rep1sep(assign, punctuation(",")) ~ (keyword("in") ~> expr) ^^ {
    case assigns ~ body => LetExpr(assigns, body)
  }

  def assign: Parser[Assignment] =
    ((monadicName | dyadicName) ~ (punctuation("=") ~> expr) |
      keyword("type") ~> (monadicName | dyadicName) ~ (punctuation("=") ~> typeScheme)) ^^ {
    case name ~ value => Assignment(name.name, value)
  }

  def ifExpr: Parser[Expr] = keyword("if") ~> expr ~ (keyword("then") ~> expr) ~ (keyword("else") ~> expr) ^^ {
    case cond ~ ifTrue ~ ifFalse => IfExpr(cond, ifTrue, ifFalse)
  }

  def lambdaExpr: Parser[Expr] = punctuation("\\") ~> rep1(monadicName | dyadicName) ~ (punctuation("->") ~> expr) ^^ {
    case names ~ expr => LambdaExpr(names.map(_.name), expr)
  } | infix

  def infix: Parser[Expr] = prefix ~ dyadicName ~ lambdaExpr ^^ {
    case left ~ op ~ right => InfixExpr(op, left, right)
  } | prefix

  def prefix: Parser[Expr] = rep1(recordAccess) ^^ {
    case List(atomic) => atomic
    case atomics => atomics.reduceLeft(PrefixExpr)
  }

  def recordAccess: Parser[Expr] =
    atomic ~ rep(punctuation("#") ~> monadicName) ^^ {
      case atom ~ Nil => atom
      case atom ~ (NameNode(name) :: names) => names.foldLeft(GetExpr(atom, name)) {
        case (left, NameNode(name)) => GetExpr(left, name)
      }
    }

  def atomic: Parser[Expr] = positioned(
      punctuation("(") ~> (partialInfix | tuple) <~ punctuation(")") |
      record |
      list |
      monadicName |
      literal
  )

  def tuple: Parser[Expr] = repsep(expr, punctuation(",")) ^^ {
    case Nil => TupleExpr(List.empty)
    case x :: Nil => x
    case xs => TupleExpr(xs)
  }

  def partialInfix: Parser[Expr] = dyadicName ~ opt(lambdaExpr) ^^ {
    case name ~ Some(expr) => LambdaExpr(List(" "), InfixExpr(name, NameNode(" "), expr))
    case name ~ None => name
  }

  def list: Parser[Expr] =
    punctuation("[") ~> rep(atomic) <~ punctuation("]") ^^ ListExpr

  def record: Parser[RecordExpr] =
    punctuation("{") ~> rep1sep(field, punctuation(",")) <~ punctuation("}") ^^ {
      case fields => RecordExpr(fields.toMap)
    }

  def field: Parser[(String, Expr)] = monadicName ~ (punctuation("=") ~> expr) ^^ {
    case NameNode(name) ~ value => name -> value
  }

  def dyadicName: Parser[NameNode] = positioned(acceptMatch(s"a name", {
    case InfixToken(name) => NameNode(name)
  }))

  def monadicName: Parser[NameNode] = positioned(acceptMatch(s"a name", {
    case NameToken(name) => NameNode(name)
  }))

  def literal: Parser[Expr] = acceptMatch(s"a literal", {
    case IntToken(num) => IntNode(num.toInt)
    case FloatToken(num) => FloatNode(num.toDouble)
    case StringToken(value) => StringNode(value)
  })

  def keyword(str: String): Parser[Unit] = acceptMatch(s"keyword '$str'", {
    case KeywordToken(keyword) if keyword == str => ()
  })

  def operator(str: String): Parser[Unit] = acceptMatch(s"operator '$str'", {
    case InfixToken(operator) if operator == str => ()
  })

  def punctuation(str: String): Parser[Unit] = acceptMatch(s"punctuation '$str'", {
    case PunctuationToken(punctuation) if punctuation == str => ()
  })

  def repl(tokens: Seq[Token]): AstNode = program(TokenReader(tokens)) match {
    case Success(result, _) => result
    case NoSuccess(_, _) => replExpr(TokenReader(tokens)) match {
      case Success(result, _) => result
      case NoSuccess(msg, next) => throw new SyntaxError(s"unexpected ${next.first}, $msg", next.pos)
    }
  }

  def kind(tokens: Seq[Token]): PolytypeExpr = replType(TokenReader(tokens)) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) => throw new SyntaxError(s"unexpected ${next.first}, $msg", next.pos)
  }

  def apply(tokens: Seq[Token]): Program = program(TokenReader(tokens)) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) => throw new SyntaxError(s"unexpected ${next.first}, $msg", next.pos)
  }
}
