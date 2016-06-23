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

  def definition: Parser[Definition] = positioned(keyword("let") ~> assign)

  def importStmt: Parser[Import] = positioned(keyword("import") ~> acceptMatch("a string", {
    case StringToken(name) => Import(name)
  }))

  def typeScheme: Parser[PolytypeExpr] =
    opt(keyword("forall") ~> rep1sep(monadicName, punctuation(",")) <~ punctuation(".")) ~ constraints ~ typeInfix ^^ {
      case Some(names) ~ context ~ t => PolytypeExpr(names.map(_.name).toSet, context, t)
      case None ~ context ~ t => PolytypeExpr(Set.empty, context, t)
  }

  def constraints: Parser[Set[ConstraintExpr]] = opt(rep1sep(constraint, punctuation(",")) <~ operator("=>")) ^^ {
    case Some(constraints) => constraints.toSet
    case None => Set.empty
  }

  def constraint: Parser[ConstraintExpr] = monadicName ~ rep1(typeAtom) ^^ {
    case NameNode(typeClass) ~ parameters => ConstraintExpr(typeClass, parameters)
  }

  def typeInfix: Parser[MonotypeExpr] = typePrefix ~ dyadicName ~ typeInfix ^^ {
    case left ~ NameNode(op) ~ right => TypePrefixExpr(TypeNameNode(op), List(left, right))
  } | typePrefix

  def typePrefix: Parser[MonotypeExpr] = rep1(typeAtom) ^^ {
    case List(atom) => atom
    case op :: parameters => TypePrefixExpr(op, parameters)
  }

  def typeAtom: Parser[MonotypeExpr] =  positioned(
    tupleType |
    recordType |
    listType |
    monadicName ^^ (name => TypeNameNode(name.name))
  )

  def tupleType: Parser[MonotypeExpr] = punctuation("(") ~> repsep(typeInfix, punctuation(",")) <~ punctuation(")")  ^^ {
    case Nil => TypePrefixExpr(TypeNameNode("()"), List.empty)
    case x :: Nil => x
    case xs => TypePrefixExpr(TypeNameNode("()"), xs)
  }

  def listType: Parser[MonotypeExpr] =
    punctuation("[") ~> typeAtom <~ punctuation("]") ^^ {
      case t => TypePrefixExpr(TypeNameNode("[]"), List(t))
    }

  def recordType: Parser[MonotypeExpr] =
    (punctuation("{") ~> rep1sep(fieldType, punctuation(",")) <~ punctuation("}")) ~ opt(punctuation("∪") ~> monadicName) ^^ {
      case fields ~ name => RecordTypeExpr(fields.toMap, name.map(_.name))
    }

  def fieldType: Parser[(String, MonotypeExpr)] = monadicName ~ (punctuation(":") ~> typeInfix) ^^ {
    case NameNode(name) ~ value => name -> value
  }

  def replExpr: Parser[Expr] = phrase(expr)

  def expr: Parser[Expr] = positioned(letExpr | lambdaExpr | ifExpr)

  def letExpr: Parser[Expr] = keyword("let") ~> rep1sep(assign, punctuation(",")) ~ (keyword("in") ~> expr) ^^ {
    case assigns ~ body => LetExpr(assigns, body)
  }

  def assign: Parser[Definition] =
    (monadicName | dyadicName) ~ ((punctuation("=") ~> expr) | (punctuation(":") ~> typeScheme)) ^^ {
    case name ~ (value: Expr) => ValueDefinition(name.name, value)
    case name ~ (t: PolytypeExpr) => TypeDefinition(name.name, t)
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

  def apply(tokens: Seq[Token]): Program = program(TokenReader(tokens)) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) => throw new SyntaxError(s"unexpected ${next.first}, $msg", next.pos)
  }
}
