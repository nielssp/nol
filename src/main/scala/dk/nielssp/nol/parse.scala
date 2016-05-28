package dk.nielssp.nol

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

  def program: Parser[Program] = phrase(rep(statement)) ^^ Program

  def statement: Parser[Statement] = positioned(definition | importStmt)

  def definition: Parser[Definition] = keyword("let") ~> assign

  def importStmt: Parser[Import] = keyword("import") ~> acceptMatch("a string", {
    case StringToken(name) => Import(name)
  })

  def replExpr: Parser[Expr] = phrase(expr)

  def expr: Parser[Expr] = positioned(letExpr | lambdaExpr | ifExpr)

  def letExpr: Parser[Expr] = keyword("let") ~> rep1sep(assign, punctuation(",")) ~ (keyword("in") ~> expr) ^^ {
    case assigns ~ body => LetExpr(assigns, body)
  }

  def assign: Parser[Definition] = (monadicName | dyadicName) ~ (punctuation("=") ~> expr) ^^ {
    case name ~ value => Definition(name.name, value)
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

  def prefix: Parser[Expr] = rep1(atomic) ^^ {
    case List(atomic) => atomic
    case atomics => atomics.reduceLeft(PrefixExpr)
  }

  def atomic: Parser[Expr] = positioned(
      punctuation("(") ~> (partialInfix | expr) <~ punctuation(")") |
      list |
      monadicName |
      literal
  )

  def partialInfix: Parser[Expr] = dyadicName ~ opt(lambdaExpr) ^^ {
    case name ~ Some(expr) => LambdaExpr(Seq(" "), InfixExpr(name, NameNode(" "), expr))
    case name ~ None => name
  }

  def list: Parser[Expr] =
    punctuation("[") ~> rep(atomic) <~ punctuation("]") ^^ ListExpr

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

  def apply(tokens: Seq[Token]): AstNode = program(TokenReader(tokens)) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) => throw new SyntaxError(s"unexpected ${next.first}, $msg", next.pos)
  }
}
