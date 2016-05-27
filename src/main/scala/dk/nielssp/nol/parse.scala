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

  def expr: Parser[Expr] = letExpr | lambdaExpr | ifExpr | infix

  def letExpr: Parser[Expr] = keyword("let") ~> rep1sep(assign, operator(",")) ~ (keyword("in") ~> expr) ^^ {
    case assigns ~ body => LetExpr(assigns, body)
  }

  def assign: Parser[Assign] = name ~ (operator("=") ~> expr) ^^ {
    case name ~ value => Assign(name, value)
  }

  def lambdaExpr: Parser[Expr] = operator("\\") ~> rep1(name) ~ (operator("->") ~> expr) ^^ {
    case names ~ expr => LambdaExpr(names, expr)
  }

  def ifExpr: Parser[Expr] = keyword("if") ~> expr ~ (keyword("then") ~> expr) ~ (keyword("else") ~> expr) ^^ {
    case cond ~ ifTrue ~ ifFalse => IfExpr(cond, ifTrue, ifFalse)
  }

  def infix: Parser[Expr] = chainl1(prefix, infixOperation)

  def prefix: Parser[Expr] = rep1(atomic) ^^ {
    case List(atomic) => atomic
    case atomics => atomics.reduceLeft(PrefixExpr)
  }

  def atomic: Parser[Expr] =
    punctuation("(") ~> expr <~ punctuation(")") | list | positioned(name ^^ NameNode) | literal

  def list: Parser[Expr] =
    punctuation("[") ~> rep(atomic) <~ punctuation("]") ^^ ListExpr

  def name: Parser[String] = acceptMatch(s"a name", {
    case NameToken(name) => name
  })

  def literal: Parser[Expr] = positioned(acceptMatch(s"a literal", {
    case IntToken(num) => IntNode(num.toInt)
    case FloatToken(num) => FloatNode(num.toDouble)
    case StringToken(value) => StringNode(value)
  }))

  def keyword(str: String): Parser[Unit] = acceptMatch(s"keyword '$str'", {
    case KeywordToken(keyword) if keyword == str => ()
  })

  def operator(str: String): Parser[Unit] = acceptMatch(s"operator '$str'", {
    case InfixToken(operator) if operator == str => ()
  })

  def punctuation(str: String): Parser[Unit] = acceptMatch(s"punctuation '$str'", {
    case PunctuationToken(punctuation) if punctuation == str => ()
  })

  def infixOperation: Parser[(Expr, Expr) => Expr] = accept(s"an infix operator", {
    case t@InfixToken(op) => (a: Expr, b: Expr) => InfixExpr(NameNode(op), a, b).setPos(t.pos)
  })

  def apply(tokens: Seq[Token]): Expr = expr(TokenReader(tokens)) match {
    case Success(result, _) => result
    case failure : NoSuccess => throw new SyntaxError(s"unexpected ${failure.next.first}, ${failure.msg}", failure.next.pos)
  }
}