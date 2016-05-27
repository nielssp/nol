package dk.nielssp.nol

import scala.collection.immutable.HashSet
import scala.util.parsing.combinator.RegexParsers


object lex extends RegexParsers {
  override def skipWhitespace = false

  def program: Parser[List[Token]] = skip ~> rep(token <~ skip) <~ eof

  def eof: Parser[String] = "\\z".r | failure("unexpected character")

  def token: Parser[Token] =
    positioned(operator | punctuation | name | number | string)

  def skip: Parser[Unit] = rep(whiteSpace | comment) ^^^ Unit

  def comment: Parser[Unit] = singleComment | multiComment

  def singleComment: Parser[Unit] = "//" ~ rep(not("\n") ~ ".".r) ^^^ Unit

  def multiComment: Parser[Unit] = "/*" ~ rep(not("*/") ~ ".".r) ~ "*/" ^^^ Unit

  def number: Parser[Token] = """\d+(\.\d+)?""".r ^^ {
    case s if s.contains('.') => FloatToken(s)
    case s => IntToken(s)
  }

  def operator: Parser[Token] = ("""[,\.:!?&^$%@~|+=<>*/-]+""".r | """`[a-zA-Z0-9_!?]+`""".r) ^^ InfixToken

  def punctuation: Parser[Token] = """\(|\)|\\|\[|\]""".r ^^ PunctuationToken

  val keywords = HashSet("let", "in", "if", "then", "else")

  def name: Parser[Token] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ {
    case s if keywords.contains(s) => KeywordToken(s)
    case s => NameToken(s)
  }

  def string: Parser[Token] =
    "\"" ~> rep(char) <~ "\"" ^^ (chars => StringToken(chars.mkString))

  def char: Parser[Char] = ("""[^"\\]""".r | '\\' ~> ".".r) ^^ { _.head }

  def apply(input: String): List[Token] = parseAll(program, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) => throw new SyntaxError(msg, next.pos)
  }
}
