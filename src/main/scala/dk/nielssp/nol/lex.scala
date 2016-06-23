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

  val reserved = HashSet("->", "=", "#", ":")

  def operator: Parser[Token] = ("""[#\.:;!?&^$%@~|+=<>*/-]+""".r | """`[a-zA-Z0-9_!?]+`""".r) ^^ {
    case s if reserved.contains(s) => PunctuationToken(s)
    case s => InfixToken(s)
  }

  def punctuation: Parser[Token] = """\(|\)|\\|\[|\]|,|\{|\}""".r ^^ PunctuationToken

  val keywords = HashSet("let", "in", "if", "then", "else", "import", "typeclass", "instance", "forall")

  def name: Parser[Token] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ {
    case s if keywords.contains(s) => KeywordToken(s)
    case s => NameToken(s)
  }

  def quoted[T, U](quote: Parser[U], string: Parser[T]): Parser[T] = Parser(input => quote(input) match {
    case Success(_, next1) => string(next1) match {
      case Success(result, next2) if next2.atEnd => Failure("unclosed string literal", input)
      case Success(result, next2) => quote(next2) match {
        case Success(_, next3) => Success(result, next3)
        case e => Error("unclosed string literal", input)
      }
      case fail => fail
    }
    case _ => Failure("expected string literal", input)
  })

  def string: Parser[Token] = quoted('"', rep(char)) ^^ (chars => StringToken(chars.mkString))

  def char: Parser[String] = """[^"\\]""".r | '\\' ~> escape

  def escape: Parser[String] = "[abfnrtv\\\\'\"]|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}|[0-7]{3}".r ^^ {
    case "a" => 7.toChar.toString
    case "b" => '\b'.toString
    case "f" => '\f'.toString
    case "n" => '\n'.toString
    case "r" => '\r'.toString
    case "t" => '\t'.toString
    case "v" => 11.toChar.toString
    case "'" => '\''.toString
    case "\"" => '"'.toString
    case "\\" => '\\'.toString
    case str if str.head == 'x' => Character.toChars(Integer.parseInt(str.tail, 16)).mkString
    case str if str.head == 'u' => Character.toChars(Integer.parseInt(str.tail, 16)).mkString
    case str if str.head == 'U' => Character.toChars(Integer.parseInt(str.tail, 16)).mkString
    case octal => Integer.parseInt(octal, 8).toChar.toString
  } | failure("undefined escape sequence")

  def apply(input: String): List[Token] = parseAll(program, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) => throw new SyntaxError(msg, next.pos)
  }
}
