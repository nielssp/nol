package dk.nielssp.nol

import scala.util.parsing.input.Positional

abstract sealed class Token extends Positional
case class StringToken(value: String) extends Token
case class IntToken(value: String) extends Token
case class FloatToken(value: String) extends Token
case class NameToken(name: String) extends Token
case class KeywordToken(keyword: String) extends Token
case class InfixToken(operator: String) extends Token
case class PunctuationToken(punctuation: String) extends Token