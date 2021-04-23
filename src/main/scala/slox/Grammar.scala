package slox

import scala.util.matching.Regex

sealed trait TokenType

case object IfTokenType extends TokenType
case object IntTokenType extends TokenType
case object VarTokenType extends TokenType
case object EOFTokenType extends TokenType

sealed trait Literal

case class Token(
    tokenType: TokenType,
    lexeme: String,
    // literal: Literal,
    line: Int
)

object Grammar {
  type Grammar = List[(TokenType, Regex)]

  implicit val grammar: Grammar = List(
    (IfTokenType, """if"""),
    (IntTokenType, """[0-9]+"""),
    (VarTokenType, """[a-zA-Z]{1}[\w_]*"""),
    (EOFTokenType, "")
  ).map({ case (token, pattern) => (token, patternToRegex(pattern)) })

  def patternToRegex(pattern: String): Regex = s"""(?s)^($pattern)(.*)""".r
}
