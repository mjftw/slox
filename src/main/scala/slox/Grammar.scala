package slox

import scala.util.matching.Regex

sealed trait TokenType

case object IfToken extends TokenType
case object IntToken extends TokenType
case object VarToken extends TokenType
case object EOFToken extends TokenType

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
    (IfToken, """if"""),
    (IntToken, """[0-9]+"""),
    (VarToken, """[a-zA-Z]{1}[\w_]*"""),
    (EOFToken, "")
  ).map({ case (token, pattern) => (token, patternToRegex(pattern)) })

  def patternToRegex(pattern: String): Regex = s"""(?s)^($pattern)(.*)""".r
}
