package slox

import scala.util.matching.Regex

sealed trait Token

case class IfToken() extends Token
case class IntToken() extends Token
case class VarToken() extends Token

object Grammar {
  type Grammar = List[(Regex, Token)]

  implicit val grammar: Grammar = List(
    ("""if""", IfToken()),
    ("""[0-9]+""", IntToken()),
    ("""[a-zA-Z]{1}[\w_]*""", VarToken())
  ).map(grammarToRegex)

  def grammarToRegex(grammar: (String, Token)): (Regex, Token) = grammar match {
    case (reStr, token) => (s"""^($reStr)(.*)""".r, token)
  }
}
