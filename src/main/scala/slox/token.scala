import scala.util.matching.Regex
import scala.annotation.tailrec

case class ParseError(message: String)

sealed trait Token

case class IfToken() extends Token
case class IntToken() extends Token
case class VarToken() extends Token

object Parse {
  val grammar = List(
    ("""if""", IfToken()),
    ("""[0-9]+""", IntToken()),
    ("""[a-zA-Z]{1}[\w_]*""", VarToken())
  ).map(grammarToRegex)

  def grammarToRegex(grammar: (String, Token)): (Regex, Token) = grammar match {
    case (reStr, token) => (s"""^($reStr)(.*)""".r, token)
  }

  def takeToken(
      input: String,
      grammar: List[(Regex, Token)]
  ): Option[(Token, String)] =
    grammar match {
      case Nil => None
      case (pattern, token) :: lexiconRest =>
        input match {
          case pattern(matched, restInput) => Some((token, restInput))
          case _                           => takeToken(input, lexiconRest)
        }
    }

  @tailrec
  def tokenise(
      input: String,
      grammar: List[(Regex, Token)],
      tokens: List[Token] = Nil
  ): Either[ParseError, List[Token]] =
    input match {
      case "" => Right(tokens.reverse)
      case _ =>
        takeToken(input, grammar) match {
          case None =>
            Left(ParseError(s"Text didn't match any pattern: $input"))
          case Some((token, restInput)) =>
            tokenise(restInput, grammar, token :: tokens)
        }
    }
}
