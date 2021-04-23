package slox

import scala.annotation.tailrec
import Grammar._

case class ParseError(message: String)
object Scanner {
  def takeToken(
      input: String,
      grammar: Grammar
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
  def scanTokens(
      input: String,
      tokens: List[Token] = Nil
  )(implicit grammar: Grammar): Either[ParseError, List[Token]] =
    input match {
      case "" => Right(tokens.reverse)
      case _ =>
        takeToken(input, grammar) match {
          case None =>
            Left(ParseError(s"Text didn't match any pattern: $input"))
          case Some((token, restInput)) =>
            scanTokens(restInput, token :: tokens)(grammar)
        }
    }
}
