package slox

import scala.annotation.tailrec
import Grammar._
import os.truncate

case class SyntaxError(line: Int, message: String)
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

  def getCurrentLine(tokens: List[Token]) = tokens
    .filter({
      case _: NewlineToken => true
      case _               => false
    })
    .length + 1

  def formatSyntaxError(error: SyntaxError): String =
    s"${error.message} on line ${error.line}"

  @tailrec
  def scanTokens(
      input: String,
      tokens: List[Token] = Nil
  )(implicit grammar: Grammar): Either[SyntaxError, List[Token]] =
    input match {
      case "" => Right(tokens.reverse)
      case _ =>
        takeToken(input, grammar) match {
          case None =>
            Left(SyntaxError(getCurrentLine(tokens), "Syntax error"))
          case Some((token, restInput)) =>
            scanTokens(restInput, token :: tokens)(grammar)
        }
    }
}
