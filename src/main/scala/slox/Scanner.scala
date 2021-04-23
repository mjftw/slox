package slox

import scala.annotation.tailrec
import Grammar._
import os.truncate

case class SyntaxError(line: Int, message: String)
object Scanner {
  val newline = Grammar.patternToRegex("""\r{0,1}\n""")

  def takeToken(
      input: String,
      grammar: Grammar,
      line: Int
  ): Option[(Token, String)] =
    grammar match {
      case Nil => None
      case (tokenType, pattern) :: grammarRest =>
        input match {
          case newline(_, restInput)      => takeToken(restInput, grammar, line + 1)
          case pattern(lexeme, restInput) => Some(Token(tokenType, lexeme, line), restInput)
          case _                          => takeToken(input, grammarRest, line)
        }
    }

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
        takeToken(input, grammar, 1) match {
          case None =>
            Left(SyntaxError(0, "Syntax error"))
          case Some((token, restInput)) =>
            scanTokens(restInput, token :: tokens)(grammar)
        }
    }
}
