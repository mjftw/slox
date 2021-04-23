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
      line: Int,
      column: Int
  ): Option[(Token, String)] =
    grammar match {
      case Nil => None
      case (tokenType, pattern) :: grammarRest =>
        input match {
          case newline(_, restInput) => takeToken(restInput, grammar, line + 1, 1)
          case pattern(lexeme, restInput) =>
            Some(Token(tokenType, lexeme, line, column), restInput)
          case _ => takeToken(input, grammarRest, line, column)
        }
    }

  def formatSyntaxError(error: SyntaxError): String =
    s"${error.message} on line ${error.line}"

  def scanTokens(input: String)(implicit grammar: Grammar): Either[SyntaxError, List[Token]] = {

    @tailrec
    def loop(
        tokens: List[Token],
        input: String,
        line: Int,
        column: Int
    ): Either[SyntaxError, List[Token]] =
      input match {
        case "" => Right(tokens.reverse)
        case _ =>
          takeToken(input, grammar, line, column) match {
            case None =>
              Left(SyntaxError(0, "Syntax error"))
            case Some((token, restInput)) =>
              loop(token :: tokens, restInput, token.line, token.column + token.lexeme.length)
          }
      }

    loop(Nil, input, 1, 1)
  }
}
