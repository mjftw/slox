package slox

import scala.annotation.tailrec
import Grammar._
import os.truncate

case class SyntaxError(message: String, line: Int, column: Int)

object Scanner {
  val newline = Grammar.patternToRegex("""\r{0,1}\n""")
  val whitespace = Grammar.patternToRegex("""\s+""")

  def takeToken(
      input: String,
      grammar: Grammar,
      line: Int,
      column: Int
  ): Option[(Token, String)] = {
    input match {
      case ""                    => Some((Token(EOFToken, "", line, column), ""))
      case newline(_, restInput) => takeToken(restInput, grammar, line + 1, 1)
      case whitespace(space, restInput) =>
        takeToken(restInput, grammar, line, column + space.length)
      case _ => {
        grammar
          .map { case (tokenType, pattern) =>
            input match {
              case pattern(lexeme, restInput) =>
                Some(Token(tokenType, lexeme, line, column), restInput)
              case _ => None
            }
          }
          .maxByOption {
            case None                              => 0
            case Some((Token(_, lexeme, _, _), _)) => lexeme.length
          }
          .flatten
      }
    }
  }

  def formatSyntaxError(error: SyntaxError): String =
    s"${error.message} on line ${error.line}, ${error.column}"

  def scanTokens(input: String)(implicit grammar: Grammar): Either[SyntaxError, List[Token]] = {

    @tailrec
    def loop(
        tokens: List[Token],
        input: String,
        line: Int,
        column: Int
    ): Either[SyntaxError, List[Token]] =
      takeToken(input, grammar, line, column) match {
        case None =>
          Left(SyntaxError("Syntax error", line, column))
        case Some((token, _)) if token.tokenType == EOFToken => Right((token :: tokens).reverse)
        case Some((token, restInput)) =>
          loop(token :: tokens, restInput, token.line, token.column + token.lexeme.length)
      }

    loop(Nil, input, 1, 1)
  }
}
