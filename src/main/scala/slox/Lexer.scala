package slox.lexer

import scala.annotation.tailrec
import LexicalGrammar._
import os.Path

import slox.SyntaxError

object Lexer {
  def countNewlines(text: String): Int = "\r{0,1}\n".r.findAllIn(text).length
  def endColumn(text: String): Int = {
    val afterNewlines = """(?s).*\r{0,1}\n""".r.replaceFirstIn(text, "")
    afterNewlines.length()
  }

  def takeToken(
      input: String,
      grammar: LexicalGrammar,
      line: Int,
      column: Int
  ): Option[(Token, String, Int)] = {
    input match {
      case ""                    => Some((Token(Token.EOF, "", line, column), "", 0))
      case newline(_, restInput) => takeToken(restInput, grammar, line + 1, 1)
      case comment(_, restInput) => takeToken(restInput, grammar, line + 1, 1)
      case blockComment(comment, restInput) =>
        takeToken(restInput, grammar, line + countNewlines(comment), 1 + endColumn(comment))
      case whitespace(space, restInput) =>
        takeToken(restInput, grammar, line, column + space.length)
      case _ => {
        grammar
          .map { case (tokenType, pattern) =>
            input match {
              case pattern(lexeme, restInput) =>
                Some(
                  Token(tokenType, lexeme, line, column),
                  restInput,
                  line + countNewlines(lexeme)
                )
              case _ => None
            }
          }
          .maxByOption {
            case None                                 => 0
            case Some((Token(_, lexeme, _, _), _, _)) => lexeme.length
          }
          .flatten
      }
    }
  }

  def formatSyntaxError(error: SyntaxError, file: Option[Path] = None): String =
    s"${error.message} on line ${error.line}, ${error.column}"

  def scanTokens(
      input: String,
      grammar: LexicalGrammar = LexicalGrammar.grammar
  ): Either[SyntaxError, List[Token]] = {
    @tailrec
    def loop(
        tokens: List[Token],
        input: String,
        line: Int,
        column: Int
    ): Either[SyntaxError, List[Token]] =
      takeToken(input, grammar, line, column) match {
        case None =>
          Left(SyntaxError("Syntax error", Some(line), Some(column)))
        case Some((token, _, _)) if token.tokenType == Token.EOF => Right((token :: tokens).reverse)
        case Some((token, restInput, lineNum)) =>
          loop(token :: tokens, restInput, lineNum, token.column + token.lexeme.length)
      }

    loop(Nil, input, 1, 1)
  }
}
