package slox

import slox.lexer.Token

case class SyntaxError(message: String, line: Option[Int], column: Option[Int])

object SyntaxError {
  def error(message: String): SyntaxError = SyntaxError(message, None, None)

  def fromToken(message: String, token: Token): SyntaxError = {
    SyntaxError(message, Some(token.line), Some(token.column))
  }

  def formatError(error: SyntaxError): String = {
    val lineStr = error.line match {
      case None       => ""
      case Some(line) => s" on line ${line}"
    }
    val columnStr = error.column match {
      case None         => ""
      case Some(column) => s" column ${column}"
    }

    s"Parse error${lineStr}${columnStr}: ${error.message}"
  }
}
