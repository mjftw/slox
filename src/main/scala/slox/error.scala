package slox

import slox.lexer.Token

trait Error {
  def format: String
}

case class SyntaxError(message: String, line: Option[Int], column: Option[Int]) extends Error {
  def format: String = {
    val lineStr = line.map(num => s" on line ${num}").getOrElse("")
    val columnStr = column.map(num => s" column ${num}").getOrElse("")

    s"Syntax error${lineStr}${columnStr}: ${message}"
  }
}
case class RuntimeError(message: String) extends Error {
  def format: String = s"Runtime error: ${message}"
}

object SyntaxError {
  def error(message: String): SyntaxError = SyntaxError(message, None, None)

  def fromToken(message: String, token: Token): SyntaxError = {
    SyntaxError(message, Some(token.line), Some(token.column))
  }
}
