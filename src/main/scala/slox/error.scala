package slox

import slox.lexer.Token
import cats.instances.string

trait Error {
  def format: String
}

object Error {
  def formatLineCol(line: Option[Int], column: Option[Int]): String = {
    val lineStr = line.map(num => s" on line ${num}").getOrElse("")
    val columnStr = column.map(num => s" column ${num}").getOrElse("")

    s"${lineStr}${columnStr}"
  }
}

case class SyntaxError(message: String, line: Option[Int], column: Option[Int]) extends Error {
  def format: String = s"Syntax error${Error.formatLineCol(line, column)}: ${message}"
}
object SyntaxError {
  def fromString(message: String): SyntaxError = SyntaxError(message, None, None)
  def fromToken(message: String, token: Token): SyntaxError = {
    SyntaxError(message, Some(token.line), Some(token.column))
  }
}
case class RuntimeError(message: String, line: Option[Int], column: Option[Int]) extends Error {
  def format: String = s"Runtime error${Error.formatLineCol(line, column)}: ${message}"
}

object RuntimeError {
  def fromString(message: String): RuntimeError = RuntimeError(message, None, None)
  def fromToken(message: String, token: Token): RuntimeError = {
    RuntimeError(message, Some(token.line), Some(token.column))
  }
}
