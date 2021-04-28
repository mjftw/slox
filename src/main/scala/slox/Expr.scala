package slox.parser

import scala.util.Try
import slox.lexer._

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
case class NoExpr() extends Expr

sealed trait Literal extends Expr
case class StringLiteral(value: String, token: Option[Token]) extends Literal
case class NumberLiteral(value: Double, token: Option[Token]) extends Literal
case class BoolLiteral(value: Boolean, token: Option[Token]) extends Literal
case class NilLiteral(token: Option[Token]) extends Literal

object Expr {
  def toString(expression: Expr): String = expression match {
    case Binary(left, op, right) => s"(${op.lexeme} ${toString(left)} ${toString(right)})"
    case Grouping(expr)          => s"(group ${toString(expr)})"
    case StringLiteral(value, _) => value
    case NumberLiteral(value, _) => {
      """\.0+$""".r.replaceFirstIn(value.toString, "")
    }
    case BoolLiteral(value, _) => value.toString
    case _: NilLiteral         => "nil"
    case Unary(op, right)      => s"(${op.lexeme} ${toString(right)})"
    case NoExpr()              => "<empty>"
  }

  def toLiteral(token: Token): Either[ParseError, Literal] = token match {
    case Token(StringToken, lexeme, _, _) => Right(StringLiteral(lexeme, Some(token)))
    case Token(NumberToken, lexeme, line, column) =>
      Try(NumberLiteral(lexeme.toDouble, Some(token))).toEither
        .fold(
          l => Left(Parser.parseError(s"${lexeme} is not a valid number", line, column)),
          r => Right(r)
        )
    case Token(TrueToken, lexeme, line, column) =>
      Try(BoolLiteral(lexeme.toBoolean, Some(token))).toEither
        .fold(
          l => Left(Parser.parseError(s"${lexeme} is not a valid boolean", line, column)),
          r => Right(r)
        )
    case Token(FalseToken, lexeme, line, column) =>
      Try(BoolLiteral(lexeme.toBoolean, Some(token))).toEither
        .fold(
          l => Left(Parser.parseError(s"${lexeme} is not a valid boolean", line, column)),
          r => Right(r)
        )
    case Token(NilToken, _, _, _) => Right(NilLiteral(Some(token)))
    case Token(_, lexeme, line, col) =>
      Left(Parser.parseError(s"${lexeme} is not a valid literal", line, col))
  }
}
