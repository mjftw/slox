package slox.parser

import scala.util.Try
import slox.lexer._

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
case class NoExpr() extends Expr

sealed trait Literal extends Expr
case class StringLiteral(token: Token, value: String) extends Literal
case class NumberLiteral(token: Token, value: Double) extends Literal
case class BoolLiteral(token: Token, value: Boolean) extends Literal
case class NilLiteral(token: Token) extends Literal

object Expr {
  def toString(expression: Expr): String = expression match {
    case Binary(left, op, right)                  => s"(${op.lexeme} ${toString(left)} ${toString(right)})"
    case Grouping(expr)                           => s"(group ${toString(expr)})"
    case StringLiteral(Token(_, lexeme, _, _), _) => lexeme
    case NumberLiteral(Token(_, lexeme, _, _), _) => lexeme
    case BoolLiteral(Token(_, lexeme, _, _), _)   => lexeme
    case _: NilLiteral                            => "nil"
    case Unary(op, right)                         => s"(${op.lexeme} ${toString(right)})"
    case NoExpr()                                 => "<empty>"
  }

  def toLiteral(token: Token): Either[ParseError, Literal] = token match {
    case Token(StringToken, lexeme, _, _) => Right(StringLiteral(token, lexeme))
    case Token(NumberToken, lexeme, line, column) =>
      Try(NumberLiteral(token, lexeme.toDouble)).toEither
        .fold(
          l => Left(Parser.parseError(s"${lexeme} is not a valid number", line, column)),
          r => Right(r)
        )
    case Token(TrueToken, lexeme, line, column) =>
      Try(BoolLiteral(token, lexeme.toBoolean)).toEither
        .fold(
          l => Left(Parser.parseError(s"${lexeme} is not a valid boolean", line, column)),
          r => Right(r)
        )
    case Token(FalseToken, lexeme, line, column) =>
      Try(BoolLiteral(token, lexeme.toBoolean)).toEither
        .fold(
          l => Left(Parser.parseError(s"${lexeme} is not a valid boolean", line, column)),
          r => Right(r)
        )
    case Token(NilToken, _, _, _) => Right(NilLiteral(token))
    case Token(_, lexeme, line, col) =>
      Left(Parser.parseError(s"${lexeme} is not a valid literal", line, col))
  }
}
