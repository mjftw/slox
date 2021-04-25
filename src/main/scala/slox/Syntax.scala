package slox.parser

import slox.lexer.Token

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Literal(token: Token) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr

object Expr {
  def toString(expression: Expr): String = expression match {
    case Binary(left, op, right)         => s"(${op.lexeme} ${toString(left)} ${toString(right)})"
    case Grouping(expr)                  => s"(group ${toString(expr)})"
    case Literal(Token(_, lexeme, _, _)) => lexeme
    case Unary(op, right)                => s"(${op.lexeme} ${toString(right)})"
  }
}
