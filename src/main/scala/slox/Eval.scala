package slox.eval

import slox.{Error, SyntaxError, RuntimeError}
import slox.lexer._
import slox.parser._

object Eval {
  private def evaluateBinary(expr: Binary): Either[List[slox.Error], Expr] = {
    val leftResult = evaluate(expr.left)
    val rightResult = evaluate(expr.right)

    (leftResult, expr.operator.tokenType, rightResult) match {
      case (Right(lExpr), tokenType, Right(rExpr)) =>
        (lExpr, tokenType, rExpr) match {
          case (StringLiteral(lVal, _), Token.Plus, StringLiteral(rVal, _)) =>
            Right(StringLiteral(lVal ++ rVal, None))
          case (NumberLiteral(lVal, _), Token.Plus, NumberLiteral(rVal, _)) =>
            Right(NumberLiteral(lVal + rVal, None))
          case (NumberLiteral(lVal, _), Token.Minus, NumberLiteral(rVal, _)) =>
            Right(NumberLiteral(lVal - rVal, None))
          case (NumberLiteral(lVal, _), Token.Star, NumberLiteral(rVal, _)) =>
            Right(NumberLiteral(lVal * rVal, None))
          case (NumberLiteral(lVal, _), Token.Slash, NumberLiteral(0, _)) =>
            Left(List(slox.RuntimeError.fromString("Division by zero")))
          case (NumberLiteral(lVal, _), Token.Slash, NumberLiteral(rVal, _)) =>
            Right(NumberLiteral(lVal / rVal, None))
          case (BoolLiteral(lVal, _), Token.EqualEqual, BoolLiteral(rVal, _)) =>
            Right(BoolLiteral(lVal == rVal, None))
          case (BoolLiteral(lVal, _), Token.BangEqual, BoolLiteral(rVal, _)) =>
            Right(BoolLiteral(lVal != rVal, None))
          case (l, _, r) =>
            Left(
              List(
                slox.SyntaxError.fromString(
                  s"Invalid operation: ${Expr.toString(l)} ${expr.operator.lexeme} ${Expr.toString(r)}"
                )
              )
            )
        }
      case (Left(lErrors), _, Left(rErrors)) => Left(lErrors ++ rErrors)
      case (Left(lErrors), _, Right(_))      => Left(lErrors)
      case (Right(_), _, Left(rErrors))      => Left(rErrors)
    }
  }

  private def evaluateUnary(expr: Unary): Either[List[slox.Error], Expr] = {
    val rightResult = evaluate(expr.right)

    (expr.operator.tokenType, rightResult) match {
      case (tokenType, Right(rExpr)) =>
        (tokenType, rExpr) match {
          case (Token.Minus, NumberLiteral(value, _)) => Right(NumberLiteral(-value, None))
          case (Token.Bang, BoolLiteral(value, _))    => Right(BoolLiteral(!value, None))
          case _ =>
            Left(
              List(
                SyntaxError.fromString(
                  s"Invalid operation: ${expr.operator.lexeme} ${Expr.toString(rExpr)}"
                )
              )
            )
        }
      case (_, Left(errors)) => Left(errors)
    }
  }

  private def evaluateGrouping(expr: Grouping): Either[List[slox.Error], Expr] =
    evaluate(expr.expr)

  def evaluate(expr: Expr): Either[List[slox.Error], Expr] = expr match {
    case _: StringLiteral | _: NumberLiteral | _: BoolLiteral | _: NilLiteral =>
      Right(expr)
    case expr: Binary   => evaluateBinary(expr)
    case expr: Unary    => evaluateUnary(expr)
    case expr: Grouping => evaluateGrouping(expr)
    case expr =>
      Left(List(SyntaxError.fromString(s"Unsupported expression: ${Expr.toString(expr)}")))
  }
}
