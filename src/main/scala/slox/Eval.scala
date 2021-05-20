package slox.eval

import slox.SyntaxError
import slox.lexer._
import slox.parser._

object Eval {
  private def evaluateBinary(expr: Binary): Either[List[SyntaxError], Expr] = {
    val leftResult = evaluate(expr.left)
    val rightResult = evaluate(expr.right)

    (leftResult, expr.operator.tokenType, rightResult) match {
      case (Right(lExpr), tokenType, Right(rExpr)) =>
        (lExpr, tokenType, rExpr) match {
          case (StringLiteral(lVal, _), PlusToken, StringLiteral(rVal, _)) =>
            Right(StringLiteral(lVal ++ rVal, None))
          case (NumberLiteral(lVal, _), PlusToken, NumberLiteral(rVal, _)) =>
            Right(NumberLiteral(lVal + rVal, None))
          case (NumberLiteral(lVal, _), MinusToken, NumberLiteral(rVal, _)) =>
            Right(NumberLiteral(lVal - rVal, None))
          case (NumberLiteral(lVal, _), StarToken, NumberLiteral(rVal, _)) =>
            Right(NumberLiteral(lVal * rVal, None))
          case (NumberLiteral(lVal, _), SlashToken, NumberLiteral(rVal, _)) =>
            Right(NumberLiteral(lVal / rVal, None))
          case (BoolLiteral(lVal, _), EqualEqualToken, BoolLiteral(rVal, _)) =>
            Right(BoolLiteral(lVal == rVal, None))
          case (BoolLiteral(lVal, _), BangEqualToken, BoolLiteral(rVal, _)) =>
            Right(BoolLiteral(lVal != rVal, None))
          case (l, _, r) =>
            Left(
              List(
                SyntaxError.error(
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

  private def evaluateUnary(expr: Unary): Either[List[SyntaxError], Expr] = {
    val rightResult = evaluate(expr.right)

    (expr.operator.tokenType, rightResult) match {
      case (tokenType, Right(rExpr)) =>
        (tokenType, rExpr) match {
          case (MinusToken, NumberLiteral(value, _)) => Right(NumberLiteral(-value, None))
          case (BangToken, BoolLiteral(value, _))    => Right(BoolLiteral(!value, None))
          case _ =>
            Left(
              List(
                SyntaxError.error(
                  s"Invalid operation: ${expr.operator.lexeme} ${Expr.toString(rExpr)}"
                )
              )
            )
        }
      case (_, Left(errors)) => Left(errors)
    }
  }

  private def evaluateGrouping(expr: Grouping): Either[List[SyntaxError], Expr] =
    evaluate(expr.expr)

  def evaluate(expr: Expr): Either[List[SyntaxError], Expr] = expr match {
    case _: StringLiteral | _: NumberLiteral | _: BoolLiteral | _: NilLiteral =>
      Right(expr)
    case expr: Binary   => evaluateBinary(expr)
    case expr: Unary    => evaluateUnary(expr)
    case expr: Grouping => evaluateGrouping(expr)
    case expr =>
      Left(List(SyntaxError.error(s"Unsupported expression: ${Expr.toString(expr)}")))
  }
}
