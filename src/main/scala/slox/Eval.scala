package slox.eval

import slox.SyntaxError
import slox.lexer._
import slox.parser._

object Eval {
  def evaluate(expression: Expr): Either[List[SyntaxError], Expr] = expression match {
    case StringLiteral(_, _) | NumberLiteral(_, _) | BoolLiteral(_, _) | NilLiteral(_) =>
      Right(expression)
    case Binary(l, op, r) => {
      val lResult = evaluate(l)
      val rResult = evaluate(r)

      (lResult, op.tokenType, rResult) match {
        case (Left(lErrors), _, Left(rErrors)) => Left(lErrors ++ rErrors)
        case (Left(lErrors), _, Right(_))      => Left(lErrors)
        case (Right(_), _, Left(rErrors))      => Left(rErrors)
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
            case (lExpr, _, rExpr) =>
              Left(
                List(
                  SyntaxError.error(
                    s"Invalid operation: ${Expr.toString(lExpr)} ${op.lexeme} ${Expr.toString(rExpr)}"
                  )
                )
              )
          }
      }
    }
    case Unary(op, r) => {
      val rResult = evaluate(r)

      (op.tokenType, rResult) match {
        case (_, Left(errors)) => Left(errors)
        case (tokenType, Right(rExpr)) =>
          (tokenType, rExpr) match {
            case (MinusToken, NumberLiteral(value, _)) => Right(NumberLiteral(-value, None))
            case (BangToken, BoolLiteral(value, _))    => Right(BoolLiteral(!value, None))
            case _ =>
              Left(
                List(SyntaxError.error(s"Invalid operation: ${op.lexeme} ${Expr.toString(rExpr)}"))
              )
          }
      }
    }
    case Grouping(expr) => evaluate(expr)
    case expression =>
      Left(List(SyntaxError.error(s"Unsupported expression: ${Expr.toString(expression)}")))
  }
}
