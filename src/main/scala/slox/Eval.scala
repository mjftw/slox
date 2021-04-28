package slox.eval

import slox.lexer._
import slox.parser._

object Eval {
  def evaluate(expression: Expr): Expr = expression match {
    case StringLiteral(_, _) | NumberLiteral(_, _) | BoolLiteral(_, _) | NilLiteral(_) => expression
    case Binary(l, op, r) => {
      val lResult = evaluate(l)
      val rResult = evaluate(r)

      (lResult, op.tokenType, rResult) match {
        case (StringLiteral(lVal, _), PlusToken, StringLiteral(rVal, _)) =>
          StringLiteral(lVal ++ rVal, None)
        case (NumberLiteral(lVal, _), PlusToken, NumberLiteral(rVal, _)) =>
          NumberLiteral(lVal + rVal, None)
        case (NumberLiteral(lVal, _), MinusToken, NumberLiteral(rVal, _)) =>
          NumberLiteral(lVal - rVal, None)
        case (NumberLiteral(lVal, _), StarToken, NumberLiteral(rVal, _)) =>
          NumberLiteral(lVal * rVal, None)
        case (NumberLiteral(lVal, _), SlashToken, NumberLiteral(rVal, _)) =>
          NumberLiteral(lVal / rVal, None)
        case (BoolLiteral(lVal, _), EqualEqualToken, BoolLiteral(rVal, _)) =>
          BoolLiteral(lVal == rVal, None)
        case (BoolLiteral(lVal, _), BangEqualToken, BoolLiteral(rVal, _)) =>
          BoolLiteral(lVal != rVal, None)
        case _ => ??? //TODO: Error handling
      }
    }
    case Unary(op, r) => {
      val rResult = evaluate(r)

      (op.tokenType, rResult) match {
        case (MinusToken, NumberLiteral(value, _)) => NumberLiteral(-value, None)
        case (BangToken, BoolLiteral(value, _))    => BoolLiteral(!value, None)
        case _                                     => ??? //TODO: Error handling
      }
    }
    case Grouping(expr) => evaluate(expr)
    case _              => ??? // TODO: Error handling
  }
}
