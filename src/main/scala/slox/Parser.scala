package slox.parser

import slox.lexer.Token
import scala.annotation.tailrec
import slox.lexer.BangEqualToken
import slox.lexer.EqualEqualToken
import slox.lexer.TrueToken
import slox.lexer.TokenType
import slox.lexer.GreaterToken
import slox.lexer.GreaterEqualToken
import slox.lexer.LessToken
import slox.lexer.LessEqualToken
import slox.lexer.MinusToken
import slox.lexer.PlusToken
import slox.lexer.SlashToken
import slox.lexer.StarToken

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

object Parser {
  type Context = (List[Token], Expr)

  def tokenMatches(token: Token, matches: List[TokenType]): Boolean =
    matches.exists(_ == token.tokenType)

  def leftAssocBinOp(matches: List[TokenType])(
      goDeeper: List[Token] => Context
  ): List[Token] => Context = tokens => {
    @tailrec
    def loop(ctx: Context): Context =
      ctx match {
        case (Nil, _) => ctx
        case (token :: tailTokens, expr) => {
          if (tokenMatches(token, matches)) {
            val operator = token
            val (restTokens, rightExpr) = goDeeper(tailTokens)
            loop((restTokens, Binary(expr, operator, rightExpr)))
          } else ctx
        }
      }

    loop(goDeeper(tokens))
  }

  def primary(tokens: List[Token]): Context = ???

  def unary(tokens: List[Token]): Context = ???

  val factor = leftAssocBinOp(List(SlashToken, StarToken))(unary)

  val term = leftAssocBinOp(List(MinusToken, PlusToken))(factor)

  val comparison =
    leftAssocBinOp(List(GreaterToken, GreaterEqualToken, LessToken, LessEqualToken))(term)

  val equality =
    leftAssocBinOp(List(BangEqualToken, EqualEqualToken))(comparison)

  val expression = equality

  def parseToAst(tokens: List[Token]): Expr = ???

}
