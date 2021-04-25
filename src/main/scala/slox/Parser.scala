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
import slox.lexer.BangToken
import slox.lexer.EOFToken
import slox.lexer.FalseToken
import slox.lexer.NilToken
import slox.lexer.NumberToken
import slox.lexer.StringToken
import slox.lexer.RightParenToken
import slox.lexer.LeftParenToken
import slox.lexer.SemicolonToken

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Literal(token: Token) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
case class NoExpr() extends Expr

object Expr {
  def toString(expression: Expr): String = expression match {
    case Binary(left, op, right)         => s"(${op.lexeme} ${toString(left)} ${toString(right)})"
    case Grouping(expr)                  => s"(group ${toString(expr)})"
    case Literal(Token(_, lexeme, _, _)) => lexeme
    case Unary(op, right)                => s"(${op.lexeme} ${toString(right)})"
    case NoExpr()                        => "<empty>"
  }
}

//TODO: Should also have line and col numbers
case class ParseError(message: String, line: Option[Int], column: Option[Int])

object Parser {
  type Context = (List[Token], Expr, List[ParseError])

  def parseError(
      message: String,
      line: Int = -1,
      column: Int = -1
  ) = {
    val maybeLine = if (line < 0) None else Some(line)
    val maybeColumn = if (column < 0) None else Some(column)
    ParseError(message, maybeLine, maybeColumn)
  }

  def formatParseError(error: ParseError): String = {
    val lineStr = error.line match {
      case None       => ""
      case Some(line) => s" on line ${line}"
    }
    val columnStr = error.column match {
      case None         => ""
      case Some(column) => s" column ${column}"
    }

    s"Parse error${lineStr}${columnStr}: ${error.message}"
  }

  def tokenMatches(token: Token, matches: List[TokenType]): Boolean =
    matches.exists(_ == token.tokenType)

  /** Search for next statement */
  def synchronise(tokens: List[Token]): List[Token] =
    tokens.dropWhile(_.tokenType != SemicolonToken)

  def consume(tokens: List[Token], ofType: TokenType): Context =
    tokens match {
      case Token(ofType, _, _, _) :: tailTokens => (tailTokens, NoExpr(), Nil)
      case Nil => {
        val error = parseError(s"Found end of file while looking for a ${ofType}")
        (Nil, NoExpr(), List(error))
      }
      case token :: tailTokens => {
        val error = parseError(s"Expected a ${ofType} but found: ${token.lexeme}")
        (tailTokens, NoExpr(), List(error))
      }
    }

  def leftAssocBinOp(matches: List[TokenType])(
      goDeeper: List[Token] => Context
  ): List[Token] => Context = tokens => {
    @tailrec
    def loop(ctx: Context): Context =
      ctx match {
        case (Nil, _, _) => ctx
        case (token :: tailTokens, expr, errors) => {
          if (tokenMatches(token, matches)) {
            val operator = token
            val (restTokens, rightExpr, newErrors) = goDeeper(tailTokens)
            loop((restTokens, Binary(expr, operator, rightExpr), errors ++ newErrors))
          } else ctx
        }
      }

    loop(goDeeper(tokens))
  }

  def primary(tokens: List[Token]): Context = tokens match {
    case Nil => (Nil, NoExpr(), List(parseError("Reached end of file while looking for literal!")))
    case token :: tailTokens =>
      token.tokenType match {
        case TrueToken | FalseToken | NilToken | NumberToken | StringToken =>
          (tailTokens, Literal(token), Nil)
        case LeftParenToken => {
          val (restTokens1, expr, errors1) = expression(tailTokens)
          val (restTokens2, _, errors2) = consume(restTokens1, RightParenToken)
          errors2 match {
            case Nil => (restTokens2, Grouping(expr), errors1)
            case _   => (synchronise(restTokens2), expr, errors1 ++ errors2)
          }
        }
        case _ => {
          val error =
            parseError(s"Expected a value, not a ${token.tokenType}", token.line, token.column)
          val (restTokens, expr, errors) = expression(synchronise(tokens))
          (restTokens, expr, error :: errors)
        }
      }
  }

  def unary(tokens: List[Token]): Context = {
    val matches = List(BangToken, MinusToken)

    tokens match {
      case Nil => (Nil, NoExpr(), List(parseError("Unexpected end of file!")))
      case token :: tailTokens if (tokenMatches(token, matches)) => {
        val operator = token
        val (restTokens, rightExpr, errors) = unary(tailTokens)
        (restTokens, Unary(operator, rightExpr), errors)
      }
      case _ => primary(tokens)
    }
  }

  val factor = leftAssocBinOp(List(SlashToken, StarToken))(unary)

  val term = leftAssocBinOp(List(MinusToken, PlusToken))(factor)

  val comparison =
    leftAssocBinOp(List(GreaterToken, GreaterEqualToken, LessToken, LessEqualToken))(term)

  val equality =
    leftAssocBinOp(List(BangEqualToken, EqualEqualToken))(comparison)

  val expression = equality

  def parseToAst(tokens: List[Token]): Either[List[ParseError], Expr] = {
    val (unconsumed, expr, errors) = expression(tokens)
    //TODO: Check unconsumed tokens (errors)
    errors match {
      case Nil => Right(expr)
      case _   => Left(errors)
    }
  }

}
