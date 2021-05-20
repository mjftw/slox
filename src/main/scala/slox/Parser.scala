package slox.parser

import slox.SyntaxError
import scala.annotation.tailrec
import slox.lexer._

object Parser {
  type Context = (List[Token], Expr, List[SyntaxError])

  def tokenMatches(token: Token, matches: List[TokenType]): Boolean =
    matches.exists(_ == token.tokenType)

  /** Search for next statement */
  def synchronise(tokens: List[Token]): List[Token] =
    tokens.dropWhile(_.tokenType != Token.Semicolon)

  def consume(tokens: List[Token], ofType: TokenType): Context =
    tokens match {
      case Token(ofType, _, _, _) :: tailTokens => (tailTokens, NoExpr(), Nil)
      case Nil => {
        val error = SyntaxError.fromString(s"Found end of file while looking for a ${ofType}")
        (Nil, NoExpr(), List(error))
      }
      case token :: tailTokens => {
        val error = SyntaxError.fromString(s"Expected a ${ofType} but found: ${token.lexeme}")
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
    case Nil =>
      (
        Nil,
        NoExpr(),
        List(SyntaxError.fromString("Reached end of file while looking for literal!"))
      )
    case token :: tailTokens =>
      token.tokenType match {
        case Token.True | Token.False | Token.Nil | Token.Number | Token.String =>
          Expr.toLiteral(token) match {
            case Left(error)    => (Nil, NoExpr(), List(error))
            case Right(literal) => (tailTokens, literal, Nil)
          }
        case Token.LeftParen => {
          val (restTokens1, expr, errors1) = expression(tailTokens)
          val (restTokens2, _, errors2) = consume(restTokens1, Token.RightParen)
          errors2 match {
            case Nil => (restTokens2, Grouping(expr), errors1)
            case _   => (synchronise(restTokens2), expr, errors1 ++ errors2)
          }
        }
        case _ => {
          val error =
            SyntaxError.fromToken(s"Expected a value, not a ${token.tokenType}", token)
          val (restTokens, expr, errors) = expression(synchronise(tokens))
          (restTokens, expr, error :: errors)
        }
      }
  }

  def unary(tokens: List[Token]): Context = {
    val matches = List(Token.Bang, Token.Minus)

    tokens match {
      case Nil => (Nil, NoExpr(), List(SyntaxError.fromString("Unexpected end of file!")))
      case token :: tailTokens if (tokenMatches(token, matches)) => {
        val operator = token
        val (restTokens, rightExpr, errors) = unary(tailTokens)
        (restTokens, Unary(operator, rightExpr), errors)
      }
      case _ => primary(tokens)
    }
  }

  val factor = leftAssocBinOp(List(Token.Slash, Token.Star))(unary)

  val term = leftAssocBinOp(List(Token.Minus, Token.Plus))(factor)

  val comparison =
    leftAssocBinOp(List(Token.Greater, Token.GreaterEqual, Token.Less, Token.LessEqual))(term)

  val equality =
    leftAssocBinOp(List(Token.BangEqual, Token.EqualEqual))(comparison)

  val expression = equality

  def parseToAst(tokens: List[Token]): Either[List[SyntaxError], Expr] = {
    val (unconsumed, expr, errors) = expression(tokens)

    val fullErrors = unconsumed match {
      case Nil                              => errors
      case Token(Token.EOF, _, _, _) :: Nil => errors
      case token :: _ =>
        SyntaxError.fromToken(s"Unconsumed token ${token.tokenType}", token) :: errors
    }

    fullErrors match {
      case Nil => Right(expr)
      case _   => Left(errors)
    }
  }

}
