package slox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues
import org.scalatest.Inside
import slox.lexer._
import slox.parser._
import org.scalactic.Fail

class ExprSpec extends AnyFlatSpec with Matchers with EitherValues with Inside {
  def getValidLiteral(tokenType: TokenType, lexeme: String) = {
    val token = Token(tokenType, lexeme, 0, 0)
    val result = Expr.toLiteral(token)

    result should be('right)
    result
  }

  def isErrorLiteral(tokenType: TokenType, lexeme: String) = {
    val token = Token(tokenType, lexeme, 0, 0)
    val result = Expr.toLiteral(token)

    result should be('left)
  }

  "toLiteral" should "convert valid ints" in {
    inside(getValidLiteral(Token.Number, "42")) {
      case Right(l: NumberLiteral) => l.value should be(42.0)
      case _                       => fail("Was incorrect literal")
    }
  }

  it should "convert valid doubles" in {
    inside(getValidLiteral(Token.Number, "3.141592654")) {
      case Right(l: NumberLiteral) => l.value should be(3.141592654)
      case _                       => fail("Was incorrect literal")
    }
  }

  it should "convert valid floats" in {
    inside(getValidLiteral(Token.Number, "3.14")) {
      case Right(l: NumberLiteral) => l.value should be(3.14)
      case _                       => fail("Was incorrect literal")
    }
  }

  it should "convert valid strings" in {
    inside(getValidLiteral(Token.String, "hello")) {
      case Right(l: StringLiteral) => l.value should be("hello")
      case _                       => fail("Was incorrect literal")
    }
  }

  it should "convert true" in {
    inside(getValidLiteral(Token.True, "true")) {
      case Right(l: BoolLiteral) => l.value should be(true)
      case _                     => fail("Was incorrect literal")
    }
  }

  it should "convert false" in {
    inside(getValidLiteral(Token.False, "false")) {
      case Right(l: BoolLiteral) => l.value should be(false)
      case _                     => fail("Was incorrect literal")
    }
  }

  it should "convert nil, ignoring lexeme" in {
    inside(getValidLiteral(Token.Nil, "foo")) {
      case Right(l: NilLiteral) => succeed
      case _                    => fail("Was incorrect literal")
    }
  }

  it should "error on invalid number" in isErrorLiteral(Token.Number, "hello")
  it should "error on invalid true" in isErrorLiteral(Token.True, "world")
  it should "error on invalid false" in isErrorLiteral(Token.False, "world")
}
