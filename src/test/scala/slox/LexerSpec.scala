package slox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues
import slox.lexer._

class LexerSpec extends AnyFlatSpec with Matchers with EitherValues {
  def getTokens(inText: String): List[Token] = {
    val maybeTokens = Lexer.scanTokens(inText)

    maybeTokens should be('right)
    maybeTokens.right.get
  }

  def getFirstToken(inText: String): Token = {
    val tokens = getTokens(inText)
    tokens.length should be >= 1

    tokens.head
  }

  def firstTokenHasType(tokenType: TokenType, inText: String) =
    getFirstToken(inText).tokenType should be(tokenType)

  def firstTokenHasLexeme(lexeme: String, inText: String) =
    getFirstToken(inText).lexeme should be(lexeme)

  def tokensHaveTypes(types: List[TokenType], inText: String) = {
    val tokenTypes = getTokens(inText).map(_.tokenType)

    types should contain theSameElementsInOrderAs (tokenTypes)
  }

  "scanTokens" should "always put an EOF token last" in firstTokenHasType(Token.EOF, "")

  it should "find LeftParen" in firstTokenHasType(Token.LeftParen, "(")
  it should "find RightParen" in firstTokenHasType(Token.RightParen, ")")
  it should "find LeftBrace" in firstTokenHasType(Token.LeftBrace, "{")
  it should "find RightBrace" in firstTokenHasType(Token.RightBrace, "}")
  it should "find Comma" in firstTokenHasType(Token.Comma, ",")
  it should "find Dot" in firstTokenHasType(Token.Dot, ".")
  it should "find Minus" in firstTokenHasType(Token.Minus, "-")
  it should "find Plus" in firstTokenHasType(Token.Plus, "+")
  it should "find Semicolon" in firstTokenHasType(Token.Semicolon, ";")
  it should "find Slash" in firstTokenHasType(Token.Slash, "/")
  it should "find Star" in firstTokenHasType(Token.Star, "*")
  it should "find Bang" in firstTokenHasType(Token.Bang, "!")
  it should "find BangEqual" in firstTokenHasType(Token.BangEqual, "!=")
  it should "find Equal" in firstTokenHasType(Token.Equal, "=")
  it should "find EqualEqual" in firstTokenHasType(Token.EqualEqual, "==")
  it should "find Greater" in firstTokenHasType(Token.Greater, ">")
  it should "find GreaterEqual" in firstTokenHasType(Token.GreaterEqual, ">=")
  it should "find Less" in firstTokenHasType(Token.Less, "<")
  it should "find LessEqual" in firstTokenHasType(Token.LessEqual, "<=")
  it should "find Identifier" in firstTokenHasType(Token.Identifier, "foo")
  it should "find String" in firstTokenHasType(Token.String, "\"foo\"")
  it should "find Number without decimal point" in firstTokenHasType(Token.Number, "42")
  it should "find Number with decimal point" in firstTokenHasType(Token.Number, "3.14")
  it should "find And" in firstTokenHasType(Token.And, "and")
  it should "find Or" in firstTokenHasType(Token.Or, "or")
  it should "find Class" in firstTokenHasType(Token.Class, "class")
  it should "find False" in firstTokenHasType(Token.False, "false")
  it should "find True" in firstTokenHasType(Token.True, "true")
  it should "find Fun" in firstTokenHasType(Token.Fun, "fun")
  it should "find For" in firstTokenHasType(Token.For, "for")
  it should "find If" in firstTokenHasType(Token.If, "if")
  it should "find Else" in firstTokenHasType(Token.Else, "else")
  it should "find Nil" in firstTokenHasType(Token.Nil, "nil")
  it should "find Print" in firstTokenHasType(Token.Print, "print")
  it should "find Return" in firstTokenHasType(Token.Return, "return")
  it should "find Super" in firstTokenHasType(Token.Super, "super")
  it should "find This" in firstTokenHasType(Token.This, "this")
  it should "find Var" in firstTokenHasType(Token.Var, "var")
  it should "find While" in firstTokenHasType(Token.While, "while")

  it should "capture funky string text" in {
    val str = "\"j!I_A.w,@W~R+N=y-5~6[a]}{|\\><£$%^&*:;#`\""
    firstTokenHasLexeme(str, str)
  }

  it should "find multiple tokens in a single line" in tokensHaveTypes(
    List(
      Token.LeftParen,
      Token.RightParen,
      Token.LeftBrace,
      Token.RightBrace,
      Token.Comma,
      Token.Dot,
      Token.Minus,
      Token.Plus,
      Token.Semicolon,
      Token.Slash,
      Token.Star,
      Token.Bang,
      Token.BangEqual,
      Token.Equal,
      Token.EqualEqual,
      Token.Greater,
      Token.GreaterEqual,
      Token.Less,
      Token.LessEqual,
      Token.Identifier,
      Token.String,
      Token.Number,
      Token.Number,
      Token.And,
      Token.Or,
      Token.Class,
      Token.False,
      Token.True,
      Token.Fun,
      Token.For,
      Token.If,
      Token.Else,
      Token.Nil,
      Token.Print,
      Token.Return,
      Token.Super,
      Token.This,
      Token.Var,
      Token.While,
      Token.EOF
    ),
    "( ) { } , . - + ; / * ! != = == > >= < <= foo \"foo\" 42 3.14 and or class false true fun for if else nil print return super this var while"
  )
  it should "find multiple tokens over multiple lines" in tokensHaveTypes(
    List(
      Token.LeftParen,
      Token.RightParen,
      Token.LeftBrace,
      Token.RightBrace,
      Token.Comma,
      Token.Dot,
      Token.Minus,
      Token.Plus,
      Token.Semicolon,
      Token.Slash,
      Token.Star,
      Token.Bang,
      Token.BangEqual,
      Token.Equal,
      Token.EqualEqual,
      Token.Greater,
      Token.GreaterEqual,
      Token.Less,
      Token.LessEqual,
      Token.Identifier,
      Token.String,
      Token.Number,
      Token.Number,
      Token.And,
      Token.Or,
      Token.Class,
      Token.False,
      Token.True,
      Token.Fun,
      Token.For,
      Token.If,
      Token.Else,
      Token.Nil,
      Token.Print,
      Token.Return,
      Token.Super,
      Token.This,
      Token.Var,
      Token.While,
      Token.EOF
    ),
    """(
)
{
}
,
.
-
+
;
/
*
!
!=
=
==
>
>=
<
<=
foo
"foo"
42
3.14
and
or
class
false
true
fun
for
if
else
nil
print
return
super
this
var
while"""
  )

  it should "ignore line comments" in tokensHaveTypes(
    List(
      Token.Identifier,
      Token.Equal,
      Token.Number,
      Token.Identifier,
      Token.Equal,
      Token.True,
      Token.EOF
    ),
    """// First line comment
foo = 12.345 // Pointless comment
bar = true
"""
  )

  it should "ignore block comments" in tokensHaveTypes(
    List(
      Token.Identifier,
      Token.Equal,
      Token.Number,
      Token.Identifier,
      Token.Equal,
      Token.True,
      Token.EOF
    ),
    """/* Single line comment */
foo = 12.345 /* Multi line
block
comment*/bar = true
"""
  )

  it should "handle sequential strings" in tokensHaveTypes(
    List(
      Token.String,
      Token.String,
      Token.EOF
    ),
    """ "hello" "world" """
  )

  it should "handle multi line strings" in tokensHaveTypes(
    List(
      Token.Number,
      Token.String,
      Token.Number,
      Token.EOF
    ),
    """1 "I
    am a
    mutiline string
    "
    2
    """
  )

  it should "correctly track token lines and columns" in {
    var expected = List(
      Token(Token.String, "", 2, 5),
      Token(Token.True, "", 6, 12),
      Token(Token.If, "", 7, 1),
      Token(Token.False, "", 9, 3),
      Token(Token.EOF, "", 10, 1)
    )

    var tokensWithoutLexemes = getTokens("""
    "a
  multi line
  string"
  /* block
comment */ true
if
// line comment
  false
""").map({ case Token(t, _, l, c) => Token(t, "", l, c) })

    tokensWithoutLexemes should contain theSameElementsInOrderAs (expected)
  }

}
