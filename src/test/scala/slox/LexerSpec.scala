package slox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues
import org.scalatest.prop.TableDrivenPropertyChecks
import slox.lexer._

class LexerSpec extends AnyFlatSpec with Matchers with EitherValues with TableDrivenPropertyChecks {
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

  "scanTokens" should "always put an EOF token last" in firstTokenHasType(EOFToken, "")

  it should "find LeftParen" in firstTokenHasType(LeftParenToken, "(")
  it should "find RightParen" in firstTokenHasType(RightParenToken, ")")
  it should "find LeftBrace" in firstTokenHasType(LeftBraceToken, "{")
  it should "find RightBrace" in firstTokenHasType(RightBraceToken, "}")
  it should "find Comma" in firstTokenHasType(CommaToken, ",")
  it should "find Dot" in firstTokenHasType(DotToken, ".")
  it should "find Minus" in firstTokenHasType(MinusToken, "-")
  it should "find Plus" in firstTokenHasType(PlusToken, "+")
  it should "find Semicolon" in firstTokenHasType(SemicolonToken, ";")
  it should "find Slash" in firstTokenHasType(SlashToken, "/")
  it should "find Star" in firstTokenHasType(StarToken, "*")
  it should "find Bang" in firstTokenHasType(BangToken, "!")
  it should "find BangEqual" in firstTokenHasType(BangEqualToken, "!=")
  it should "find Equal" in firstTokenHasType(EqualToken, "=")
  it should "find EqualEqual" in firstTokenHasType(EqualEqualToken, "==")
  it should "find Greater" in firstTokenHasType(GreaterToken, ">")
  it should "find GreaterEqual" in firstTokenHasType(GreaterEqualToken, ">=")
  it should "find Less" in firstTokenHasType(LessToken, "<")
  it should "find LessEqual" in firstTokenHasType(LessEqualToken, "<=")
  it should "find Identifier" in firstTokenHasType(IdentifierToken, "foo")
  it should "find String" in firstTokenHasType(StringToken, "\"foo\"")
  it should "find Number without decimal point" in firstTokenHasType(NumberToken, "42")
  it should "find Number with decimal point" in firstTokenHasType(NumberToken, "3.14")
  it should "find And" in firstTokenHasType(AndToken, "and")
  it should "find Or" in firstTokenHasType(OrToken, "or")
  it should "find Class" in firstTokenHasType(ClassToken, "class")
  it should "find False" in firstTokenHasType(FalseToken, "false")
  it should "find True" in firstTokenHasType(TrueToken, "true")
  it should "find Fun" in firstTokenHasType(FunToken, "fun")
  it should "find For" in firstTokenHasType(ForToken, "for")
  it should "find If" in firstTokenHasType(IfToken, "if")
  it should "find Else" in firstTokenHasType(ElseToken, "else")
  it should "find Nil" in firstTokenHasType(NilToken, "nil")
  it should "find Print" in firstTokenHasType(PrintToken, "print")
  it should "find Return" in firstTokenHasType(ReturnToken, "return")
  it should "find Super" in firstTokenHasType(SuperToken, "super")
  it should "find This" in firstTokenHasType(ThisToken, "this")
  it should "find Var" in firstTokenHasType(VarToken, "var")
  it should "find While" in firstTokenHasType(WhileToken, "while")

  it should "capture funky string text" in {
    val str = "\"j!I_A.w,@W~R+N=y-5~6[a]}{|\\><£$%^&*:;#`\""
    firstTokenHasLexeme(str, str)
  }

  it should "find multiple tokens in a single line" in tokensHaveTypes(
    List(
      LeftParenToken,
      RightParenToken,
      LeftBraceToken,
      RightBraceToken,
      CommaToken,
      DotToken,
      MinusToken,
      PlusToken,
      SemicolonToken,
      SlashToken,
      StarToken,
      BangToken,
      BangEqualToken,
      EqualToken,
      EqualEqualToken,
      GreaterToken,
      GreaterEqualToken,
      LessToken,
      LessEqualToken,
      IdentifierToken,
      StringToken,
      NumberToken,
      NumberToken,
      AndToken,
      OrToken,
      ClassToken,
      FalseToken,
      TrueToken,
      FunToken,
      ForToken,
      IfToken,
      ElseToken,
      NilToken,
      PrintToken,
      ReturnToken,
      SuperToken,
      ThisToken,
      VarToken,
      WhileToken,
      EOFToken
    ),
    "( ) { } , . - + ; / * ! != = == > >= < <= foo \"foo\" 42 3.14 and or class false true fun for if else nil print return super this var while"
  )
  it should "find multiple tokens over multiple lines" in tokensHaveTypes(
    List(
      LeftParenToken,
      RightParenToken,
      LeftBraceToken,
      RightBraceToken,
      CommaToken,
      DotToken,
      MinusToken,
      PlusToken,
      SemicolonToken,
      SlashToken,
      StarToken,
      BangToken,
      BangEqualToken,
      EqualToken,
      EqualEqualToken,
      GreaterToken,
      GreaterEqualToken,
      LessToken,
      LessEqualToken,
      IdentifierToken,
      StringToken,
      NumberToken,
      NumberToken,
      AndToken,
      OrToken,
      ClassToken,
      FalseToken,
      TrueToken,
      FunToken,
      ForToken,
      IfToken,
      ElseToken,
      NilToken,
      PrintToken,
      ReturnToken,
      SuperToken,
      ThisToken,
      VarToken,
      WhileToken,
      EOFToken
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
      IdentifierToken,
      EqualToken,
      NumberToken,
      IdentifierToken,
      EqualToken,
      TrueToken,
      EOFToken
    ),
    """// First line comment
foo = 12.345 // Pointless comment
bar = true
"""
  )

  it should "ignore block comments" in tokensHaveTypes(
    List(
      IdentifierToken,
      EqualToken,
      NumberToken,
      IdentifierToken,
      EqualToken,
      TrueToken,
      EOFToken
    ),
    """/* Single line comment */
foo = 12.345 /* Multi line
block
comment*/bar = true
"""
  )

  it should "handle sequential strings" in tokensHaveTypes(
    List(
      StringToken,
      StringToken,
      EOFToken
    ),
    """ "hello" "world" """
  )

  it should "handle multi line strings" in tokensHaveTypes(
    List(
      NumberToken,
      StringToken,
      NumberToken,
      EOFToken
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
      Token(StringToken, "", 2, 5),
      Token(TrueToken, "", 6, 12),
      Token(IfToken, "", 7, 1),
      Token(FalseToken, "", 9, 3),
      Token(EOFToken, "", 10, 1)
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
