package slox

import org.scalatest.flatspec.AnyFlatSpec
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
}
