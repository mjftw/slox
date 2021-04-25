package slox

import scala.util.matching.Regex

sealed trait TokenType

case object LeftParenToken extends TokenType
case object RightParenToken extends TokenType
case object LeftBraceToken extends TokenType
case object RightBraceToken extends TokenType
case object CommaToken extends TokenType
case object DotToken extends TokenType
case object MinusToken extends TokenType
case object PlusToken extends TokenType
case object SemicolonToken extends TokenType
case object SlashToken extends TokenType
case object StarToken extends TokenType
case object BangToken extends TokenType
case object BangEqualToken extends TokenType
case object EqualToken extends TokenType
case object EqualEqualToken extends TokenType
case object GreaterToken extends TokenType
case object GreaterEqualToken extends TokenType
case object LessToken extends TokenType
case object LessEqualToken extends TokenType
case object IdentifierToken extends TokenType
case object StringToken extends TokenType
case object NumberToken extends TokenType
case object AndToken extends TokenType
case object OrToken extends TokenType
case object ClassToken extends TokenType
case object FalseToken extends TokenType
case object TrueToken extends TokenType
case object FunToken extends TokenType
case object ForToken extends TokenType
case object IfToken extends TokenType
case object ElseToken extends TokenType
case object NilToken extends TokenType
case object PrintToken extends TokenType
case object ReturnToken extends TokenType
case object SuperToken extends TokenType
case object ThisToken extends TokenType
case object VarToken extends TokenType
case object WhileToken extends TokenType
case object EOFToken extends TokenType

case class Token(
    tokenType: TokenType,
    lexeme: String,
    line: Int,
    column: Int
)

object Grammar {
  type Grammar = List[(TokenType, Regex)]

  val newline = Grammar.patternToRegex("""\r{0,1}\n""")
  val whitespace = Grammar.patternToRegex("""\s+""")
  val comment = """(?s)^//.*?(\n|$)(.*)""".r
  val blockComment = Grammar.patternToRegex("""/\*.*\*/""")

  implicit val grammar: Grammar = List(
    (LeftParenToken, """\("""),
    (RightParenToken, """\)"""),
    (LeftBraceToken, """\{"""),
    (RightBraceToken, """\}"""),
    (CommaToken, ""","""),
    (DotToken, """\."""),
    (MinusToken, """-"""),
    (PlusToken, """\+"""),
    (SemicolonToken, """;"""),
    (SlashToken, """/"""),
    (StarToken, """\*"""),
    (BangToken, """!"""),
    (BangEqualToken, """!="""),
    (EqualToken, """="""),
    (EqualEqualToken, """=="""),
    (GreaterToken, """>"""),
    (GreaterEqualToken, """>="""),
    (LessToken, """<"""),
    (LessEqualToken, """<="""),
    (StringToken, """\".*\""""),
    (NumberToken, """[0-9]+"""),
    (NumberToken, """[0-9]+\.[0-9]+"""),
    (AndToken, """and"""),
    (OrToken, """or"""),
    (ClassToken, """class"""),
    (FalseToken, """false"""),
    (TrueToken, """true"""),
    (FunToken, """fun"""),
    (ForToken, """for"""),
    (IfToken, """if"""),
    (ElseToken, """else"""),
    (NilToken, """nil"""),
    (PrintToken, """print"""),
    (ReturnToken, """return"""),
    (SuperToken, """super"""),
    (ThisToken, """this"""),
    (VarToken, """var"""),
    (WhileToken, """while"""),
    (IdentifierToken, """[a-zA-Z_][a-zA-Z_0-9]*""")
  ).map({ case (token, pattern) => (token, patternToRegex(pattern)) })

  def patternToRegex(pattern: String): Regex = s"""(?s)^($pattern)(.*)""".r
}
