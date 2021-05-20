package slox.lexer

import scala.util.matching.Regex

sealed trait TokenType

object Token {
  case object LeftParen extends TokenType
  case object RightParen extends TokenType
  case object LeftBrace extends TokenType
  case object RightBrace extends TokenType
  case object Comma extends TokenType
  case object Dot extends TokenType
  case object Minus extends TokenType
  case object Plus extends TokenType
  case object Semicolon extends TokenType
  case object Slash extends TokenType
  case object Star extends TokenType
  case object Bang extends TokenType
  case object BangEqual extends TokenType
  case object Equal extends TokenType
  case object EqualEqual extends TokenType
  case object Greater extends TokenType
  case object GreaterEqual extends TokenType
  case object Less extends TokenType
  case object LessEqual extends TokenType
  case object Identifier extends TokenType
  case object String extends TokenType
  case object Number extends TokenType
  case object And extends TokenType
  case object Or extends TokenType
  case object Class extends TokenType
  case object False extends TokenType
  case object True extends TokenType
  case object Fun extends TokenType
  case object For extends TokenType
  case object If extends TokenType
  case object Else extends TokenType
  case object Nil extends TokenType
  case object Print extends TokenType
  case object Return extends TokenType
  case object Super extends TokenType
  case object This extends TokenType
  case object Var extends TokenType
  case object While extends TokenType
  case object EOF extends TokenType
}

case class Token(
    tokenType: TokenType,
    lexeme: String,
    line: Int,
    column: Int
)

object LexicalGrammar {
  type LexicalGrammar = List[(TokenType, Regex)]

  val newline = patternToRegex("""\r{0,1}\n""")
  val whitespace = patternToRegex("""\s+""")
  val comment = """(?s)^//.*?(\n|$)(.*)""".r
  val blockComment = patternToRegex("""\/\*.*?\*\/""")

  implicit val grammar: LexicalGrammar = List(
    (Token.LeftParen, """\("""),
    (Token.RightParen, """\)"""),
    (Token.LeftBrace, """\{"""),
    (Token.RightBrace, """\}"""),
    (Token.Comma, ""","""),
    (Token.Dot, """\."""),
    (Token.Minus, """-"""),
    (Token.Plus, """\+"""),
    (Token.Semicolon, """;"""),
    (Token.Slash, """/"""),
    (Token.Star, """\*"""),
    (Token.Bang, """!"""),
    (Token.BangEqual, """!="""),
    (Token.Equal, """="""),
    (Token.EqualEqual, """=="""),
    (Token.Greater, """>"""),
    (Token.GreaterEqual, """>="""),
    (Token.Less, """<"""),
    (Token.LessEqual, """<="""),
    (Token.String, """\".*?\""""),
    (Token.Number, """[0-9]+"""),
    (Token.Number, """[0-9]+\.[0-9]+"""),
    (Token.And, """and"""),
    (Token.Or, """or"""),
    (Token.Class, """class"""),
    (Token.False, """false"""),
    (Token.True, """true"""),
    (Token.Fun, """fun"""),
    (Token.For, """for"""),
    (Token.If, """if"""),
    (Token.Else, """else"""),
    (Token.Nil, """nil"""),
    (Token.Print, """print"""),
    (Token.Return, """return"""),
    (Token.Super, """super"""),
    (Token.This, """this"""),
    (Token.Var, """var"""),
    (Token.While, """while"""),
    (Token.Identifier, """[a-zA-Z_][a-zA-Z_0-9]*""")
  ).map({ case (token, pattern) => (token, patternToRegex(pattern)) })

  def patternToRegex(pattern: String): Regex = s"""(?s)^($pattern)(.*)""".r
}
