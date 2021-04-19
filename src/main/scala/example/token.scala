import scala.util.matching.Regex
import scala.annotation.tailrec

sealed trait Token

case class IfToken() extends Token
case class IntToken() extends Token
case class VarToken() extends Token

object Parse {
  val lexicon: List[(String, Token)] = List(
    ("""if""", IfToken()),
    ("""[0-9]+""", IntToken()),
    ("""[a-zA-Z]{1}[\w_]*""", VarToken())
  )

  def reLexicon(lexicon: List[(String, Token)]): List[(Regex, Token)] = for (
    (re, token) <- lexicon
  ) yield (s"""^($re)(.*)""".r, token)

  def takeToken(
      input: String,
      lexicon: List[(Regex, Token)]
  ): Option[(Token, String)] = {
    lexicon match {
      case Nil => None
      case (pattern, token) :: lexiconRest =>
        input match {
          case pattern(matched, restInput) => Some((token, restInput))
          case _                           => takeToken(input, lexiconRest)
        }
    }

  }

  // TODO
  // @tailrec
  // def tokenise(
  //     input: String,
  //     lexicon: Map[Regex, Token],
  //     tokens: List[Token] = Nil
  // ): List[Token]
}
