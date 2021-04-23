import os.Path
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object Lox extends App {
  override def main(args: Array[String]): Unit = {
    val usage: String = """Usage: slox [script]"""

    args match {
      case Array() => runRepl()
      case Array(file) =>
        Try(Path(file)) match {
          case Success(path) => runFile(path)
          case Failure(err) =>
            exitWithMessage(s"${usage}\n\n${err.getMessage}", -1)
        }
      case _ => exitWithMessage(usage, 64)
    }
  }
  def exitWithMessage(message: String, status: Int): Unit = {
    println(message)
    System.exit(status)
  }

  def runRepl() = ???

  def runFile(file: Path) = ???
}
