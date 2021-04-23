import os.Path
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object Lox {
  val usage: String = """
Usage: slox [args] [script]

If no script is supplied the REPL run.
  
Args:
-h, --help:                       Show this text
"""

  def main(args: Array[String]): Unit = {
    args match {
      case Array()                       => runRepl()
      case Array("-h") | Array("--help") => exitWithMessage(usage, 0)
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
    print(message)
    System.exit(status)
  }

  def runRepl() = ???

  def runFile(file: Path) = ???
}
