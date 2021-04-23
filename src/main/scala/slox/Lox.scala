import os.Path
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.io._
import scala.annotation.tailrec

object Lox {
  val usage: String = {
    """
Usage: slox [args] [script]

If no script is supplied the REPL run.
  
Args:
-h, --help:                       Show this text
"""
  }

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

  def runRepl(prompt: String = "> "): Unit = {
    println("Starting REPL. Press Ctrl-D to exit...")

    def isExit(s: String): Boolean = s match {
      case null | "exit" => true
      case _             => false
    }

    @tailrec
    def loop(): Unit = {
      print(prompt)

      StdIn.readLine() match {
        case msg if isExit(msg) => println("\nGoodbye!")
        case code => {
          val output = evaluate(code)
          println(s"\n${output}")
          loop()
        }
      }
    }

    loop()
  }

  def runFile(file: Path) = ???

  def evaluate(code: String): String = s"Mock eval of code ${code}"
}
