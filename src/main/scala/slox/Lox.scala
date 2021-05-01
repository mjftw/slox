package slox

import scala.util.{Try, Success, Failure}
import scala.io.StdIn
import scala.annotation.tailrec

import slox.lexer.Lexer
import slox.parser.{Parser, Expr}
import slox.eval.Eval

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
        Try(os.Path(file)) match {
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
      case null => true
      case _    => false
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

  def runFile(file: os.Path): Unit = {
    Try(os.read(file)) match {
      case Success(code) => {
        val output = evaluate(code)
        println(output)
      }
      case Failure(error) =>
        exitWithMessage(s"Could not open file: ${file}", -1)
    }
  }

  def evaluate(code: String): String = {
    Lexer.scanTokens(code) match {
      case Left(error) => Lexer.formatSyntaxError(error)
      case Right(tokens) => {
        Parser.parseToAst(tokens) match {
          case Left(errors) => errors.map(SyntaxError.formatError).mkString("\n")
          case Right(ast)   => Expr.toString(Eval.evaluate(ast))
        }
      }
    }
  }
}
