package de.ant.mima.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import java.lang.Integer._

trait MimaParsers extends JavaTokenParsers {
  
  final class ParseHandler(in: String) {
    def parseWith[A, B](p: Parser[A])(f: A => B): B =
      parseAll(p, in) match {
        case NoSuccess(msg, input) => error(msg, input)
        case Success(a, _) => f(a)
      }
    def parseAs[A](p: Parser[A]): A =
      parseAll(p, in) match {
        case NoSuccess(msg, input) => error(msg, input)
        case Success(a, _) => a
      }
    
    private def error(msg: String, input: Input) = {
      sys.error("%s (line: %d, pos: %d)" format (msg, input.pos.line, input.pos.column))
    }
  }
  
  implicit def ParseHandler(in: String): ParseHandler = new ParseHandler(in)
  
  override val whiteSpace = """[ \t\x0B\f\r]+""".r

  lazy val const: Parser[Int] = (
      hexNumber ^^ (n => parseInt(n substring 1, 16))
    | wholeNumber ^^ (_.toInt)
  )
  
  lazy val hexNumber = """-?\$[\da-fA-F]+""".r
  
  val comment = "//.*".r
}