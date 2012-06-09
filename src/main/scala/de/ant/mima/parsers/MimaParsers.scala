package de.ant.mima.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import java.lang.Integer._

trait MimaParsers extends JavaTokenParsers {
  
  final class ParseHandler(in: String) {
    def parseAs[A, B](p: Parser[A])(f: A => B): B =
      parseAll(p, in) match {
        case NoSuccess(msg, _) => sys.error(msg)
        case Success(a, _) => f(a)
      }
  }
  
  implicit def ParseHandler(in: String): ParseHandler = new ParseHandler(in)
  
  override val whiteSpace = """[ \t\x0B\f\r]+""".r

  lazy val const: Parser[Int] = (
      hexNumber ^^ (n => parseInt(n substring 2, 16))
    | wholeNumber ^^ (_.toInt)
  )
  
  lazy val hexNumber = """-?(0x)?[\da-fA-F]+""".r
}