package de.ant.mima.parsers

import scala.util.parsing.combinator.JavaTokenParsers

trait MimaParsers extends JavaTokenParsers {
  
  override val whiteSpace = """[ \t\x0B\f\r]+""".r

  lazy val const: Parser[Int] =
    wholeNumber ^^ (_.toInt)
}