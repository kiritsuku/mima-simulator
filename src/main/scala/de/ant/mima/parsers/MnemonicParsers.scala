package de.ant.mima.parsers

import scala.{IllegalArgumentException => IAE}

trait MnemonicParsers extends MimaParsers {
  
  val Memory: Int = (1 << 20)-1

  lazy val mnemonic: Parser[Mnemonic] =
    ident ~ opt(const) ^^ {
      case "HALT" ~ None => HALT
      case "NOT" ~ None => NOT
      case "RAR" ~ None => RAR
      case m ~ Some(v) =>
        if (v > Memory)
          throw new IAE("Value '%d' is too large. Maximum is '%d'" format (v, Memory))
        mnemonics.getOrElse(m, throw new IAE("The mnemonic '%s' is unknown" format m))(v)
    }
  
  private val mnemonics = Map(
    "LDC" -> LDC, "LDV" -> LDV, "STV" -> STV, "ADD" -> ADD, "AND" -> AND,
    "OR" -> OR, "XOR" -> XOR, "EQL" -> EQL, "JMP" -> JMP, "JMN" -> JMN
  )
}

abstract class Mnemonic(val op: Int)
case class LDC(i: Int) extends Mnemonic(0)
case class LDV(a: Int) extends Mnemonic(1)
case class STV(a: Int) extends Mnemonic(2)
case class ADD(a: Int) extends Mnemonic(3)
case class AND(a: Int) extends Mnemonic(4)
case class OR(a: Int) extends Mnemonic(5)
case class XOR(a: Int) extends Mnemonic(6)
case class EQL(a: Int) extends Mnemonic(7)
case class JMP(a: Int) extends Mnemonic(8)
case class JMN(a: Int) extends Mnemonic(9)
case object HALT extends Mnemonic(0xF0)
case object NOT extends Mnemonic(0xF1)
case object RAR extends Mnemonic(0xF2)