package de.ant.mima.parsers

import scala.{IllegalArgumentException => IAE}

trait MnemonicParsers extends MimaParsers {
  
  val Memory: Int = (1 << 20)-1
  
  override val comment = ";.*".r
  
  lazy val asmLines: Parser[List[List[Asm]]] =
    repsep(asmLine, "\n") ^^ { _ filter (_.nonEmpty) }
    
  lazy val asmLine: Parser[List[Asm]] =
    opt(label | (mnemonic ^^ (List(_)))) <~ opt(comment) ^^ { _.flatten.toList }
  
  lazy val label: Parser[List[Asm]] =
    (ident <~ ":") ~ opt(mnemonic) ^^ {
      case i ~ None => List(Label(i))
      case i ~ Some(mn) => List(Label(i), mn)
    }

  lazy val mnemonic: Parser[Asm] =
    ident ~ opt(const | ident) ^? {
      case "HALT" ~ None => HALT
      case "NOT" ~ None => NOT
      case "RAR" ~ None => RAR
      case m ~ Some(v: Int) =>
        if (v > Memory)
          throw new IAE("Value '%d' is too large. Maximum is '%d'" format (v, Memory))
        mnemonics.getOrElse(m, throw new IAE("The mnemonic '%s' is unknown" format m))(Adr(v))
      case m ~ Some(i: String) =>
        mnemonics.getOrElse(m, throw new IAE("The mnemonic '%s' is unknown" format m))(Label(i))
      case m ~ None =>
        throw new IAE("Missing operand in mnemonic '%s'" format m)
    }
  
  private val mnemonics = Map(
    "LDC" -> LDC, "LDV" -> LDV, "STV" -> STV, "ADD" -> ADD, "AND" -> AND,
    "OR" -> OR, "XOR" -> XOR, "EQL" -> EQL, "JMP" -> JMP, "JMN" -> JMN
  )
}

trait Asm
case class Label(s: String) extends Asm
case class Adr(i: Int) extends Asm

abstract class Mnemonic(val op: Int) extends Asm {
  
  val fetchPhase = """
    IAR -> SAR; IAR -> X; R = 1
    E -> Y; R = 1
    ALU = add; R = 1
    Z -> IAR
    SDR -> IR
    //D = 1
  """
  
  def eval: String
}
case class LDC(i: Asm) extends Mnemonic(0) {
  def eval = "IR -> Akku"
}
case class LDV(a: Asm) extends Mnemonic(1) {
  def eval = """
    IR -> SAR; R = 1
    R = 1
    R = 1
    SDR -> Akku
  """
}
case class STV(a: Asm) extends Mnemonic(2) {
  def eval = """
    Akku -> SDR
    IR -> SAR; W = 1
    W = 1
    W = 1
  """
}
case class ADD(a: Asm) extends Mnemonic(3) {
  def eval = """
    ΙR -> SAR; R = 1
    Akku -> X; R = 1
    R = 1
    SDR -> Y
    ALU = add
    Z -> Akku
  """
}
case class AND(a: Asm) extends Mnemonic(4) {
  def eval = """
    ΙR -> SAR; R = 1
    Akku -> X; R = 1
    R = 1
    SDR -> Y
    ALU = and
    Z -> Akku
  """
}
case class OR(a: Asm) extends Mnemonic(5) {
  def eval = """
    ΙR -> SAR; R = 1
    Akku -> X; R = 1
    R = 1
    SDR -> Y
    ALU = or
    Z -> Akku
  """
}
case class XOR(a: Asm) extends Mnemonic(6) {
  def eval = """
    ΙR -> SAR; R = 1
    Akku -> X; R = 1
    R = 1
    SDR -> Y
    ALU = xor
    Z -> Akku
  """
}
case class EQL(a: Asm) extends Mnemonic(7) {
  def eval = """
    IR -> SAR; R = 1
    Akku -> X; R = 1
    R = 1
    SDR -> Y
    ALU = cmp
    Z -> Akku
  """
}
case class JMP(a: Asm) extends Mnemonic(8) {
  def eval = "IR -> IAR"
}
case class JMN(a: Asm) extends Mnemonic(9) {
  // TODO save 0x800000 and 0 to memory
  def eval = """
    IR -> X // X = 0x800000 (2^23)
    Akku -> Y // Y = value to check if negative
    ALU = and // Z = 0 if value is positive =: res
    // STV res
    // another fetch phase to load 0x800000 to IR
    IR -> X // X = 0x800000
    // LDV res
    IR -> Y // Y = previous computed Z
    ALU = cmp // Z = -1 if value is negative, 0 otherwise
    Z -> X
    E -> Y
    ALU = add // Z = 0 if value is negative, 1 otherwise
    Z -> X
    IAR -> Y
    ALU = add
    ---------------------------------------------
    Z -> IAR; Z -> X; R = 1 // IAR increased by 1 if  value is positive
    E -> Y; R = 1
    ALU = add; R = 1
    Z -> IAR
    SDR -> IR
  """
}
case object HALT extends Mnemonic(0xF0) {
  def eval = ""
}
case object NOT extends Mnemonic(0xF1) {
  def eval = ""
}
case object RAR extends Mnemonic(0xF2) {
  def eval = ""
}