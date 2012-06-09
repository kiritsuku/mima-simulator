package de.ant.mima

import de.ant.mima.parsers.RegisterTransferParsers
import scala.{IllegalArgumentException => IAE}
import de.ant.mima.parsers.MnemonicParsers

object Mima extends RegisterTransferParsers with MnemonicParsers {

  import java.lang.Integer._
    
  def dataset(hex: Int) =
    ("%28s" format toBinaryString(parseInt(hex.toString, 16))) replaceAll (" ", "0")

  def hex(signals: Signal*) = {
    val h = Array.fill(28) { '0' }
    signals foreach {
      case Adr(adr) =>
        if (adr > 0xFF)
          throw new IAE("Address '0x%X' is too large. Maximum is 0xFF" format adr)
        val bin = toBinaryString(adr).reverse
        for (i <- 0 until bin.length)
          h(i) = bin.charAt(i)
      case o @ Op(op) =>
        if (op > 7)
          throw new IAE("Op-Code '%d' is illegal." format op)
        val bin = toBinaryString(op).reverse
        for (i <- 0 until bin.length)
          h(o.pos+i) = bin.charAt(i)
      case s =>
        h(s.pos) = '1'
    }
    "%07X" format parseInt(h.mkString.reverse, 2)
  }
    
  def interpret(str: String) {
    (str parseAs lines) {
      case xs =>
        val x = xs map (_ flatMap handleCommand) map (hex(_: _*))
        x foreach println
    }
  }
  
  def mn(str: String) {
    (str parseAs asmLine) {
      case m => println(m)
    }
  }
}

abstract class Signal(val pos: Int)
case object Ar extends Signal(27)
case object Aw extends Signal(26)
case object X extends Signal(25)
case object Y extends Signal(24)
case object Z extends Signal(23)
case object E extends Signal(22)
case object Pr extends Signal(21)
case object Pw extends Signal(20)
case object Ir extends Signal(19)
case object Iw extends Signal(18)
case object Dr extends Signal(17)
case object Dw extends Signal(16)
case object S extends Signal(15)
case object C2 extends Signal(14)
case object C1 extends Signal(13)
case object C0 extends Signal(12)
case object R extends Signal(11)
case object W extends Signal(10)
case object Res1 extends Signal(9)
case object Res0 extends Signal(8)
case class Adr(adr: Int) extends Signal(0)
case class Op(op: Int) extends Signal(12)