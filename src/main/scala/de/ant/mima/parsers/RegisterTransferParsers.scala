package de.ant.mima.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import scala.{IllegalArgumentException => IAE}
import de.ant.mima._

trait Command
case class Arrow(w: Signal, r: Signal) extends Command
case class Assign(s: Signal) extends Command

trait RegisterTransferParsers extends MimaParsers {
  
  lazy val lines: Parser[List[List[Command]]] =
    repsep(commands, "\n") ^^ { _ filter (_.nonEmpty) }
  
  lazy val commands: Parser[List[Command]] =
    repsep(command, ";") <~ opt(comment)
  
  lazy val command: Parser[Command] =
    (ident <~ ("->" | "=")) ~ (ident | const) ^? {
      case "ALU" ~ (n: Int) =>
        if (n > 7)
          throw new IAE("ALU operation '%s' is unknown" format n)
        Assign(Op(n))
      case "ALU" ~ (s: String) =>
        Assign(Op(aluOp(s)))
      case reg ~ 1 =>
        Assign(signal(reg, assignSignals))
      case wReg ~ (rReg: String) =>
        Arrow(signal(wReg, writeSignals), signal(rReg, readSignals))
    }
    
  protected def handleCommand: Command => List[Signal] = {
    case Arrow(w, r) => List(w, r)
    case Assign(s) => List(s)
  }
    
  private val writeSignals = Map[String, Signal](
    "Akku" -> Aw, "Z" -> Z, "E" -> E, "IAR" -> Pw, "IR" -> Iw, "SDR" -> Dw
  )
  private val readSignals = Map[String, Signal](
    "Akku" -> Ar, "X" -> X, "Y" -> Y, "IAR" -> Pr, "IR" -> Ir, "SDR" -> Dr, "SAR" -> S
  )
  private val assignSignals = Map[String, Signal](
    "R" -> R, "W" -> W, "D" -> D, "B" -> B
  )
  private val aluOps = Map[String, Int](
    "nop" -> 0, "add" -> 1, "rot" -> 2, "and" -> 3, "or" -> 4, "xor" -> 5, "cpl" -> 7, "cmp" -> 8
  )
  
  private def aluOp(s: String) =
    aluOps.getOrElse(s, throw new IAE("ALU operation '%s' is unknown" format s))
  
  private def signal(reg: String, signals: Map[String, Signal]) =
    signals.getOrElse(reg, throw new IAE("Register '%s' is unknown" format reg))
  
}