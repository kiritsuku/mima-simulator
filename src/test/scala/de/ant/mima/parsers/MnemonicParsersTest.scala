package de.ant.mima.parsers

import org.specs2.specification.Scope
import org.specs2.mutable.SpecificationWithJUnit
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MnemonicParsersTest extends SpecificationWithJUnit {
  
  classOf[MnemonicParsers].getSimpleName should {
    "parse" in {
      "a mnemonic" in new Test {
        val input1 = "ADD 0x123"
        val input2 = "JMP LOOP"
        
        (input1 parseAs mnemonic) === ADD(Adr(0x123))
        (input2 parseAs mnemonic) === JMP(Label("LOOP"))
      }
      "a comment" in new Test {
        val input = "; this is a comment"
        (input parseAs comment) === input
      }
      "a complete line" in new Test {
        val input = "START: LDV 0x100 ; this is a comment"
        
        (input parseAs asmLine) === List(Label("START"), LDV(Adr(0x100)))
      }
      "multiple lines" in new Test {
        val input = """|START: LDV 0x100 ; this is a comment
          |ADD CONST ; STV CONST
          |
          |; empty line
          |EXIT: ADD MS""".stripMargin
        
        (input parseAs asmLines) === List(
          List(Label("START"), LDV(Adr(0x100))),
          List(ADD(Label("CONST"))),
          List(Label("EXIT"), ADD(Label("MS")))
        )
      }
    }
    "throw an error" in {
      "in mnemonic" in new Test {
        val input1 = "add 0x123"
        val input2 = "ADD"
          
        for (i <- List(input1, input2))
          (i parseAs mnemonic) must throwA[IllegalArgumentException]
      }
    }
  }

  trait Test extends Scope with MnemonicParsers
}