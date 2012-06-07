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
        val input1 = "ADD $123"
        val input2 = "JMP LOOP"
        
        ADD(Adr(0x123)) === (input1 parseAs mnemonic)
        JMP(Label("LOOP")) === (input2 parseAs mnemonic)
      }
      "a comment" in new Test {
        val input = "; this is a comment"
        input === (input parseAs comment)
      }
      /*"a macro" in new Test {
        val input = "THIS = $300"
        
        (input parseAs macro) === List(Label("THIS"), Macro(0x300))
      }*/
      "a complete line" in new Test {
        val input = "START: LDV $100 ; this is a comment"
        
        List(Label("START"), LDV(Adr(0x100))) === (input parseAs asmLine)
      }
      "multiple lines" in new Test {
        val input = """
          * = $10
          E_CONST: DS $100
                   DS $200
                   DS $300
          START: LDV $100 ; this is a comment
          ADD CONST ; STV CONST

          ; empty line
          EXIT: ADD MS"""
        
        List(
          List(LoadPoint(0x10)),
          List(Label("E_CONST", Some(Storage(List(0x100, 0x200, 0x300))))),
          List(Label("START"), LDV(Adr(0x100))),
          List(ADD(Label("CONST"))),
          List(Label("EXIT"), ADD(Label("MS")))
        ) === (input parseAs asmLines)
      }
      "a load point" in new Test {
        val input = "* = $10"
       
         LoadPoint(0x10) === (input parseAs loadPoint)
      }
    }
    "throw an error" in {
      "in mnemonic" in new Test {
        val input1 = "add $123"
        val input2 = "ADD"
          
        for (i <- List(input1, input2))
          (i parseAs mnemonic) must throwA[IllegalArgumentException]
      }
    }
  }

  trait Test extends Scope with MnemonicParsers
}