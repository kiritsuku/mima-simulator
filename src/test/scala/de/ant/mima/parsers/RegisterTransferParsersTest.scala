package de.ant.mima.parsers

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import de.ant.mima._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RegisterTransferParsersTest extends SpecificationWithJUnit {

  classOf[RegisterTransferParsers].getSimpleName should {
    "parse" in {
      "a command" in new Test {
        val input1 = "IAR -> SAR"
        val input2 = "R = 1"
        val input3 = "ALU = add"
        val input4 = "ALU = 5"
          
        Arrow(Pw, S) === (input1 parseAs command)
        Assign(R) === (input2 parseAs command)
        Assign(Op(1)) === (input3 parseAs command)
        Assign(Op(5)) === (input4 parseAs command)
        Assign(Op(5)) === (input4 parseAs command)
      }
      "a comment" in new Test {
        val input = "// this is a comment"
          
        input === (input parseAs comment)
      }
      "a command and a comment in same line" in new Test {
        val input = "IAR -> SAR // this is a comment"
        
        List(Arrow(Pw, S)) === (input parseAs commands)
      }
      "multiple commands" in new Test {
        val input = "IAR -> SAR; IAR -> X; R = 1"
        
        List(Arrow(Pw, S), Arrow(Pw, X), Assign(R)) === (input parseAs commands)
      }
      "multiple lines" in new Test {
        val input = """
          IAR -> SAR; R = 1
          // comment
          
          ALU = add
          SDR -> IR
          D = 1
        """
        
        val expected = List(
          List(Arrow(Pw, S), Assign(R)),
          List(Assign(Op(1))),
          List(Arrow(Dw, Ir)),
          List(Assign(D))
        )
        expected === (input parseAs lines)
      }
    }
    "throw an error" in {
      "in command" in new Test {
        val input1 = "XXX -> SAR"
        val input2 = "ALU = 123"
        val input3 = "ALU = not_found"
        
        (input1 parseAs command) must throwA[IllegalArgumentException]
        (input2 parseAs command) must throwA[IllegalArgumentException]
        (input3 parseAs command) must throwA[IllegalArgumentException]
      }
    }
  }
  
  trait Test extends Scope with RegisterTransferParsers
}