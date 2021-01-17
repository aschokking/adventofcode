import $ivy.`com.beachape::enumeratum:1.6.1`
import $ivy.`org.scalatest::scalatest:3.2.2`
import $file.IntCode

import IntCode._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable.{ListBuffer, Queue}

class Spec extends AnyFlatSpec {
  val testCases = Seq(
    ("1,0,0,0,99", "2,0,0,0,99"),
    ("2,3,0,3,99", "2,3,0,6,99"),
    ("2,4,4,5,99,0", "2,4,4,5,99,9801"),
    ("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99")
  )

  "Computer" should "pass day2 test cases" in {
    testCases.map {
      case (input, expected) => {
        runProgram(input).memory should be(parseInput(expected))
      }
    }
  }

  "parameterValue" should "mode 0 return memory address value" in {
    val memory = new Memory()
    memory.write(0, 10)
    memory.write(1, 20)
    parameterValue(0, ParamMode.MemAddr, memory) should be(10)
  }

  it should "mode 1 return the actual value directly" in {
    val memory = new Memory()
    memory.write(0, 10)
    memory.write(1, 20)
    parameterValue(-10, ParamMode.Direct, memory) should be(-10)
  }

  "Instruction.parameterValues()" should "respect paramModes" in {
    val i = Instruction(
      OpCode.Add,
      Map(
        0 -> ParamMode.Direct,
        1 -> ParamMode.MemAddr,
        2 -> ParamMode.MemAddr
      ),
      List(-1, 0, 1)
    )
    val memory = new Memory()
    memory.write(0, 1)
    memory.write(1, 2)
    memory.write(2, 3)
    i.parameterValues(memory, 0) should be(List(-1, 1, 2))
  }

  "day 9 test case 1" should "make a copy of itself as output" in {
    val program = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    val output = Queue[BigInt]()
    runProgram(program, outputBuffer = output)
    output.mkString(",") should be(program)
  }

  "day 9 test case 2" should "output 16 digit number" in {
    val program = "1102,34915192,34915192,7,4,7,99,0"
    val output = Queue[BigInt]()
    runProgram(program, outputBuffer = output)
    output.dequeue().toString().size should be(16)
  }

  "day 9 test case 3" should "output big number in middle" in {
    val program = "104,1125899906842624,99"
    val output = Queue[BigInt]()
    runProgram(program, outputBuffer = output)
    output.dequeue() should be(BigInt("1125899906842624"))
  }

  "Memory" should "extend" in {
    val memory = new Memory(ListBuffer())
    memory.read(0) should be(0)
    memory.read(10) should be(0)
  }
}

class Spec2 extends AnyFlatSpec {

  "day 9 test case 1" should "make a copy of itself as output" in {
    val program = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    val output = Queue[BigInt]()
    runProgram(program, outputBuffer = output)
    output.mkString(",") should be(program)
  }

}

(new Spec2()).execute()
