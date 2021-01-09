import $ivy.`com.beachape::enumeratum:1.6.1`
import $ivy.`org.scalatest::scalatest:3.2.2`
import $file.Util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import enumeratum.values._
import scala.annotation.meta.param

val STATIC_INPUT = Some(1)

sealed abstract class OpCode(val value: Int, val length: Int)
    extends IntEnumEntry
object OpCode extends IntEnum[OpCode] {
  case object Halt extends OpCode(99, 1)
  case object Add extends OpCode(1, 4)
  case object Mult extends OpCode(2, 4)
  case object Input extends OpCode(3, 2)
  case object Output extends OpCode(4, 2)

  val values = findValues // required for enums
}
import OpCode._

def parameterValue(
    parameter: Int,
    parameterMode: Int,
    memory: ListBuffer[Int]
): Int = {
  parameterMode match {
    case 0 => memory(parameter) // read address
    case 1 => parameter // immediate mode, use value directly
  }
}

def parseInput(rawInput: String): ListBuffer[Int] =
  rawInput.split(",").map(_.toInt).to(ListBuffer)

case class Instruction(
    opCode: OpCode,
    paramModes: Map[Int, Int],
    parameters: List[Int]
) {
  def parameterValues(memory: ListBuffer[Int]): List[Int] = {
    parameters.zipWithIndex.map { case (parameter, i) =>
      parameterValue(parameter, paramModes.get(i).getOrElse(0), memory)
    }
  }
}

def parseInstruction(values: List[Int]): Instruction = {
  val instructionCode = values.head
  val opCode = OpCode.withValue(instructionCode % 100)
  Instruction(
    opCode = opCode,
    paramModes = (0 to 2).map { position =>
      position -> instructionCode / math.pow(10, position + 2).toInt % 10
    }.toMap,
    parameters = values.slice(1, opCode.length)
  )
}

def groupOps(input: ListBuffer[Int]): Iterator[Instruction] = {
  var position = 0
  Iterator
    .continually {
      // parse next instruction and advance the position pointer the right number of spots
      val instruction =
        parseInstruction(input.slice(position, position + 4).toList)
      position = position + instruction.opCode.length
      instruction
    }
    .takeWhile(_.opCode != Halt && position <= input.size)
}

def runInstruction(instruction: Instruction, memory: ListBuffer[Int]): Unit = {
  val paramValues = instruction.parameterValues(memory)
  instruction.opCode match {
    case Add => {
      memory(instruction.parameters(2)) = paramValues(0) + paramValues(1)
    }
    case Mult => {
      memory(instruction.parameters(2)) = paramValues(0) * paramValues(1)
    }
    case Input => {
      memory(instruction.parameters(0)) =
        STATIC_INPUT.getOrElse(readLine("Input? ").toInt)
    }
    case Output => {
      println(s"Output: ${paramValues(0)}")
    }
  }
}

def runProgram(rawInput: String): ListBuffer[Int] = {
  val memory = parseInput(rawInput)
  groupOps(memory).foreach(runInstruction(_, memory))
  memory
}

// unit tests ---------------

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
        runProgram(input) should be(parseInput(expected))
      }
    }
  }

  "parameterValue" should "mode 0 return memory address value" in {
    parameterValue(0, 0, ListBuffer(10, 20, 30, 40)) should be(10)
  }

  it should "mode 1 return the actual value directly" in {
    parameterValue(-10, 1, ListBuffer(10, 20, 30, 40)) should be(-10)
  }

  "Instruction.parameterValues()" should "respect paramModes" in {
    val i = Instruction(Add, Map(0 -> 1, 1 -> 0, 2 -> 0), List(-1, 0, 1))
    i.parameterValues(ListBuffer(1, 2, 3, 4)) should be(List(-1, 1, 2))
  }
}

(new Spec()).execute()

//// run actual program

val rawInput = Util.inputForDay(5).head
runProgram(rawInput)
