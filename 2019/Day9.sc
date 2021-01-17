import $ivy.`com.beachape::enumeratum:1.6.1`
import $ivy.`org.scalatest::scalatest:3.2.2`
import $file.Util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable.{ListBuffer, Queue}
import scala.io.StdIn.readLine
import enumeratum.values._
import scala.annotation.meta.param

val rawInput = Util.inputForDay(9).head

sealed abstract class OpCode(val value: Int, val length: Int)
    extends IntEnumEntry
object OpCode extends IntEnum[OpCode] {
  case object Halt extends OpCode(99, 1)
  case object Add extends OpCode(1, 4)
  case object Mult extends OpCode(2, 4)
  case object Input extends OpCode(3, 2)
  case object Output extends OpCode(4, 2)
  case object JumpIfTrue extends OpCode(5, 3)
  case object JumpIfFalse extends OpCode(6, 3)
  case object LessThan extends OpCode(7, 4)
  case object Equals extends OpCode(8, 4)
  case object SetBase extends OpCode(9, 2)

  val values = findValues // required for enums
}
import OpCode._

class Memory(val memory: ListBuffer[BigInt]) {
  def read(address: Int): BigInt = {
    validateAddress(address)
    memory(address)
  }

  def validateAddress(address: Int): Unit = {
    if (address < 0) {
      throw new Exception("Address ${address} below 0")
    }

    if (address >= memory.size) {
      // add 0s to pad until reaching memoryMemory
      memory.appendAll((0 to (address - memory.size)).map(_ => 0))
    }
  }

  def write(address: Int, value: BigInt): Unit = {
    validateAddress(address)
    memory(address) = value
  }
}

def parameterValue(
    parameter: BigInt,
    parameterMode: BigInt,
    memory: Memory,
    relativeBase: Int
): BigInt = {
  parameterMode.toInt match {
    case 0 => memory.read(parameter.toInt) // read address
    case 1 => parameter // immediate mode, use value directly
    case 2 => memory.read((relativeBase + parameter).toInt)
  }
}

def parseInput(rawInput: String): ListBuffer[BigInt] =
  rawInput.split(",").map(str => BigInt(str)).to(ListBuffer)

case class Instruction(
    opCode: OpCode,
    paramModes: Map[Int, BigInt],
    parameters: List[BigInt]
) {
  def parameterValues(
      memory: Memory,
      relativeBase: Int
  ): List[BigInt] = {
    parameters.zipWithIndex.map { case (parameter, i) =>
      parameterValue(
        parameter,
        paramModes.get(i).getOrElse(0),
        memory,
        relativeBase
      )
    }
  }
}

def parseInstruction(values: List[BigInt]): Instruction = {
  val instructionCode = values.head
  val opCode = OpCode.withValue(instructionCode.toInt % 100)
  Instruction(
    opCode = opCode,
    paramModes = (0 to 2).map { position =>
      position -> instructionCode / math.pow(10, position + 2).toLong % 10
    }.toMap,
    parameters = values.slice(1, opCode.length)
  )
}

def runInstruction(
    instruction: Instruction,
    memory: Memory,
    inputBuffer: Queue[BigInt],
    outputBuffer: Queue[BigInt],
    relativeBase: ListBuffer[Int]
): Option[Int] = {
  val paramValues = instruction.parameterValues(memory, relativeBase(0))
  instruction.opCode match {
    case Add => {
      memory.write(
        instruction.parameters(2).toInt,
        paramValues(0) + paramValues(1)
      )
      None
    }
    case Mult => {
      memory.write(
        (instruction.parameters(2).toInt),
        paramValues(0) * paramValues(1)
      )
      None
    }
    case Input => {
      memory.write(instruction.parameters(0).toInt, inputBuffer.dequeue())
      None
    }
    case Output => {
      outputBuffer += paramValues(0)
      None
    }
    case Halt => {
      Some(-1) // jumping to -1 will halt program
    }
    case JumpIfTrue => {
      if (paramValues(0) > 0) {
        Some(paramValues(1).toInt)
      } else {
        None
      }
    }
    case JumpIfFalse => {
      if (paramValues(0) == 0) {
        Some(paramValues(1).toInt)
      } else {
        None
      }
    }
    case LessThan => {
      memory.write(
        instruction.parameters(2).toInt,
        if (paramValues(0) < paramValues(1)) {
          1
        } else {
          0
        }
      )
      None
    }
    case Equals => {
      memory.write(
        instruction.parameters(2).toInt,
        if (paramValues(0) == paramValues(1)) {
          1
        } else {
          0
        }
      )
      None
    }
    case SetBase => {
      relativeBase(0) = paramValues(0).toInt
      None
    }
  }
}

def runProgram(
    rawInput: String,
    inputBuffer: Queue[BigInt] = Queue.empty,
    outputBuffer: Queue[BigInt] = Queue.empty
): Memory = {
  // println(s"Starting program ${inputBuffer}")
  val memory = new Memory(parseInput(rawInput))
  val relativeBase = ListBuffer[Int](0)
  var position: Int = 0
  do {
    // parse next instruction
    val instruction =
      parseInstruction(memory.memory.slice(position, position + 4).toList)
    // println(s"Running ${instruction}")
    val maybeJump =
      runInstruction(
        instruction,
        memory,
        inputBuffer,
        outputBuffer,
        relativeBase
      )
    position = maybeJump.getOrElse(position + instruction.opCode.length)
  } while (position >= 0)
  memory
}

// Day 9 stuff

// unit tests ---------------

class Spec extends AnyFlatSpec {
  "test case 1" should "make a copy of itself as output" in {
    val program = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    val output = Queue[BigInt]()
    runProgram(program, outputBuffer = output)
    output.mkString(",") should be(program)
  }

  "test case 2" should "output 16 digit number" in {
    val program = "1102,34915192,34915192,7,4,7,99,0"
    val output = Queue[BigInt]()
    runProgram(program, outputBuffer = output)
    output.dequeue().toString().size should be(16)
  }

  "test case 3" should "output big number in middle" in {
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

(new Spec()).execute()
