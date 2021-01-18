import $ivy.`com.beachape::enumeratum:1.6.1`
import $ivy.`org.scalatest::scalatest:3.2.2`

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable.{ListBuffer, Queue}
import scala.io.StdIn.readLine
import enumeratum.values._
import scala.annotation.meta.param
import os.FileType.Dir

sealed abstract class OpCode(
    val value: Int,
    val length: Int,
    val outputParams: Int = 0
) extends IntEnumEntry
object OpCode extends IntEnum[OpCode] {
  case object Halt extends OpCode(99, 1)
  case object Add extends OpCode(1, 4, 1)
  case object Mult extends OpCode(2, 4, 1)
  case object Input extends OpCode(3, 2, 1)
  case object Output extends OpCode(4, 2)
  case object JumpIfTrue extends OpCode(5, 3)
  case object JumpIfFalse extends OpCode(6, 3)
  case object LessThan extends OpCode(7, 4, 1)
  case object Equals extends OpCode(8, 4, 1)
  case object SetBase extends OpCode(9, 2)

  val values = findValues // required for enums
}
import OpCode._

sealed abstract class ParamMode(val value: Int) extends IntEnumEntry
object ParamMode extends IntEnum[ParamMode] {
  case object MemAddr extends ParamMode(0)
  case object Direct extends ParamMode(1)
  case object RelMemAddr extends ParamMode(2)

  val values = findValues // required for enums
}
import ParamMode._

class Memory(val memory: ListBuffer[BigInt] = ListBuffer.empty) {
  def read(address: Int): BigInt = {
    validateAddress(address)
    memory(address)
  }

  def validateAddress(address: Int): Unit = {
    if (address < 0) {
      throw new Exception(s"Address ${address} below 0")
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
    parameterMode: ParamMode,
    memory: Memory,
    isOutputAddress: Boolean = false,
    relativeBase: Int = 0
): BigInt = {
  if (isOutputAddress) {
    parameterMode match {
      case MemAddr    => parameter.toInt // read address
      case RelMemAddr => (relativeBase + parameter.toInt)
    }
  } else {
    parameterMode match {
      case MemAddr    => memory.read(parameter.toInt) // read address
      case Direct     => parameter // immediate mode, use value directly
      case RelMemAddr => memory.read((relativeBase + parameter.toInt))
    }
  }
}

def parseInput(rawInput: String): ListBuffer[BigInt] =
  rawInput.split(",").map(str => BigInt(str)).to(ListBuffer)

case class Instruction(
    opCode: OpCode,
    parameters: List[BigInt],
    paramModes: Map[Int, ParamMode]
) {
  def parameterValues(
      memory: Memory,
      relativeBase: Int
  ): List[BigInt] = {
    parameters.zipWithIndex.map { case (parameter, i) =>
      parameterValue(
        parameter,
        paramModes.get(i).getOrElse(ParamMode.MemAddr),
        memory,
        i >= parameters.size - opCode.outputParams,
        relativeBase
      )
    }
  }
}

def parseInstruction(instructionAddress: Int, memory: Memory): Instruction = {
  val instructionCode = memory.read(instructionAddress)
  val opCode = OpCode.withValue(instructionCode.toInt % 100)
  val paramInputs = memory.memory.slice(
    instructionAddress + 1,
    instructionAddress + opCode.length
  )
  Instruction(
    opCode = opCode,
    paramModes = (0 to opCode.length - 2).map { position =>
      position -> ParamMode.withValue(
        (instructionCode / math.pow(10, position + 2).toInt % 10).toInt
      )
    }.toMap,
    parameters = paramInputs.toList
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
        paramValues(2).toInt,
        paramValues(0) + paramValues(1)
      )
      None
    }
    case Mult => {
      memory.write(
        paramValues(2).toInt,
        paramValues(0) * paramValues(1)
      )
      None
    }
    case Input => {
      memory.write(paramValues(0).toInt, inputBuffer.dequeue())
      None
    }
    case Output => {
      // println(s"Outputting ${paramValues(0)}")
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
        paramValues(2).toInt,
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
        paramValues(2).toInt,
        if (paramValues(0) == paramValues(1)) {
          1
        } else {
          0
        }
      )
      None
    }
    case SetBase => {
      relativeBase(0) = relativeBase(0) + paramValues(0).toInt
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
      parseInstruction(position, memory)
    //println(s"${memory.memory.take(20)}")
    //println(s"Running ${instruction} || base ${relativeBase.head}")
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
