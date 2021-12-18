// Ammonite 2.3.8, Scala 2.13.4

import $ivy.`com.beachape::enumeratum:1.6.1`
import $ivy.`org.scalatest::scalatest:3.2.2`
import $file.Util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable.{ListBuffer, Queue}
import scala.io.StdIn.readLine
import enumeratum.values._
import scala.annotation.meta.param

val rawInput = Util.inputForDay(7).head

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
  op
  Instruction(
    opCode = opCode,
    paramModes = (0 to 2).map { position =>
      position -> instructionCode / math.pow(10, position + 2).toInt % 10
    }.toMap,
    parameters = values.slice(1, opCode.length)
  )
}

def runInstruction(
    instruction: Instruction,
    memory: ListBuffer[Int],
    inputBuffer: Queue[Int],
    outputBuffer: Queue[Int]
): Option[Int] = {
  val paramValues = instruction.parameterValues(memory)
  instruction.opCode match {
    case Add => {
      memory(instruction.parameters(2)) = paramValues(0) + paramValues(1)
      None
    }
    case Mult => {
      memory(instruction.parameters(2)) = paramValues(0) * paramValues(1)
      None
    }
    case Input => {
      memory(instruction.parameters(0)) = inputBuffer.dequeue()
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
        Some(paramValues(1))
      } else {
        None
      }
    }
    case JumpIfFalse => {
      if (paramValues(0) == 0) {
        Some(paramValues(1))
      } else {
        None
      }
    }
    case LessThan => {
      memory(instruction.parameters(2)) = if (paramValues(0) < paramValues(1)) {
        1
      } else {
        0
      }
      None
    }
    case Equals => {
      memory(instruction.parameters(2)) =
        if (paramValues(0) == paramValues(1)) {
          1
        } else {
          0
        }
      None
    }
  }
}

def runProgram(
    rawInput: String,
    inputBuffer: Queue[Int] = Queue.empty,
    outputBuffer: Queue[Int] = Queue.empty
): ListBuffer[Int] = {
  // println(s"Starting program ${inputBuffer}")
  val memory = parseInput(rawInput)
  var position = 0
  do {
    // parse next instruction
    val instruction =
      parseInstruction(memory.slice(position, position + 4).toList)
    // println(s"Running ${instruction}")
    val maybeJump =
      runInstruction(instruction, memory, inputBuffer, outputBuffer)
    position = maybeJump.getOrElse(position + instruction.opCode.length)
  } while (position >= 0 && position < memory.size)
  memory
}

// day 7 part 1 specific stuff

def getOutputThrust(program: String, phaseSeq: Seq[Int]): Int = {
  //println(s"getOutputThrust ${phaseSeq}")
  phaseSeq.foldLeft(0)((previousOutput, currentPhase) => {
    val outputQueue = Queue[Int]()
    runProgram(
      program,
      inputBuffer = Queue(currentPhase, previousOutput),
      outputBuffer = outputQueue
    )
    outputQueue.dequeue()
  })
}

def part1(): Int = {
  (0 to 4).permutations.map { phases =>
    {
      getOutputThrust(rawInput, phases)
    }
  }.max
}

// day 7 part 2

class Program(
    val instructions: String,
    var inputBuffer: Queue[Int] = Queue.empty,
    var outputBuffer: Queue[Int] = Queue.empty
) {
  val memory = parseInput(instructions)
  var position = 0

  def isHalted(): Boolean = {
    !(position >= 0 && position < memory.size)
  }

  def step(): Boolean = {
    if (position >= 0 && position < memory.size) {
      val instruction =
        parseInstruction(memory.slice(position, position + 4).toList)
      // println(s"Running ${instruction}")
      try {
        val maybeJump =
          runInstruction(instruction, memory, inputBuffer, outputBuffer)
        position = maybeJump.getOrElse(position + instruction.opCode.length)
      } catch {
        case e: NoSuchElementException => true
      }
      true
    } else {
      false
    }
  }
}

def getOutputThrust2(program: String, phaseSeq: Seq[Int]): Int = {
  //println(s"getOutputThrust ${phaseSeq}")
  val programs = phaseSeq.map(phase => {
    new Program(program, inputBuffer = Queue(phase))
  })
  programs(0).inputBuffer += 0 // initial input to start
  while (!programs.forall(_.isHalted)) {
    programs.zipWithIndex.map {
      case (program, idx) => {
        program.step()
        // if there's anything in the output queue, send it to the next programs input
        if (program.outputBuffer.nonEmpty) {
          val nextProgram = programs((idx + 1) % programs.size)
          nextProgram.inputBuffer += program.outputBuffer.dequeue()
        }
      }
    }
  }
  programs(0).inputBuffer.dequeue()
}

def part2(): Int = {
  (5 to 9).permutations.map { phases =>
    {
      getOutputThrust2(rawInput, phases)
    }
  }.max
}

// unit tests ---------------

class Spec extends AnyFlatSpec {
  val testCases = Seq(
    (
      "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0",
      Seq(4, 3, 2, 1, 0),
      43210
    ),
    (
      "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0",
      Seq(0, 1, 2, 3, 4),
      54321
    ),
    (
      "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0",
      Seq(1, 0, 4, 3, 2),
      65210
    )
  )

  val testCases2 = Seq(
    (
      "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5",
      Seq(9, 8, 7, 6, 5),
      139629729
    ),
    (
      "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10",
      Seq(9, 7, 8, 5, 6),
      18216
    )
  )

  "getOutputThrust" should "pass test cases" in {
    testCases.foreach {
      case (program, phases, expected) => {
        getOutputThrust(program, phases) should be(expected)
      }
    }
  }

  "getOutputThrust2" should "pass test cases" in {
    testCases2.foreach {
      case (program, phases, expected) => {
        getOutputThrust2(program, phases) should be(expected)
      }
    }
  }
}

(new Spec()).execute()

//// run actual program

println(part1())
println(part2())
