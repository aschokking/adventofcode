import $ivy.`com.beachape::enumeratum:1.6.1`

import scala.collection.mutable.ListBuffer
import enumeratum.values._

sealed abstract class OpCode(val value: Int, val length: Int) extends IntEnumEntry
object OpCode extends IntEnum[OpCode] {
  case object Halt extends OpCode(99, 1)
  case object Add extends OpCode(1, 4)
  case object Mult extends OpCode(2, 4)
  case object Input extends OpCode(3, 2)
  case object Output extends OpCode(4, 2)

  val values = findValues

}

case class Instruction(
    opCode: OpCode,
    paramModes: Map[Int, Int] = Map.empty,
    parameters: List[Int] = List.empty
)

def parseInstruction(instructionCode: Int): Instruction = {
  Instruction(
    opCode = OpCode.withValue(instructionCode % 100),
    paramModes = (0 to 3).map { position =>
      position -> instructionCode / math.pow(10, position + 2).toInt % 10
    }.toMap
  )
}

def groupOps(input: ListBuffer[Int]): Iterator[Instruction] = {
  var position = 0
  Iterator
    .continually {
      val instruction = parseInstruction(input(position))
      position = position + instruction.opCode.length
      instruction

    }
    .takeWhile(_.opCode != OpCode.Halt && position <= input.size)
}

val l = ListBuffer(1, 2, 3, 4, 99)

println(groupOps(l).toSeq)
