import $ivy.`com.beachape::enumeratum:1.6.1`
import $file.Util
import $file.IntCode

import scala.collection.mutable.{ListBuffer, Queue, Map => MMap}

import enumeratum.values._
import enumeratum._

import IntCode._

sealed trait Direction extends EnumEntry
object Direction extends Enum[Direction] {
  val values = findValues

  case object Up extends Direction
  case object Left extends Direction
  case object Down extends Direction
  case object Right extends Direction
}
val directions: List[Direction] =
  List(Direction.Up, Direction.Left, Direction.Down, Direction.Right)

val rawInput = Util.inputForDay(11).head

type Coord = (Int, Int)

def runProgram(
    rawInput: String
) = {
  // println(s"Starting program ${inputBuffer}")
  val memory = new Memory(parseInput(rawInput))
  val relativeBase = ListBuffer[Int](0)
  val inputBuffer: Queue[BigInt] = Queue.empty
  val outputBuffer: Queue[BigInt] = Queue.empty
  //var whitePixels: Set[Coord] = Set.empty
  // part 2, start on white
  var whitePixels: Set[Coord] = Set((0, 0))
  var everPixels: Set[Coord] = Set.empty

  var position: Coord = (0, 0)
  var direction: Direction = Direction.Up

  var instructionPointer: Int = 0
  do {
    // if there are 2 output values, apply paint operation
    if (outputBuffer.size == 2) {
      everPixels = everPixels + position
      val color = outputBuffer.dequeue()
      color.intValue match {
        case 1 => {
          whitePixels = whitePixels + position
        }
        case 0 => {
          whitePixels = whitePixels - position
        }
      }
      val turn = outputBuffer.dequeue()
      val directionChange: Int = turn.intValue match {
        case 1 => -1 // right
        case 0 => 1 // left
      }
      direction = directions(
        (directions.size + directions.indexOf(
          direction
        ) + directionChange) % directions.size
      )
      // now change position by direction
      position = direction match {
        case Direction.Up    => (position._1, position._2 + 1)
        case Direction.Down  => (position._1, position._2 - 1)
        case Direction.Left  => (position._1 - 1, position._2)
        case Direction.Right => (position._1 + 1, position._2)

      }
    }

    // Set input to current pixel value
    inputBuffer.clear()
    if (whitePixels.contains(position)) {
      inputBuffer.enqueue(1)
    } else {
      inputBuffer.enqueue(0)
    }

    // parse next instruction
    val instruction =
      parseInstruction(instructionPointer, memory)
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
    instructionPointer =
      maybeJump.getOrElse(instructionPointer + instruction.opCode.length)
  } while (instructionPointer >= 0)

  println(everPixels.size)
  whitePixels
}

val whitePixels = runProgram(rawInput)

// draw pixels
val minY = whitePixels.map(_._2).min - 1
val maxY = whitePixels.map(_._2).max + 1
val minX = whitePixels.map(_._1).min - 1
val maxX = whitePixels.map(_._1).max + 1

(minY to maxY).reverse.map { y => // seems to be mirrored so reverse the order
  {
    println((minX to maxX).map { x =>
      {
        if (whitePixels.contains((x, y))) {
          "#"
        } else {
          " "
        }
      }
    }.mkString)
  }
}
