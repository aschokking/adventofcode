import $ivy.`com.beachape::enumeratum:1.6.1`
import $file.Util
import $file.IntCode

import scala.collection.mutable.{ListBuffer, Queue, Map => MMap}

import enumeratum.values._
import enumeratum._

import IntCode._

val rawInput = Util.inputForDay(13).head

case class Coord(x: Int, y: Int)
type BoardState = Map[Coord, Int]

val output: Queue[BigInt] = Queue.empty
runProgram(rawInput, outputBuffer = output)

// process output and update grid with values
val finalBoard = output.toList
  .grouped(3)
  .foldLeft[BoardState](Map.empty)((prevMap, current) => {
    val coord = Coord(current(0).toInt, current(1).toInt)
    prevMap.updated(coord, current(2).toInt)
  })

// part 1, count block tiles
val result1 = finalBoard.values.filter(_ == 2).size
println(result1)

//drawBoard(finalBoard)

def drawBoard(board: BoardState) = {
  // draw pixels
  val minY = board.keys.map(_.y).min - 1
  val maxY = board.keys.map(_.y).max + 1
  val minX = board.keys.map(_.x).min - 1
  val maxX = board.keys.map(_.x).max + 1

  (minY to maxY).reverse.map { y => // seems to be mirrored so reverse the order
    {
      println((minX to maxX).map { x =>
        {
          board.get(Coord(x, y)) match {
            case Some(0) => " "
            case Some(1) => "W"
            case Some(2) => "b"
            case Some(3) => "="
            case Some(4) => "0"
            case None    => " "
          }
        }
      }.mkString)
    }
  }

}

// part 2

val output2: Queue[BigInt] = Queue.empty
val updatedProgram = "2" + rawInput.drop(1)

def part2(
    rawInput: String,
    inputBuffer: Queue[BigInt] = Queue.empty,
    outputBuffer: Queue[BigInt] = Queue.empty
): Int = {
  // println(s"Starting program ${inputBuffer}")
  var boardState: BoardState = Map.empty
  var score = -1
  val memory = new Memory(parseInput(rawInput))
  val relativeBase = ListBuffer[Int](0)
  var position: Int = 0
  var prevBallPos: Option[Coord] = None
  var prevPaddlePos: Option[Coord] = None
  // queue up neutral position
  inputBuffer.enqueue(0)
  do {
    if (outputBuffer.size == 3) {
      val coord =
        Coord(outputBuffer.dequeue().toInt, outputBuffer.dequeue().toInt)
      val value = outputBuffer.dequeue().toInt
      if (coord == (Coord(-1, 0))) {
        score = value
      } else {
        boardState = boardState.updated(coord, value)
      }

    }

    // find paddle and ball, adjust joystick to line up paddle and ball
    val currentBallPos =
      boardState.filter { case (_, kind) => kind == 4 }.headOption.map(_._1)
    val currentPaddlePos =
      boardState.filter { case (_, kind) => kind == 3 }.headOption.map(_._1)
    if (currentBallPos != prevBallPos) {
      // move paddle towards ball
      inputBuffer.clear()
      val joystickInput = (currentPaddlePos, currentBallPos) match {
        case (Some(paddlePos), Some(ballPos)) => {
          if (paddlePos.x == ballPos.x) { 0 }
          else if (paddlePos.x > ballPos.x) { -1 }
          else { 1 }
        }
        case _ => 0
      }
      inputBuffer.enqueue(joystickInput)

      // draw board state and sleep
      println(s"s: ${score}")
      drawBoard(boardState)
    }

    prevPaddlePos = currentPaddlePos
    prevBallPos = currentBallPos
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

  drawBoard(boardState)
  score
}

println(part2(updatedProgram))
