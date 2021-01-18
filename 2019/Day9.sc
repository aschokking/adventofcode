import $file.IntCode
import $file.Util

import scala.collection.mutable.{ListBuffer, Queue}

import IntCode._

val rawInput = Util.inputForDay(9).head

def part1() = {
  val input = Queue[BigInt](1)
  val output = Queue[BigInt]()
  runProgram(rawInput, inputBuffer = input, outputBuffer = output)
  println(output.mkString(","))
}

part1()

def part2() = {
  val input = Queue[BigInt](2)
  val output = Queue[BigInt]()
  runProgram(rawInput, inputBuffer = input, outputBuffer = output)
  println(output.mkString(","))
}

part2()
