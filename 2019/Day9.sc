// Day 9 stuff

def part1() = {
  val input = Queue[BigInt](1)
  val output = Queue[BigInt]()
  runProgram(rawInput, inputBuffer = input, outputBuffer = output)
  println(output.mkString(","))
}

part1()

// unit tests ---------------
