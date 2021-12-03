// Ammonite 2.3.8, Scala 2.13.4
import $file.Util

val rawInput = Util.inputForDay(3)

val testInput = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010""".split("\n").toList

def part1(input: List[String]) = {
    val len = input.size
    val gammaBits = input
        .map(_.map(_.toInt - 48))
        .transpose
        .map(_.sum)
        .map { _ >= len / 2 match {
            case true => "1"
            case false => "0"
        } }.mkString
    println(f"Gamma: $gammaBits%s")
    val epBits = gammaBits.map(_ match {
        case '1' => "0"
        case '0' => "1"
    }).mkString
    println(f"Gamma: $epBits%s")
    Integer.parseInt(gammaBits, 2) * Integer.parseInt(epBits, 2)
}

assert (part1(testInput) == 198)

println(part1(rawInput))

def part2Recursive(input: List[List[Int]], position: Int, common: Boolean): Int = {
    println(f"Starting position ${position} with a list of size ${input.size}")
    if(input.size == 1) {
        Integer.parseInt(input(0).mkString, 2)
    } else {
        val bitCountsByPosition = input.transpose.map(_.sum)
        val positionCount = bitCountsByPosition(position)
        val isOneMostCommon = positionCount >= input.size / 2.0
        val include = if(common) {
            isOneMostCommon
        } else {
            !isOneMostCommon
        }
        include match {
            case true => part2Recursive(input.filter(_(position) == 1), position + 1, common)
            case _ => part2Recursive(input.filter(_(position) == 0), position + 1, common)
        }
    }
}

def part2(input: List[String]) = {
    val inputAsNumbers = input.map(_.map(_.toInt - 48).toList)
    val common = part2Recursive(inputAsNumbers, 0, true)
    val leastCommon = part2Recursive(inputAsNumbers, 0, false)
    println(f"Got parts: ${common} ${leastCommon}")
    common * leastCommon
}

assert (part2(testInput) == 230)

println(part2(rawInput))