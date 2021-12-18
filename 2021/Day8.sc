// Ammonite 2.3.8, Scala 2.13.4
import $file.Util

val rawInput = Util.inputForDay(8)

val testInput = Util.testInputForDay(8)


def parseInput(input: List[String]) = {
    input.map(_.split(" \\| ").toList.map(_.split(" ").toList))
}

def part1(input: List[String]) = {
    val parsed = parseInput(input)
    val uniqueSegmentCounts = Set(2, 3, 4, 7)
    parsed
        .map(_(1)) // just output side
        .map(
            _.map(_.size) // length of tokens
            .filter(len => uniqueSegmentCounts.contains(len))
            .size
        ).sum
}

assert(part1(testInput) == 26)
println(part1(rawInput))

val numberToSegmentsMap = Map(
    0 -> "abcefg",
    1 -> "cf",
    2 -> "acdeg",
    3 -> "acdfg",
    4 -> "bcdf",
    5 -> "abdfg",
    6 -> "abdefg",
    7 -> "acf",
    8 -> "abcdefg",
    9 -> "abcdfg",
).mapValues(_.split("").toSet)

val segmentsToNumbersMap = numberToSegmentsMap.map{ case(key, value) => value -> key }.toMap

val segmentCountsToNumbers = segmentsToNumbersMap
    .toList
    .map{ case(segments, number) => segments.size -> number}
    .groupBy(_._1).mapValues(_.map(_._2).toSet).toMap

def part2(input: List[String]) = {
    val parsed = parseInput(input)
    val mapping = determineMapping(parsed(0)(0))
    0
}

def determineMapping(inputDigits: List[String]): Map[String, String] = {
    // at first every input letter can go to any other letter
    var mapping = "abcdefg".split("").map(letter => letter -> "abcdefg".split("").toSet).toMap
    inputDigits.map(digitStr => {
        val possibleNumbers = segmentCountsToNumbers(digitStr.size)
        val posibleLetters = possibleNumbers.flatMap(number => numberToSegmentsMap(number))
        println(f"${digitStr} could be ${possibleNumbers} which map to letters ${posibleLetters}")
        digitStr.split("").foreach(inputLetter => {
            mapping = mapping.map{ case(letter, possibilities) => {
                letter match {
                    case `inputLetter` => {
                        letter -> possibilities.intersect(posibleLetters)
                    }
                    case _ => letter -> possibilities
                }
            }}
        })
    })
    println(mapping)
    assert(mapping.mapValues(_.size).max == 1)
    mapping.mapValues(_.toList.head).toMap
}

assert(part2(testInput) == 61229)
println(part2(rawInput))