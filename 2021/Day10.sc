// Ammonite 2.3.8, Scala 2.13.4
import $file.Util
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.ArrayDeque

val rawInput = Util.inputForDay(10)

val testInput = Util.testInputForDay(10)

/*
): 3 points.
]: 57 points.
}: 1197 points.
>: 25137 points.
*/

val pointsMap = Map(
    (")" -> 3),
    ("]" -> 57),
    ("}" -> 1197),
    (">" -> 25137),
)

val openings = "([{<".map(_.toString).toSet

val closingToOpenings = "()[]{}<>".grouped(2).toList.map(pair => (pair(1).toString -> pair(0).toString)).toMap

def findCorrupt(input: String): Option[String] = {
    val stack = new ArrayDeque[String]
    var farthestIndex = 0
    //println(s"findCorrupt ${input} of len ${input.size}")
    input.map(_.toString).iterator
        .takeWhile(current => {
            //println(s"Takewhile testing ${current}")
            openings.contains(current) || stack.lastOption == closingToOpenings.get(current)
        })
        .foreach { current => {
            //println(s"Testing character ${current} at index ${farthestIndex}, stack=${stack}")
            farthestIndex += 1
            if(openings.contains(current)) {
                stack.append(current)
            } else {
                stack.removeLast()
            }
        }}

    if(stack.nonEmpty && farthestIndex != input.size) {
        val corrupt = input.charAt(farthestIndex).toString
        //println(s"Found corrupt char ${corrupt}")
        Some(corrupt)
    } else {
        //println("Not corrupt")
        None
    }
}

def part1(input: List[String]) = {
    input.flatMap(findCorrupt).flatMap(pointsMap.get).sum
}

assert(part1(testInput) == 26397)
println(part1(rawInput))

val pointsMap2 = Map(
    ("(" -> 1),
    ("[" -> 2),
    ("{" -> 3),
    ("<" -> 4),
)

def findCompletions(input: String): Int = {
    val stack = new ArrayDeque[String]
    var farthestIndex = 0
    //println(s"findCorrupt ${input} of len ${input.size}")
    input.map(_.toString).iterator
        .takeWhile(current => {
            //println(s"Takewhile testing ${current}")
            openings.contains(current) || stack.lastOption == closingToOpenings.get(current)
        })
        .foreach { current => {
            //println(s"Testing character ${current} at index ${farthestIndex}, stack=${stack}")
            farthestIndex += 1
            if(openings.contains(current)) {
                stack.append(current)
            } else {
                stack.removeLast()
            }
        }}

    if(farthestIndex == input.size) {
        // turn stack into score
        stack.toList.reverse.foldLeft(0)((total, current) => {
            total * 5 + pointsMap2.get(current).get
        })
    } else {
        0
    }
}

def part2(input: List[String]) = {
    val sortedScores = input.map(findCompletions).filter(_ > 0).sorted
    println(s"SortedScores: ${sortedScores}")
    sortedScores(sortedScores.size / 2)
}

assert(part2(testInput) == 288957)
println(part2(rawInput))