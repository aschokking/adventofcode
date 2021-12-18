// Ammonite 2.3.8, Scala 2.13.4
import $file.Util
import scala.collection.mutable.{Set => MutableSet}

val rawInput = Util.inputForDay(9)

val testInput = Util.testInputForDay(9)


def parseInput(input: List[String]) = {
    input.map(_.map(_.toString).map(_.toInt).toList)
}

def findLowPoints(rows: List[List[Int]]) = {
    rows.zipWithIndex.flatMap { case(row, y) => {
        row.zipWithIndex.filter { case(number, x) => {
            List((-1, 0), (1, 0), (0, -1), (0, 1)).forall { case (dx, dy) => {
                rows.lift(y + dy).flatMap(_.lift(x + dx))
                    .map(_ > number).getOrElse(true)
            }}
        }}.map{ case(number, x) => (number, x, y)}
    }}
}

def part1(input: List[String]) = {
    val rows = parseInput(input)

    val lowPoints = findLowPoints(rows).map(_._1)
    lowPoints.map(_ + 1).sum

}

assert(part1(testInput) == 15)
println(part1(rawInput))

def basinSize(rows: List[List[Int]], x: Int, y: Int, visited: MutableSet[String] = MutableSet()): Int = {
    
    //println(s"Checking ${x}, ${y} recursive, visited is ${visited}")
    if (visited.contains(s"${x}-${y}")) {
        0
    } else if(x < 0 || x >= rows.head.length || y < 0 || y >= rows.length) {
        0
    } else if(rows(y)(x) == 9) {
        0
    } else {
        val currentValue = rows(y)(x)
        //println(s"CurrentValues: ${currentValue}")
        // recurse
        visited.add(s"${x}-${y}")
        1 + List((x -1, y), (x+ 1, y), (x, y -1), (x, y+1)).map { case (newX, newY) => {
            basinSize(rows, newX, newY, visited)
        }}.sum
    }
}

def part2(input: List[String]) = {
    val rows = parseInput(input)

    val lowPoints = findLowPoints(rows)

    println(s"Found low points: ${lowPoints}")

    val basinSizes = lowPoints.map { case(_, x, y) => {
        //println(s"Testing ${x}, ${y}")
        // recurse from each lowPoint to find all the adjacent non-9 squares
        basinSize(rows, x, y)
      }}

    println(s"Basin sizes: ${basinSizes}")
    
    basinSizes.sorted.reverse.take(3).product
}

assert(part2(testInput) == 1134)
println(part2(rawInput))