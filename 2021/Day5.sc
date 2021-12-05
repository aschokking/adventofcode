// Ammonite 2.3.8, Scala 2.13.4
import $file.Util

val testInput = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2""".split("\n").toList

val rawInput = Util.inputForDay(5)

case class Point(x: Int, y: Int)
case class Line(p1: Point, p2: Point)

def parseInput(lines: List[String]):List[Line] = {
    lines.map { line => {
        val points = line.split(" -> ").map(pointStr => {
            val numbers = pointStr.split(",").map(_.toInt)
            Point(numbers(0), numbers(1))
        }).sortBy(p => (p.x, p.y))
        Line(points(0), points(1))
    }}
}

def part1(input: List[String]) = {
    val lines = parseInput(input)
        // filter to straight lines
    val hLines = lines.filter(line => line.p1.y == line.p2.y)
    val vLines = lines.filter(line => line.p1.x == line.p2.x)
    // count spots covered by lines
    val points = hLines.flatMap(line => {
        assert(line.p1.x <= line.p2.x)
        (line.p1.x to line.p2.x).map(x => Point(x, line.p1.y))
    }) ++ vLines.flatMap(line => {
        assert(line.p1.y <= line.p2.y)
        (line.p1.y to line.p2.y).map(y => Point(line.p1.x, y))
    }) 
    val pointCounts = points.groupBy(identity).mapValues(_.size)
    val overlapPoints = pointCounts.filter{ case(k, count) => count > 1 }
    // println(overlapPoints.toList)
    overlapPoints.size
}

assert(part1(testInput) == 5)
println(part1(rawInput))


def part2(input: List[String]) = {
    val lines = parseInput(input)

    // count spots covered by lines
    val points = lines.flatMap(line => {
        val xs = (line.p1.x to line.p2.x)
        val ys = if (line.p1.y <= line.p2.y) {
            (line.p1.y to line.p2.y)
        } else {
            (line.p1.y to line.p2.y by -1)
        }
        val zipped = ((Stream continually xs).flatten zip (Stream continually ys).flatten take (xs.size max ys.size)).toList
        zipped.map{ case(x,y) => Point(x,y)}
    }) 
    val pointCounts = points.groupBy(identity).mapValues(_.size)
    val overlapPoints = pointCounts.filter{ case(k, count) => count > 1 }
    // println(overlapPoints.toList)
    overlapPoints.size
}

assert(part2(testInput) == 12)
println(part2(rawInput))
