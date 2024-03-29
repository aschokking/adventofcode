// Ammonite 2.3.8, Scala 2.13.4
import $file.Util

val rawInput = Util.inputForDay(6)

val testInput = """3,4,3,1,2""".split("\n").toList

def parseInput(input: List[String]) = {
    input.head.split(",").map(_.toInt).toList
}

def part1(input: List[String], days: Int) = {
    val startTimings = parseInput(input)
    // collect how many of each timing their are
    var timingCounts: Map[Int, BigInt] = startTimings.groupBy(identity).mapValues(_.size).mapValues(int => BigInt(int)).toMap
    for(i <- 0 until days) {
        var newFishes: BigInt = 0
        timingCounts = timingCounts.toList.map { case (key, count) => {
            key match {
                case 0 => {
                    newFishes = count
                    (6 -> count)
                }
                case number => (number -1 -> count)
            }
        }}.groupBy(_._1).mapValues(_.map(_._2).sum).toMap
        if(newFishes > 0) {
            timingCounts = timingCounts + (8 -> newFishes)
        }
    }
    val result = timingCounts.values.sum
    result
}

assert(part1(testInput, 80) == 5934)
println(part1(rawInput, 80))

// part 2 
assert(part1(testInput, 256) == BigInt("26984457539"))
println(part1(rawInput, 256))