// Ammonite 2.3.8, Scala 2.13.4
import $file.Util

val rawInput = Util.inputForDay(7)

val testInput = """16,1,2,0,4,2,7,1,2,14""".split("\n").toList

def parseInput(input: List[String]) = {
    input.head.split(",").map(_.toInt)
}

def findMinCost(input: List[String], costFn: (Int) => Int) = {
    val positions = parseInput(input)
    val grouped = positions.groupBy(identity).mapValues(_.size)
    val max = grouped.keys.max
    val min = grouped.keys.min
    val costs = (min to max).map(target => {
        grouped.foldLeft(0) { case (cost, (position, count)) => {
            cost + costFn((position - target).abs) * count
        }}
    })
    costs.min
}

def part1(input: List[String]) = {
    findMinCost(input, (gap) => gap)
}   

assert(part1(testInput) == 37)
println(part1(rawInput))

def part2(input: List[String]) = {
    findMinCost(input, (gap) => {
        // math from wikipedia, triangular number
        gap * (gap + 1) / 2
    })
}   

assert(part2(testInput) == 168)
println(part2(rawInput))