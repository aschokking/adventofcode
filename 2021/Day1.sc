// Ammonite 2.3.8, Scala 2.13.4
import $file.Util

val rawInput = Util.inputForDay(1)
val depths = rawInput.map(_.toInt)

def part1() = {
    depths.sliding{2}.filter(entry => entry(1) > entry(0)).size
}

println(part1())

def part2() = {
    depths.sliding(3).map(_.sum).sliding(2).filter(entry => entry(1) > entry(0)).size
}

println(part2())