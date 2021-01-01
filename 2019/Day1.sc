import scala.io.Source

val filename = "day1.txt"

val modules = Source.fromFile(filename).getLines.toList.map(_.toInt)

def part1() = {
    def massForModule(module: Int): Int = {
        module / 3 - 2
    }

    val result = modules.map(massForModule).sum
    println(result)
}

def part2() = {
    def massForModule(module: Int): Int = {
        module / 3 - 2 match {
            case value if value <= 0 => 0
            case value => value + massForModule(value)
        }
    }
    val result = modules.map(massForModule).sum
    println(result)
}

part1()
part2()
