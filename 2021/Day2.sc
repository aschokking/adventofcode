// Ammonite 2.3.8, Scala 2.13.4
import $file.Util

val rawInput = Util.inputForDay(2)
val instructions = rawInput.map(_.split(" ")).map(parts => (parts(0), parts(1).toInt))


def part1() = {
    val finalPos = instructions.foldLeft((0,0)){(pos, (current)) =>
        current match {
            case ("forward", amount) => (pos._1 + amount, pos._2)
            case ("up", amount) => (pos._1, pos._2 - amount)
            case ("down", amount) => (pos._1, pos._2 + amount)
        }
    }
    finalPos._1 * finalPos._2
}

println(part1())

def part2() = {
    val finalPos = instructions.foldLeft((0,0,0)){(pos, (current)) =>
        current match {
            case ("forward", amount) => (pos._1 + amount, pos._2 + pos._3 * amount, pos._3)
            case ("up", amount) => (pos._1, pos._2, pos._3 - amount)
            case ("down", amount) => (pos._1, pos._2, pos._3 + amount)
        }
    }
    finalPos._1 * finalPos._2
}

println(part2())