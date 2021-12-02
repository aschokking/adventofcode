// Ammonite 2.3.8, Scala 2.13.4
import $file.Util

val rawInput = Util.inputForDay(2)
val instructions = rawInput.map(_.split(" ")).map(parts => (parts(0), parts(1).toInt))

case class Pos(h: Int = 0, depth: Int = 0, aim: Int = 0)

def part1() = {
    val finalPos = instructions.foldLeft(Pos()){(pos, (current)) =>
        current match {
            case ("forward", amount) => pos.copy(h=pos.h + amount)
            case ("up", amount) => pos.copy(depth=pos.depth - amount)
            case ("down", amount) => pos.copy(depth=pos.depth + amount)
        }
    }
    finalPos.h * finalPos.depth
}

println(part1())

def part2() = {
    val finalPos = instructions.foldLeft(Pos()){(pos, (current)) =>
        current match {
            case ("forward", amount) => pos.copy(h=pos.h + amount, depth=pos.depth + pos.aim * amount)
            case ("up", amount) => pos.copy(aim=pos.aim - amount)
            case ("down", amount) => pos.copy(aim=pos.aim + amount)
        }
    }
    finalPos.h * finalPos.depth
}

println(part2())