import scala.io.Source

val filename = "day1.txt"

val modules = Source.fromFile(filename).getLines.map(_.toInt)

def day1() = {
    def massForModule(module: Int): Int = {
        module / 3 - 2
    }

    val result = modules.map(massForModule).sum
    println(result)
}

day1()


