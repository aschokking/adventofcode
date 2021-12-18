// Ammonite 2.3.8, Scala 2.13.4
import $file.Util
import scala.collection.mutable.ListBuffer

val rawInput = Util.inputForDay(11)

val testInput = Util.testInputForDay(11)

def increaseEnergy(octos: List[ListBuffer[Int]], x: Int, y: Int): Unit = {
    if(x < 0 || y < 0 || x >= octos.head.size || y >= octos.size) {
        // no op
    } else {
        octos(y)(x) = octos(y)(x) + 1
        if(octos(y)(x) == 10) {
            // recurse
            for {
                dx <- List(1, 0, -1)
                dy <- List(1, 0, -1)
            } yield {
                if(dx != 0 || dy != 0)
                    increaseEnergy(octos, x+dx, y+dy)
            }
        }
    }
}

def countFlashes(octos: List[ListBuffer[Int]]): Int = {
    octos.zipWithIndex.foreach { 
        case(row, y) => row.zipWithIndex.foreach { case (_, x) => {
            increaseEnergy(octos, x, y)
        }}
    }
    // count entries 10 and over, they flashed
    val flashCount = octos.map(_.filter(_ >= 10).size).sum

    // set those entries back to 0
    octos.foreach(row => row.zipWithIndex.map { case(entry, i) => {
        if(row(i) >= 10) {
            row(i) = 0
        }
    }})
    flashCount
}

def part1(input: List[String]) = {
    val octos = input.map(line => ListBuffer(line.map(_.toString.toInt): _*))
    var flashes = 0
    for( a <- 0 until 100){
        flashes += countFlashes(octos)
        
    }
    flashes
}

assert(part1(testInput) == 1656)
println(part1(rawInput))

def part2(input: List[String]) = {
    val octos = input.map(line => ListBuffer(line.map(_.toString.toInt): _*))
    var flashes = 0
    Stream.from(1).takeWhile( _ => countFlashes(octos) != 100).last + 1
}
assert(part2(testInput) == 195)
println(part2(rawInput))