import scala.io.Source

def inputForDay(day: Int): List[String] = {
    Source.fromFile(s"input/${day}.txt").getLines.toList
}