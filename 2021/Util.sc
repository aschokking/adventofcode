// Ammonite 2.3.8, Scala 2.13.4

import scala.io.Source

def inputForDay(day: Int): List[String] = {
    Source.fromFile(s"input/${day}.txt").getLines.toList
}