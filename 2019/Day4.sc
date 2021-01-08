val range = (134792, 675810) // inclusive?

val pairRegex = raw"(\d)\1+".r
val decreasingRegex =
  (1 to 9).map(digit => s"${digit}.*[0-${digit - 1}]").mkString("|").r

def isValid1(str: String): Boolean = {
  pairRegex.findFirstIn(str).nonEmpty && !decreasingRegex
    .findFirstIn(str)
    .nonEmpty
}

def isValid2(str: String): Boolean = {
  // see if there's a group of size exactly 2
  pairRegex.findAllIn(str).map(_.size).filter(_ == 2).nonEmpty
}

val valid1 = (range._1 to range._2).map(_.toString).filter(isValid1)
val valid2 = valid1.filter(isValid2)

println(s"Part 1: ${valid1.size}")
println(s"Part 2: ${valid2.size}")
