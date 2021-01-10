import $file.Util

val testInput = """COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L""".split("\n").toList

val inputLines = Util.inputForDay(6)

// convience type
type ChildMap = Map[String, List[String]]
type ParentMap = Map[String, String]

def parseInput(lines: List[String]) = {
  lines.map(lines => {
    val split = lines.split("\\)")
    (split(0), split(1))
  })
}

def buildChildMap(lines: List[String]) = {
  parseInput(lines)
    .groupBy(items => items._1) // group by the parent
    .mapValues(_.map(_._2))
    .toMap // have child just be the value not the tuples
}

def part1(inputLines: List[String]) = {
  val orbitMap = buildChildMap(inputLines)
  def countPaths(node: String, depth: Int = 0): Int = {
    val children = orbitMap.get(node).getOrElse(List.empty)
    depth +
      children
        .map(childNode => {
          countPaths(childNode, depth + 1)
        })
        .sum
  }
  countPaths("COM")
}

assert(part1(testInput) == 42)
println(part1(inputLines))

// part 2

val testInput2 = """COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN""".split("\n").toList

def buildParentMap(lines: List[String]) = {
  parseInput(lines)
    .groupBy(items => items._2)
    .mapValues(_.map(_._1).head)
    .toMap
}

def ancestors(node: String, parentMap: ParentMap): List[String] = {
  parentMap.get(node) match {
    case None         => List.empty
    case Some(parent) => List(parent) ++ ancestors(parent, parentMap)
  }
}

def part2(inputLines: List[String]): Int = {
  val parentMap = buildParentMap(inputLines)
  val ancestorLists =
    List("SAN", "YOU").map(node => ancestors(node, parentMap))
  val commonAncestors = ancestorLists.map(_.toSet).reduce(_.intersect(_)).toList
  val lowestCommon = commonAncestors
    .sortBy(candidate => ancestorLists(0).indexOf(candidate))
    .head
  ancestorLists.map(_.indexOf(lowestCommon)).sum
}

assert(part2(testInput2) == 4)
println(part2(inputLines))
