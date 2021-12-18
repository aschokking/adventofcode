// Ammonite 2.3.8, Scala 2.13.4
import $file.Util
import scala.collection.mutable.ListBuffer

val rawInput = Util.inputForDay(12)

val testInput = Util.testInputForDay(12)

val START = "start"
val END = "end"

type RouteMap = Map[String, Set[String]]

def parseInput(input: List[String]):RouteMap = {
    val ltr = input.map(_.split("-").toList)
    val rtl = ltr.map(_.reverse)
    (ltr ++ rtl).groupBy(_.head).mapValues(_.map(_.last).toSet).toMap
}

def findRoutes(map: RouteMap, node: String = START, route: String = ""): Set[String] = {
    // base cases
    val newRoute = s"${route}-${node}"

    // if END, add route and return this valid route
    if(node == END) {
        Set(newRoute)
    } else if (node != node.toUpperCase && route.contains(node)) {
        Set()
    } else {
        map.get(node).get.flatMap(dest => findRoutes(map, dest, newRoute))
    }
}

def part1(input: List[String]): Int = {
    val map = parseInput(input)
    val routes = findRoutes(map, START)
    routes.size
}

assert(part1(testInput) == 226)
println(part1(rawInput))

def findRoutes2(map: RouteMap, node: String = START, route: String = ""): Set[String] = {
    // base cases
    val newRoute = s"${route}-${node}"

    val smallReused = route
        .split("-")
        .filter(n => n != n.toUpperCase)
        .groupBy(identity)
        .mapValues(_.size)
        .values
        .reduceOption( _ max _ )
        .map(_ >= 2)
        .getOrElse(false)

    // if END, add route and return this valid route
    if(node == END) {
        Set(newRoute)
    } else if (
        // always keep going if node is upper case
            node == node.toUpperCase 
            || 
            // if we havent visited this small before, we can
            (!route.contains(node)) 
            ||
            // if we have visited this small before, but we haven't visited any small twice
            (!smallReused && node != START)
    ) {
            map.get(node).get.flatMap(dest => findRoutes2(map, dest, newRoute))
    } else {
        Set()
    }
}

def part2(input: List[String]): Int = {
    val map = parseInput(input)
    val routes = findRoutes2(map, START)
    routes.size
}

println(part2(rawInput))
assert(part2(testInput) == 3509)
