import $file.Util

import scala.collection.mutable.{ListBuffer, Queue}

val rawInput = Util.inputForDay(10)

type Coord = (Int, Int)
type Polar = (Double, Double)
type Grid = List[Coord]

def parseInput(rows: List[String]): Grid = {
  rows.zipWithIndex.flatMap {
    case (row, y) => {
      row.zipWithIndex.flatMap {
        case (char, x) => {
          char match {
            case '#' => Some(x, y)
            case _   => None
          }
        }
      }
    }
  }
}

def angle(c1: Coord, c2: Coord): Double = {
  Math.atan2(c1._1 - c2._1, c1._2 - c2._2) * -1
}

def distance(c1: Coord, c2: Coord): Double = {
  val delta = (c1._1 - c2._1, c1._2 - c2._2)
  Math.sqrt(Math.pow(delta._1, 2) + Math.pow(delta._2, 2))
}

def uniqueAngles(grid: Grid, origin: Coord): Int = {
  grid
    .filterNot(_ == origin)
    .map(candidate => angle(origin, candidate))
    .distinct
    .size
}

def part1(rows: List[String]) = {
  val grid = parseInput(rows)
  grid.map(origin => origin -> uniqueAngles(grid, origin)).maxBy(_._2)
}

println(part1(rawInput))

// part 2

def getPolarCoordinates(grid: Grid, origin: Coord): List[(Polar, Coord)] = {
  grid
    .filterNot(_ == origin)
    .map(candidate => {
      ((angle(origin, candidate), distance(origin, candidate)), candidate)
    })
}

def getCartesian(origin: Coord, polar: Polar): Coord = {
  val delta = (
    Math.cos(polar._1) * polar._2,
    Math.sin(polar._1) * polar._2
  )

  (
    origin._1 + Math.round(delta._1).toInt,
    origin._2 + Math.round(delta._2).toInt
  )
}

def part2(rows: List[String]) = {
  val grid = parseInput(rows)
  val origin = part1(rows)._1

  // collect distances starting with straight up and sorted by distance
  var angleGroups =
    getPolarCoordinates(grid, origin)
      .groupBy(_._1._1)
      .toList
      .sortBy(entry =>
        (entry._1 + (Math.PI * 2)) % (Math.PI * 2)
      ) // sort by smallest angles first
      .map(_._2) // drop groups, just need the lists now
      .map(_.sortBy(_._1._2)) // sort by distance within group
      .map(list =>
        Queue(list: _*)
      ) // use a Mutable Queue so easy to drop things in order later

  // build out a list of items by going in circles

  var angleIndex = 0
  (1 to 200)
    .map { idx =>
      {
        val (current, coord) = angleGroups(angleIndex).dequeue()
        val originalSize = angleGroups.size
        angleGroups = angleGroups.filter(_.nonEmpty)
        if (angleGroups.size == originalSize) {
          // if we didn't empty a group, advance to next one (otherwise we auto advance by virtue of removing our current group)
          angleIndex = (angleIndex + 1) % angleGroups.size
        }

        (current, coord)
      }
    }
}

val vaporizedPoints = part2(rawInput)
val finalCoord = vaporizedPoints.last._2

val result = finalCoord._2 + 100 * finalCoord._1
println(result)

/// unit tests

import $ivy.`org.scalatest::scalatest:3.2.2`

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Spec extends AnyFlatSpec {
  val testCases = Seq(
    (
      """......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####""",
      33
    ),
    (
      """#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.""",
      35
    ),
    (
      """.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..""",
      41
    ),
    (
      """.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##""",
      210
    )
  )

  "part1" should "pass test cases" in {
    testCases.foreach {
      case (input, expected) => {
        part1(input.split("\n").toList)._2 should be(expected)
      }
    }
  }

  "part2" should "pass test case" in {
    val input = testCases.last._1
    val vaporizedPoints = part2(input.split("\n").toList)

    """The 2nd asteroid to be vaporized is at 12,1.
The 3rd asteroid to be vaporized is at 12,2.
The 10th asteroid to be vaporized is at 12,8.
The 20th asteroid to be vaporized is at 16,0.
The 50th asteroid to be vaporized is at 16,9.
The 100th asteroid to be vaporized is at 10,16.
The 199th asteroid to be vaporized is at 9,6.
The 200th asteroid to be vaporized is at 8,2.
The 201st asteroid to be vaporized is at 10,9."""

    val expectedPoints = List(
      (1, (11, 12)),
      (2, (12, 1)),
      (3, (12, 2)),
      (10, (12, 8)),
      (20, (16, 0)),
      (50, (16, 9)),
      (100, (10, 16)),
      (199, (9, 6)),
      (200, (8, 2))
    )

    expectedPoints.foreach {
      case (pos, coord) => {
        vaporizedPoints(pos - 1)._2 should be(coord)
      }
    }

    val finalCoord = vaporizedPoints.last._2
    val result = finalCoord._2 + 100 * finalCoord._1
    result should be(802)
  }
}

(new Spec()).execute()
