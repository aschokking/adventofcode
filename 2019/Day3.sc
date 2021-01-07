import $ivy.`org.scalatest::scalatest:3.2.2` 
import $file.Util

import scala.collection.mutable.ListBuffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

val rawInput = Util.inputForDay(3)

case class Move(direction: Char, amount: Int) {
    def getDirectionVector() = {
        direction match {
            case 'U' =>  Pos(0, 1)
            case 'D' => Pos(0, -1)
            case 'L' => Pos(-1, 0)
            case 'R' => Pos(1, 0)
        }
    }
}

case class Pos(x: Int, y: Int) {
    def +(that: Pos): Pos = {
        Pos(x + that.x, y + that.y)
    }
}

def parseInput(rawInput: List[String]): List[List[Move]] = {
    rawInput.map { line => {
        line.split(',').map { entry => {
            Move(entry.charAt(0), entry.substring(1).toInt)
        }}.toList
    }}
}

def getPositions(moves: List[Move]): List[Pos] = {
    moves.foldLeft(ListBuffer(Pos(0, 0)))( (positions, current) => {
        // from last position, add positions according to current move
        Seq.fill(current.amount)(current.getDirectionVector)
            .foldLeft(positions)( (positions, delta) => {
                positions += (positions.last + delta)
                positions
            })
    }).toList
}

def part1(rawInput: List[String]): Int = {
    val moveLists = parseInput(rawInput)
    val posLists = moveLists.map(getPositions)
        .map(_.drop(1)) // remove the 0,0 position from each list
    val posSets=posLists.map(_.toSet)
    posSets
        .reduce(_.intersect(_))
        .map(pos => pos.x.abs + pos.y.abs)
        .toList
        .sorted
        .head
}

def part2(rawInput: List[String]): Int = {
    val moveLists = parseInput(rawInput)
    val posLists = moveLists.map(getPositions)
        .map(_.drop(1)) // remove the 0,0 position from each list
    val overlapPoints=posLists.map(_.toSet).reduce(_.intersect(_))
    val distances=overlapPoints.toList.map(point => {
        posLists.map(_.indexOf(point)).sum
    })
    distances.sorted.head + 2 // add the starting moves back in
}

println(s"Part1: ${part1(rawInput)}")

println(s"Part2: ${part2(rawInput)}")

class Spec extends AnyFlatSpec {
    val testCases1 = Seq(
        ("""R75,D30,R83,U83,L12,D49,R71,U7,L72
    U62,R66,U55,R34,D71,R55,D58,R83""", 159),
        ("""R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
    U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""", 135),
    )

    val testCases2 = Seq(
        ("""R75,D30,R83,U83,L12,D49,R71,U7,L72
    U62,R66,U55,R34,D71,R55,D58,R83""", 610),
        ("""R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
    U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""", 410),
    )

    "getPositions()" should "do the thing" in {
        val result = getPositions(List(Move('U', 2), Move('R', 1)))
        result should be(List(
            Pos(0, 0), Pos(0, 1), Pos(0,2), Pos(1, 2)
        ))
    } 

    "part1" should "pass" in {
        testCases1.map { case (rawInput, expected) => {
            val result = part1(rawInput.split("\n").toList)
            result should be (expected)
        }}
    }  
    
    "part2" should "pass" in {
        testCases2.map { case (rawInput, expected) => {
            val result = part2(rawInput.split("\n").toList)
            result should be (expected)
        }}
    }  
}

(new Spec).execute() 