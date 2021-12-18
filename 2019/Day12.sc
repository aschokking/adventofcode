import $file.Util

val rawInput = Util.inputForDay(12)

type Vector = List[Int]
case class State(val position: Vector, val velocity: Vector = List(0, 0, 0)) {
  def energy(): Int = {
    position.map(_.abs).sum * velocity.map(_.abs).sum
  }
}

def parseInput(input: List[String]): List[State] = {
  val pattern = "<x=([-\\d]+), y=([-\\d]+), z=([-\\d]+)>".r
  input.map { line =>
    {
      line match {
        case pattern(xStr, yStr, zStr) =>
          State(List(xStr.toInt, yStr.toInt, zStr.toInt))
      }
    }
  }
}

def computeGravity(v1: Vector, v2: Vector): (Vector, Vector) = {
  val gravity1 = v1.zip(v2).map {
    case (d1, d2) => {
      if (d1 == d2) {
        0
      } else if (d1 > d2) {
        -1
      } else {
        1
      }
    }
  }
  val gravity2 = gravity1.map(_ * -1)
  (gravity1, gravity2)
}

def addVectors(v1: Vector, v2: Vector): Vector = {
  v1.zip(v2).map { case (d1, d2) => d1 + d2 }
}

def timeStep(states: List[State]): List[State] = {
  // apply gravity for every pair
  states
    .combinations(2)
    .flatMap { listOfLists => // compute gravity for each pair
      {
        val state1 = listOfLists(0)
        val state2 = listOfLists(1)
        val (gravity1, gravity2) =
          computeGravity(state1.position, state2.position)
        List((state1, gravity1), (state2, gravity2))
      }
    }
    .toList
    .groupBy(_._1)
    .mapValues(_.map(_._2))
    .map { // sum gravity and apply to each position
      case (state, updates) => {
        val newVelocity =
          addVectors(state.velocity, updates.reduce(addVectors(_, _)))
        val newPosition = addVectors(state.position, newVelocity)
        State(
          newPosition,
          newVelocity
        )
      }
    }
    .toList
}

def part1(input: List[String], steps: Int = 1000) = {

  var states = parseInput(input)
  //println(states)
  (1 to steps).foreach { idx =>
    {

      states = timeStep(states)
      //println(states)
    }
  }

  val totalEnergy = states.map(_.energy()).sum

  totalEnergy
}

val testCases = Seq(
  (
    """<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>""".split("\n").toList,
    179
  )
)

//println(part1(testCases(0)._1, 10))
println(part1(rawInput, 1000))
