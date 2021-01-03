import $file.Util

val rawInput = Util.inputForDay(2).head

object AllDone extends Exception { }

def part1( rawInput: String, noun: Option[Int] = None, verb: Option[Int] = None): Array[Int] = {
    val array = rawInput.split(",").map(_.toInt)
    array(1) = noun.getOrElse(array(1))
    array(2) = verb.getOrElse(array(2))
    array
        .grouped(4)
        .takeWhile { 
            case Array(code, _*) => code != 99 
        }.foreach { 
            case Array(code, p1, p2, target) => {
                array(target) = code match {
                    case 1 => array(p1) + array(p2)
                    case 2 => array(p1) * array(p2)
                }
        }
    }
    array
}

val testCases = Seq(
    ("1,0,0,0,99", "2,0,0,0,99"),
    ("2,3,0,3,99", "2,3,0,6,99"),
    ("2,4,4,5,99,0", "2,4,4,5,99,9801"),
    ("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99")
)

testCases.foreach { 
    case(input, expected) => {
        val actual = part1(input).mkString(",")
        assert(actual == expected, s"Case ${input}:\nExpected ${actual} to equal ${expected}")
    }
}

println(part1(rawInput, Some(12), Some(2))(0))

// part 2
def part2() = {
    val target = 19690720

    (0 to 99).foreach { noun => {
        (0 to 99).foreach { verb => {
            if(part1(rawInput, Some(noun), Some(verb))(0) == target) {
                println(s"Found matching values ${noun}, ${verb}")
                println(s"Result: ${100 * noun + verb}")
            }
        }}
    }}
}


part2()