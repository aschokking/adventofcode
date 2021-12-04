// Ammonite 2.3.8, Scala 2.13.4
import $file.Util

val testInput = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7""".split("\n").toList

val rawInput = Util.inputForDay(4)

type Board = List[List[Int]]

val MARK = -1

def parseInput(input: List[String]) = {
    val numbers = input.head.split(",").map(_.toInt)

    var boards: List[Board] = List()
    for(i <- 2 until input.size by 6) {
        boards = boards :+ parseBoard(input, i)
    }
    (numbers, boards)
}

def parseBoard(input: List[String], startLine: Int): Board = {
    val boardLists = input
        .slice(startLine, startLine + 5)
        .map(
            _.trim.split(" +").toList
        )
    boardLists.map(_.map(_.toInt).toList)
}

def markBoards(boards: List[Board], number: Int): List[Board] = {
    boards.map(_.map(_.map(entry => {
        if(entry == number) {
            MARK
        } else {
            entry
        }
    }
    )))
}

def part1(input: List[String]) = {
    var (numbers, boards) = parseInput(input)

    var winningBoard: Option[Board] = None
    val finalNumber = numbers.find(number => {
        boards = markBoards(boards, number)

        // see if there's a winning board
        winningBoard = tryFindWinners(boards).headOption
        winningBoard.isDefined
    })

    winningBoard.head.map(_.filter(_ != MARK).sum).sum * finalNumber.head
}

def tryFindWinners(boards: List[Board]) = {
    boards.filter(
        board => board.map(_.max).min == MARK || board.transpose.map(_.max).min == MARK
    )
}

assert (part1(testInput) == 4512)
println(part1(rawInput))


def part2(input: List[String]) = {
    var (numbers, boards) = parseInput(input)

    val finalNumber = numbers.find(number => {
        boards = markBoards(boards, number)

        if(tryFindWinners(boards).size == 1 && boards.size == 1) {
            true
        } else {
            // remove any winning boards
            boards = boards.filterNot(
                board => board.map(_.max).min == MARK || board.transpose.map(_.max).min == MARK
            )
            false
        }
    })

    boards.head.map(_.filter(_ != MARK).sum).sum * finalNumber.head
}

assert (part2(testInput) == 1924)
println(part2(rawInput))
