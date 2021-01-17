import $file.Util

val rawInput = Util.inputForDay(8).head

val WIDTH = 25
val HEIGHT = 6
val LAYER_SIZE = WIDTH * HEIGHT
val layers = rawInput
  .grouped(LAYER_SIZE)
  .toList

def part1(): Int = {
  val targetLayer = layers
    .sortBy(layer => "0".r.findAllIn(layer).size)
    .head

  "1".r.findAllIn(targetLayer).size * "2".r.findAllIn(targetLayer).size
}

def part2(): Unit = {
  // resolve each pixel one at a time, top down
  val pixelLists = layers.transpose
  val resolvedPixels = pixelLists.map(pixelList => {
    pixelList.reduceLeft((current, acc) => {
      current match {
        case '2'   => acc
        case other => other
      }
    })
  })

  // print resolved pixels
  resolvedPixels
    .grouped(WIDTH)
    .foreach(row => println(row.mkString("").replace("0", " ")))

}

println(part1())
part2()
