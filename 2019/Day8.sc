import $file.Util

val rawInput = Util.inputForDay(8).head

def part1(rawInput: String, layerSize: Int = 25 * 6): Int = {
  val targetLayer = rawInput
    .grouped(layerSize)
    .map(_.toString)
    .toList
    .sortBy(layer => "0".r.findAllIn(layer).size)
    .head

  "1".r.findAllIn(targetLayer).size * "2".r.findAllIn(targetLayer).size
}

def part2(rawInput: String, width: Int = 25, height: Int = 6): Unit = {
  val layerSize = width * height
  val layers = rawInput.grouped(layerSize).toList
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
    .grouped(width)
    .foreach(row => println(row.mkString("").replace("0", " ")))

}

println(part1(rawInput))
part2(rawInput)
