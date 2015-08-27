import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

class Fill_Holes_Test extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val w = ip.getWidth
    val h = ip.getHeight

    val WHITE = 255.toByte
    val BLACK = 0.toByte
    val TEMPCOLOUR = 100.toByte

    def calculateIndex(row: Int, col: Int) = w * row + col

    def floodFill(rowStart: Int, colStart: Int, targetColour: Byte, replacementColour: Byte): Unit = {
      val stack = scala.collection.mutable.Stack[(Int,Int)]() // stored as (row,col)
      stack.push((rowStart, colStart))

      while (stack.nonEmpty) {
        val current = stack.pop()
        val row = current._1
        val westColStart, eastColStart = current._2
        var westColEnd = westColStart
        var eastColEnd = eastColStart

        while (westColEnd >= 0 && pixels(calculateIndex(row,westColEnd)) == targetColour) {westColEnd -= 1}
        westColEnd += 1 // correct overshoot
        while (eastColEnd < w && pixels(calculateIndex(row,eastColEnd)) == targetColour) {eastColEnd += 1}
        eastColEnd -= 1 // correct overshoot

        if (westColEnd != eastColEnd) {
          for (i <- westColEnd to eastColEnd) {
            pixels(calculateIndex(row, i)) = replacementColour
            val testNorth = row - 1
            val testSouth = row + 1
            if (testNorth >= 0 && pixels(calculateIndex(testNorth, i)) == targetColour) stack.push((testNorth, i))
            if (testSouth < h && pixels(calculateIndex(testSouth, i)) == targetColour) stack.push((testSouth, i))
          }
        }
      }
    }

    pixels(0) = WHITE // starting pixel
    floodFill(0,0,WHITE,TEMPCOLOUR)

    for {
      r <- 0 until h
      c <- 0 until w
    } {
      val oldColour = pixels(calculateIndex(r,c))
      val newColour = if (oldColour == TEMPCOLOUR) WHITE else if (oldColour == WHITE) BLACK else BLACK
      pixels(calculateIndex(r,c)) = newColour
    }
  }
}
