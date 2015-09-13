import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

class Filter_Blob_Centroid extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val w = ip.getWidth
    val h = ip.getHeight

    val WHITE = 255.toByte
    val BLACK = 0.toByte
    val TEMPCOLOUR = 100.toByte

    var m00 = 0.0
    var m10 = 0.0
    var m01 = 0.0

    // get moments
    for {
      r <- 0 until h
      c <- 0 until w
      if pixels(w * r + c) == 0
    } {
      m00 += 1
      m10 += c
      m01 += r
    }
    val centroidX = m10 / m00
    val centroidY = m01 / m00

    floodFill(centroidY.toInt, centroidX.toInt, BLACK, TEMPCOLOUR, ip)
    for {
      r <- 0 until h
      c <- 0 until w
    } {
      val i = calculateIndex(w,r,c)
      pixels(i) match {
        case BLACK => pixels(i) = WHITE
        case TEMPCOLOUR => pixels(i) = BLACK
        case _ => pixels(i) = WHITE
      }
    }

  }

  def calculateIndex(w: Int, row: Int, col: Int) = w * row + col

  def floodFill(rowStart: Int, colStart: Int, targetColour: Byte, replacementColour: Byte, ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val w = ip.getWidth
    val h = ip.getHeight

    val stack = scala.collection.mutable.Stack[(Int,Int)]() // stored as (row,col)
    stack.push((rowStart, colStart))

    while (stack.nonEmpty) {
      val current = stack.pop()
      val row = current._1
      val westColStart, eastColStart = current._2
      var westColEnd = westColStart
      var eastColEnd = eastColStart

      while (westColEnd >= 0 && pixels(calculateIndex(w, row,westColEnd)) == targetColour) {westColEnd -= 1}
      westColEnd += 1 // correct overshoot
      while (eastColEnd < w && pixels(calculateIndex(w, row,eastColEnd)) == targetColour) {eastColEnd += 1}
      eastColEnd -= 1 // correct overshoot

      if (westColEnd != eastColEnd) {
        for (i <- westColEnd to eastColEnd) {
          pixels(calculateIndex(w, row, i)) = replacementColour
          val testNorth = row - 1
          val testSouth = row + 1
          if (testNorth >= 0 && pixels(calculateIndex(w, testNorth, i)) == targetColour) stack.push((testNorth, i))
          if (testSouth < h && pixels(calculateIndex(w, testSouth, i)) == targetColour) stack.push((testSouth, i))
        }
      }
    }
  }

}
