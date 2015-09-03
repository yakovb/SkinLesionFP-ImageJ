package dermatological

import images.{Image, ParImage}
import operations.Transformation

object FillHoles {

  def fillHoles = (im: Image[Byte]) =>
    FillHolesTransform(im) transform

  case class FillHolesTransform(im: Image[Byte]) extends Transformation {
    val WHITE = 255.toByte
    val TEMPCOLOUR = 100.toByte
    val BLACK = 0.toByte

    def transform = {
      val tempArr = floodFillWithTempColour(im.matrix.toArray, 0, 0)
      val result = invertAndFill(tempArr)
      ParImage(result.par, im.width, im.height)
    }

    private def calculateIndex(row: Int, col: Int) = im.width * row + col

    private def floodFillWithTempColour(source: Array[Byte], rowStart: Int, colStart: Int): Array[Byte] = {
      val array = new Array[Byte](im.height * im.width)
      im.matrix.copyToArray(array)
      array(0) = WHITE // starting pixel
      val stack = scala.collection.mutable.Stack[(Int,Int)]()  //stack stores points as (row, column)
      stack.push((rowStart, colStart))

      while (stack.nonEmpty) {
        val current = stack.pop()
        val row = current._1
        val westColStart, eastColStart = current._2
        var westColEnd = westColStart
        var eastColEnd = eastColStart

        while (westColEnd >= 0 && array(calculateIndex(row,westColEnd)) == WHITE) {westColEnd -= 1}
        westColEnd += 1 // correct overshoot
        while (eastColEnd < im.width && array(calculateIndex(row,eastColEnd)) == WHITE) {eastColEnd += 1}
        eastColEnd -= 1 // correct overshoot

        if (westColEnd != eastColEnd) {
          for (i <- westColEnd to eastColEnd) {
            array(calculateIndex(row, i)) = TEMPCOLOUR
            val testNorth = row - 1
            val testSouth = row + 1
            if (testNorth >= 0 && array(calculateIndex(testNorth, i)) == WHITE) stack.push((testNorth, i))
            if (testSouth < im.height && array(calculateIndex(testSouth, i)) == WHITE) stack.push((testSouth, i))
          }
        }
      }
      array
    }

    private def invertAndFill(array: Array[Byte]): Array[Byte] = {
      for {
        r <- 0 until im.height
        c <- 0 until im.width
      } {
        val oldColour = array(calculateIndex(r,c))
        val newColour = if (oldColour == TEMPCOLOUR) WHITE else BLACK
        array(calculateIndex(r,c)) = newColour
      }
      array
    }

  }



}
