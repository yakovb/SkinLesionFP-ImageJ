package dermatological.binary_ops

import core.{Image, ParImage, Transformation}

/**
 * Provides methods for filling holes and removing small specks from binary images
 */
object HolesAndSpecs {

  private val TEMPCOLOUR = 100.toByte
  private val WHITE = 255.toByte
  private val BLACK = 0.toByte

  /**
   * Partially applied function; requires [[core.Image]] as input to complete
   * @return binary image with all holes (i.e. spaces bounded by internal contours) filled in black
   */
  def fillHoles = (im: Image[Byte]) =>
    FillHolesTransform(im, WHITE, BLACK, (0,0)) transform

  /**
   * Partially applied function; requires [[core.Image]] as input to complete
   * @return binary image will all small specks removed, leaving a single object remaining in the image
   */
  def removeSpecs = (im: Image[Byte]) => {
    val centX = Moments.getCentralMoments(im)("centroidX").round.toInt
    val centY = Moments.getCentralMoments(im)("centroidY").round.toInt
    FillHolesTransform(im, BLACK, WHITE, (centX, centY)) transform
  }

  private case class FillHolesTransform(im: Image[Byte],
                                colourToFill: Byte,
                                leaveAloneColour: Byte,
                                startPointXY: (Int,Int)) extends Transformation {

    def transform: Image[Byte] = {
      val tempArr = floodFillWithTempColour(im.matrix.toArray)
      val result = invertAndFill(tempArr)
      ParImage(result.par, im.width, im.height)
    }

    private def calculateIndex(row: Int, col: Int) = im.width * row + col

    private def floodFillWithTempColour(source: Array[Byte]): Array[Byte] = {
      val (rowStart, colStart) = (startPointXY._2, startPointXY._1)
      val array = new Array[Byte](im.height * im.width)
      im.matrix.copyToArray(array)
      array(calculateIndex(rowStart, colStart)) = colourToFill // starting pixel
      val stack = scala.collection.mutable.Stack[(Int,Int)]()  //stack stores points as (row, column)
      stack.push((rowStart, colStart))

      while (stack.nonEmpty) {
        val current = stack.pop()
        val row = current._1
        val westColStart, eastColStart = current._2
        var westColEnd = westColStart
        var eastColEnd = eastColStart

        while (westColEnd >= 0 && array(calculateIndex(row,westColEnd)) == colourToFill) {westColEnd -= 1}
        westColEnd += 1 // correct overshoot
        while (eastColEnd < im.width && array(calculateIndex(row,eastColEnd)) == colourToFill) {eastColEnd += 1}
        eastColEnd -= 1 // correct overshoot

        if (westColEnd != eastColEnd) {
          for (i <- westColEnd to eastColEnd) {
            array(calculateIndex(row, i)) = TEMPCOLOUR
            val testNorth = row - 1
            val testSouth = row + 1
            if (testNorth >= 0 && array(calculateIndex(testNorth, i)) == colourToFill) stack.push((testNorth, i))
            if (testSouth < im.height && array(calculateIndex(testSouth, i)) == colourToFill) stack.push((testSouth, i))
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
        val newColour = if (oldColour == TEMPCOLOUR) colourToFill else leaveAloneColour
        array(calculateIndex(r,c)) = newColour
      }
      array
    }

  }



}
