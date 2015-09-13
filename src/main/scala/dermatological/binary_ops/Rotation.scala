package dermatological.binary_ops

import core.{Image, Operation, ParImage, Transformation}

/**
 * Provides methods for rotating a binary image about the central axis of the image's object
 */
object Rotation {

  /**
   * Partially applied function; requires [[core.Image]] as input to complete
   * @return binary image with depicted object rotated so that its major axis is horizontal
   */
  def rotate = (im: Image[Byte]) => {
    val rotatedWithHoles = TransformRotate(im, RotationOp()) transform;
    HolesAndSpecs.fillHoles (rotatedWithHoles)
  }

  private case class TransformRotate(im: Image[Byte], rotationOp: RotationOp) extends Transformation {
    val moments = Moments.getCentralMoments (im)

    def transform = {
      val resultArray = new Array[Byte](im.height * im.width)
      for (i <- resultArray.indices) resultArray(i) = 255.toByte
      val centX = moments("centroidX").round.toInt
      val centY = moments("centroidY").round.toInt
      val orientation = - (Math.atan2(2 * moments("c11"), moments("c20") - moments("c02")) / 2)

      for {
        r <- 0 until im.height
        c <- 0 until im.width
        if im.matrix(im.width * r + c) == 0
      } {
        val (x2, y2) = rotationOp.runOn(c, r, orientation, centX, centY)
        resultArray(im.width * y2 + x2) = 0
      }
      ParImage(resultArray.par, im.width, im.height)
    }
  }

  private case class RotationOp() extends Operation {

    def runOn(x: Int, y: Int, angle: Double, centroidX: Int, centroidY: Int) = {
      val sine = Math.sin(angle)
      val cosine = Math.cos(angle)

      val xnew = x - centroidX
      val ynew = y - centroidY

      val rotx = xnew * cosine - ynew * sine
      val roty = xnew * sine + ynew * cosine

      ((rotx + centroidX).round.toInt, (roty + centroidY).round.toInt)
    }
  }
}
