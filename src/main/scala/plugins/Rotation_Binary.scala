import dermatological.binary_ops.Moments
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import images.{Image, ParImage}
import operations.InteropImageJ._
import operations._

class Rotation_Binary extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = getByteParImage(ip)
    val rotated = Rotation.rotate (src)
    makeImagePlus("Rotated image", makeGreyProcessor(rotated)) show()
  }
}

object Rotation {

  def rotate = (im: Image[Byte]) =>
    TransformRotate(im, RotationOp()) transform

  case class TransformRotate(im: Image[Byte], rotationOp: RotationOp) extends Transformation {
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

  case class RotationOp() extends Operation {

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