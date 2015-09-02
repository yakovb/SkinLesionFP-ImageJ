package dermatological.binary_ops

import images.{Image, ParImage}
import operations._

object BinaryImage {

  def otsuThreshold =
    OtsuTransform(_: Image[Byte]) transform


  def greyHistogram = TransformOneChannelToHistogram(_: Image[Byte], Histo_1ChannelTraverse()) transform


  case class MakeBinary(threshold: Int) extends PointOperation[Byte,Byte] {
    override def runOn(pixel: Byte): Byte =
      doThreshold (pixel, threshold)

    private def doThreshold(pixel: Byte, t: Int) =
      if (pixel < t.toByte) 255.toByte else 0.toByte
  }


  case class OtsuTransform(im: Image[Byte]) extends Transformation {

    def transform: Image[Byte] = {
      val newMat = PointTraverse() traverse (im, MakeBinary(getThreshold))
      ParImage(newMat, im.width, im.height)
    }

    private def getThreshold = {
      val histo = greyHistogram (im)
      val sumTotal = histo.foldLeft(0){ case (sum, (pixVal, count)) => sum + (pixVal * count) }

      def go(grey: Int, sumBack: Int, wBack: Int, wFront: Int, varMax: Double, thresh: Int): Int = {
        if (grey > 255) thresh

        else {
          val tempWB = wBack + histo.getOrElse(grey.toByte, 0)
          val tempWF = im.matrix.length - tempWB

          if (tempWB == 0) go(grey+1, sumBack, tempWB, wFront, varMax, thresh)

          else if (tempWF == 0) thresh

          else {
            val tempSumB = sumBack + grey * histo.getOrElse(grey.toByte, 0)
            val meanBack = tempSumB.toFloat / tempWB.toFloat
            val meanFront = (sumTotal.toFloat - tempSumB.toFloat) / tempWF.toFloat
            val btwVar = tempWB.toFloat * tempWF.toFloat * Math.pow(meanBack - meanFront, 2)

            val (newVarMax, newThresh) = if (btwVar > varMax) (btwVar, grey) else (varMax, thresh)
            go(grey+1, tempSumB, tempWB, tempWF, newVarMax, newThresh)
          }
        }
      }
      go(0,0,0,0,0,0)
    }

  }

}
