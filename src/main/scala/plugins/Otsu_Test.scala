import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import images.ParImage
import operations.Histo_1ChannelTraverse

class Otsu_Test extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val w = ip.getWidth
    val h = ip.getHeight

    val greyHisto = Histo_1ChannelTraverse().traverse(ParImage(pixels.par, w, h))
    val sumTotal = greyHisto.foldLeft(0){ case (sum, (pixVal, count)) => sum + (pixVal * count) }
    val length = pixels.length

    def otsu(grey: Int, sumBack: Int, wBack: Int, wFront: Int, varMax: Double, thresh: Int): Int = {
      if (grey > 255) thresh
      else {
        val tempWB = wBack + greyHisto.getOrElse(grey.toByte, 0)
        val tempWF = length - tempWB
        if (tempWB == 0) otsu(grey+1, sumBack, tempWB, wFront, varMax, thresh)
        else if (tempWF == 0) thresh
        else {
          val tempSumB = sumBack + grey * greyHisto.getOrElse(grey.toByte, 0)
          val meanBack = tempSumB.toFloat / tempWB.toFloat
          val meanFront = (sumTotal.toFloat - tempSumB.toFloat) / tempWF.toFloat
          val btwVar = tempWB.toFloat * tempWF.toFloat * Math.pow(meanBack - meanFront, 2)

          val (newVarMax, newThres) = if (btwVar > varMax) (btwVar, grey) else (varMax, thresh)
          otsu(grey+1, tempSumB, tempWB, tempWF, newVarMax, newThres)
        }
      }
    }
    val threshold = otsu(0, 0, 0, 0, 0, 0)
    for {
      r <- 0 until h
      c <- 0 until w
    } {
      if (pixels(w * r + c) < threshold.toByte) pixels(w * r + c) = 255.toByte
      else pixels(w * r + c) = 0
    }

  }
}