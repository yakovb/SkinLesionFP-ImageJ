import core._
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

/**
 * [[ij.plugin.filter.PlugInFilter]] that shows Gaussian blurred grey image
 */
class Gaussian_Blur extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = InteropImageJ.getByteParImage(ip)
    val result = dermatological.other_ops.PreProcessing.gaussianBlurGreyImage (src)
    val ijResult = InteropImageJ.makeGreyProcessor(result)
    InteropImageJ.makeImagePlus("Blurred image", ijResult) show()
  }

}