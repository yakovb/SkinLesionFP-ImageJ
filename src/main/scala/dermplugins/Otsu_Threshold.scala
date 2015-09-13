import core.InteropImageJ
import dermatological.binary_ops.BinaryImage._
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

/**
 * [[ij.plugin.filter.PlugInFilter]] that filters a grey image using the Otsu method
 */
class Otsu_Threshold extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = InteropImageJ.getByteParImage(ip)
    val binary = otsuThreshold(src)
    InteropImageJ.makeImagePlus("Binary image", InteropImageJ.makeGreyProcessor(binary)) show()
  }
}