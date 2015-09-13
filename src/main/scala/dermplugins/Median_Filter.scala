import core.InteropImageJ._
import dermatological.other_ops.PreProcessing._
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

class Median_Filter extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
  DOES_RGB

  override def run(ip: ImageProcessor): Unit = {
    val src = getIntParImage(ip)
    val resultIm = medianFilter(src)
    val ijResult = makeImagePlus("median filtered", makeColourProcessor(resultIm))
    ijResult show()
  }
}
