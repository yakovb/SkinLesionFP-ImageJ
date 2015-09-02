import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import operations.InteropImageJ

class Median_Filter extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
  DOES_RGB

  override def run(ip: ImageProcessor): Unit = {
    import InteropImageJ._
    import operations.MyPipeline._

    val src = InteropImageJ.getIntParImage(ip)
    val resultIm = medianFilter(src)
    val ijResult = makeImagePlus("median filtered", makeColourProcessor(resultIm))
    ijResult show()
  }
}
