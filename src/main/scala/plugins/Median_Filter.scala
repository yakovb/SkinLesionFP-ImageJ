import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import operations.InteropImageJ

class Median_Filter extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
  DOES_RGB

  override def run(ip: ImageProcessor): Unit = {
    import operations.MyPipeline._
    val src = InteropImageJ.getIntParImage(ip)
    val result = medianFilter(src)
    val ijResult = InteropImageJ.makeColourImage("Median filtered", result)
    ijResult show()
  }
}
