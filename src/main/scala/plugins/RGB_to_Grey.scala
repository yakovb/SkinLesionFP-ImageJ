import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import operations._

class RGB_to_Grey extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_RGB

  override def run(ip: ImageProcessor): Unit = {
    val src = InteropImageJ.getIntParImage(ip)
    val rgb = MyPipeline.rgb_2_grey(src)
    InteropImageJ.makeImagePlus("Grey image", InteropImageJ.makeGreyProcessor(rgb)) show()
  }
}
