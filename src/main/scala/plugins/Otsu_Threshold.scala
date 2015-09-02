import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import operations.{InteropImageJ, MyPipeline}

class Otsu_Threshold extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = InteropImageJ.getByteParImage(ip)
    val binary = MyPipeline.otsuThreshold(src)
    InteropImageJ.makeImagePlus("Binary image", InteropImageJ.makeGreyProcessor(binary)) show()
  }
}