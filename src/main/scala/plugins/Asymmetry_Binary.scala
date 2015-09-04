import dermatological.Asymmetry
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import operations.InteropImageJ._

class Asymmetry_Binary extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = getByteParImage(ip)
    val visual = Asymmetry.getOverlapImage (src)
    val result = Asymmetry.calculateAsymmetry (src)
    makeImagePlus("XOR-ed image", makeGreyProcessor(visual)) show()
  }
}
