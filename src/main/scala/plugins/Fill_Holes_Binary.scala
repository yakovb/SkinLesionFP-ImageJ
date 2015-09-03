import dermatological.FillHoles
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import operations.InteropImageJ._

class Fill_Holes_Binary extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = getByteParImage(ip)
    val filled = FillHoles.fillHoles (src)
    makeImagePlus("Filled holes", makeGreyProcessor(filled)) show()
  }
}