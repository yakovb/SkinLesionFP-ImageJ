import core.InteropImageJ._
import dermatological.HolesAndSpecs
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

class Kill_Small_Blobs extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = getByteParImage(ip)
    val cleaned = HolesAndSpecs.removeSpecs (src)
    makeImagePlus("Single Blob", makeGreyProcessor(cleaned)) show()
  }
}