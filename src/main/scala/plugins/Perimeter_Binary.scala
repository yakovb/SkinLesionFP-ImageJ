import dermatological.Perimeter
import dermatological.binary_ops.Moments
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import operations.InteropImageJ._

class Perimeter_Binary extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = getByteParImage(ip)
    val result = Perimeter.markPerimeter (src)
    val perimeter = result.matrix count (_ == 100.toByte)
    makeImagePlus("Marked Perimeter", makeGreyProcessor(result)) show()
    val area = Moments.getCentralMoments(src)("area")
    val circularity = 4 * Math.PI * area / Math.pow(perimeter * 0.95, 2)
    println(s"circularity is $circularity")
  }

}
