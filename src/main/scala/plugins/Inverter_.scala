import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import images.Image
import operations.PointOp

class Inverter_ extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G + DOES_STACKS + SUPPORTS_MASKING + NO_CHANGES

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Int]]
    val image = Image.makeImageInt(pixels, ip.getWidth, ip.getHeight)
    val step1 = image.processPoints( PointOp(255 - _) )

    ip.setPixels(step1.matrix.toArray)
  }
}
