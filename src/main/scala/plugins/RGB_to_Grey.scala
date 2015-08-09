import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.{ColorProcessor, ImageProcessor}
import images.Image
import operations.PointOpRGB

class RGB_to_Grey extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_RGB + DOES_STACKS + SUPPORTS_MASKING

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Int]]
    val image = Image.makeImageInt(pixels, ip.getWidth, ip.getHeight)
    val step1 = image.processPoints(
      PointOpRGB(
        (r: Int) => (r * .2126).toInt,
        (g: Int) => (g * .7152).toInt,
        (b: Int) => (b * .0722).toInt ) )

    val result = new ColorProcessor(step1.width, step1.height, step1.matrix.toArray)
    new ImagePlus("grey pic", result) show
  }
}
