import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.{ByteProcessor, ImageProcessor}
import images.ParImage
import operations.PointOpRGB

class RGB_to_Grey extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_RGB + DOES_STACKS + SUPPORTS_MASKING

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Int]]
    val image = ParImage(pixels.par, ip.getWidth, ip.getHeight)

    val transformedImage = image.traversePoints[Byte](
      PointOpRGB[Double,Byte]
        (_ * 0.2126, _ * 0.7152, _ * 0.0722)
        ((r,g,b) => (r+g+b).toByte) )

    val result = new ByteProcessor(transformedImage.width, transformedImage.height, transformedImage.matrix.toArray)
    new ImagePlus("grey pic", result) show
  }
}
