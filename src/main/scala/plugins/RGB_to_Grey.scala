import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.{ByteProcessor, ImageProcessor}
import images.ParImage
import operations.{Funcs, PointTraverse, TransformSimple}

class RGB_to_Grey extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_RGB + DOES_STACKS + SUPPORTS_MASKING

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Int]]
    val image = ParImage(pixels.par, ip.getWidth, ip.getHeight)

    val transformedImage = TransformSimple[Int,Byte](image, PointTraverse(), Funcs.rgb_2_grey) transform

    val result = new ByteProcessor(transformedImage.width, transformedImage.height, transformedImage.matrix.toArray)
    new ImagePlus("grey pic", result) show
  }
}
