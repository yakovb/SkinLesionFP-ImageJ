import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.{ColorProcessor, ImageProcessor}
import images.ParImage

class Median_Filter extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
  DOES_ALL

  override def run(ip: ImageProcessor): Unit = {
    import operations.MyPipeline._
    val pixels = ip.getPixels.asInstanceOf[Array[Int]]
    val image = ParImage[Int](pixels.par, ip.getWidth, ip.getHeight)

    val transformedImage = transformToThreeChannel(image).transform

    val result = new ColorProcessor(transformedImage.width, transformedImage.height, transformedImage.matrix.toArray)
    new ImagePlus("grey pic", result) show()
  }
}
