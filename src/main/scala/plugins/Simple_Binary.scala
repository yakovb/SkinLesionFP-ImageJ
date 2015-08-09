import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import images.ParImage
import operations.PointOp

class Simple_Binary extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int = DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val image = ParImage[Byte](pixels.par, ip.getWidth, ip.getHeight)
    val transformedImage = image.traversePoints[Byte](
      PointOp[Byte,Byte]( (p:Byte) =>
        if (p < 80) 255.toByte
        else 0.toByte ))
    ip.setPixels(transformedImage.matrix.toArray)
  }
}
