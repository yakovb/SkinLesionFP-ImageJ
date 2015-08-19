import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.{ByteProcessor, ImageProcessor}
import images.ParImage
import operations.NeighbourhoodOp

class Identity_Filter extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G + DOES_STACKS + SUPPORTS_MASKING

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val image = ParImage[Byte](pixels.par, ip.getWidth, ip.getHeight)
    val transformedImage = image.traverseNeighbourhoods[Byte](
      NeighbourhoodOp[Byte,Int,Byte](
        (pairs: List[(Byte,Int)]) => (pairs map (p => p._1 * p._2)).sum.toByte)
        (List(0,0,0,0,1,0,0,0,0)
      )
    )
    val result = new ByteProcessor(transformedImage.width - 1, transformedImage.height - 1, transformedImage.matrix.toArray)
    new ImagePlus("grey pic", result) show()
  }
}
