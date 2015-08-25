import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import ij.{IJ, ImagePlus, ImageStack}
import images.ParImage
import operations._

class RGB_to_XYZ extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_RGB + DOES_STACKS

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Int]]
    val image = ParImage[Int](pixels.par, ip.getWidth, ip.getHeight)

    val transformedImage: ParImage[Array[Float]] = TransformSimple[Int,Array[Float]](image, PointTraverse(), Funcs.rgb2xyz_Combiner) transform
    val c1 = (for (arr <- transformedImage.matrix) yield arr(0)).toArray
    val c2 = (for (arr <- transformedImage.matrix) yield arr(1)).toArray
    val c3 = (for (arr <- transformedImage.matrix) yield arr(2)).toArray


    val sstack = new ImageStack(transformedImage.width, transformedImage.height)
    sstack.addSlice("X",c1)
    sstack.addSlice("Y",c2)
    sstack.addSlice("Z",c3)

    val imluv = new ImagePlus("XYZ",sstack)
    imluv.show()
    IJ.resetMinAndMax()
  }
}
