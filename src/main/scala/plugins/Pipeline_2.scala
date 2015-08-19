import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.{ByteProcessor, ImageProcessor}
import images.ParImage
import operations.{Funcs, PointTraverse, TransformSimple}

class Pipeline_2 extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_ALL + DOES_STACKS + SUPPORTS_MASKING

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Int]]
    val image = ParImage(pixels.par, ip.getWidth, ip.getHeight)

    val transformedImage = {
      val step1: ParImage[Byte] = TransformSimple[Int,Byte](image, PointTraverse(), Funcs.rgb_2_grey) transform;
      TransformSimple[Byte,Byte](step1, PointTraverse(), Funcs.invert) transform
    }

    val result = new ByteProcessor(transformedImage.width, transformedImage.height, transformedImage.matrix.toArray)
    new ImagePlus("grey pic", result) show
  }



}
