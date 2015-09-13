import core.InteropImageJ
import dermatological.colour_ops.ColourOps._
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import ij.{IJ, ImagePlus}

class RGB_to_XYZ extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
  DOES_ALL + DOES_STACKS

  override def run(ip: ImageProcessor): Unit = {
    val src = InteropImageJ.getIntParImage(ip)
    val xyz = rgb_to_xyz(src)
    val result = InteropImageJ.arrayFloatImage_to_IJstack("XYZ", xyz)
    result show()
    IJ.resetMinAndMax()
  }
}
