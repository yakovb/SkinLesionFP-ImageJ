import dermatological.colour_ops.ColourOps._
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import ij.{IJ, ImagePlus}
import operations.InteropImageJ

class RGB_to_CIELab extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
  DOES_ALL + DOES_STACKS

  override def run(ip: ImageProcessor): Unit = {
    val src = InteropImageJ.getIntParImage(ip)
    val pipeline = rgb_to_xyz andThen xyz_to_Lab
    val Lab = pipeline (src)
    val result = InteropImageJ.arrayFloatImage_to_IJstack("CIELab", Lab)
    result show()
    IJ.resetMinAndMax()
  }
}
