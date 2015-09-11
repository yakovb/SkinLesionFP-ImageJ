import core.InteropImageJ._
import dermatological.colour_ops.ColourOps._
import dermatological.colour_ops.DistanceImage._
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

class Distance_Image extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_ALL + DOES_STACKS

  override def run(ip: ImageProcessor): Unit = {
    val src = getIntParImage(ip)
    val cielab = (rgb_to_xyz andThen xyz_to_Lab) (src)
    val backgroundMedian = getBackgroundMedian (cielab)
    val distancePic = distanceImage (cielab, backgroundMedian)

    makeImagePlus("Distance Image", makeFloatProcessor(distancePic)) show()
  }
}
