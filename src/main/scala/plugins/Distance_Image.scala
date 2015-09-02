import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import operations.{InteropImageJ, MyPipeline}

class Distance_Image extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_ALL + DOES_STACKS

  override def run(ip: ImageProcessor): Unit = {
    import MyPipeline._
    val src = InteropImageJ.getIntParImage(ip)
    val cielab = (rgb_to_xyz andThen xyz_to_Lab) (src)
    val backgroundMedian = getBackgroundMedian (cielab)
    val distancePic = distanceImage (cielab, backgroundMedian)

    InteropImageJ.makeImagePlus("Distance Image", InteropImageJ.makeFloatImage(distancePic)) show()
  }
}
