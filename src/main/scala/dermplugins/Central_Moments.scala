import core.InteropImageJ._
import dermatological.binary_ops.BinaryImage._
import dermatological.binary_ops.Moments
import ij.ImagePlus
import ij.measure.ResultsTable
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

/**
 * [[ij.plugin.filter.PlugInFilter]] that shows the central moments of a binary image
 */
class Central_Moments extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = getByteParImage(ip)
    val binary = otsuThreshold(src)
    val moms = Moments.getCentralMoments (binary)

    makeImagePlus("Binary image", makeGreyProcessor(binary)) show()
    val table = new ResultsTable
    table.incrementCounter()
    for (entry <- moms.toList.sorted) table.addValue(entry._1, entry._2)
    table.show("results")
  }
}

