import core.InteropImageJ._
import dermatological.binary_ops.Moments
import dermatological.other_ops.Perimeter
import ij.ImagePlus
import ij.measure.ResultsTable
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

/**
 * [[ij.plugin.filter.PlugInFilter]] that marks a binary skin lesion's perimeter and calculates border irregularity measure
 */
class Border_Irregularity extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = getByteParImage(ip)
    val result = Perimeter.markPerimeter (src)
    makeImagePlus("Marked Perimeter", makeGreyProcessor(result)) show()

    val perimeterMeasure = Perimeter.countPerimeter (src)
    val areaMeasure = Moments.getCentralMoments (src) ("area")
    val circularityMeasure = 4 * Math.PI * areaMeasure / Math.pow(perimeterMeasure, 2)

    val table = new ResultsTable
    table.incrementCounter()
    table.addValue("Border irregularity", circularityMeasure)
    table.show("Border irregularity measure")
  }

}
