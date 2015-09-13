import core.InteropImageJ._
import dermatological.binary_ops.Asymmetry
import ij.ImagePlus
import ij.measure.ResultsTable
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

/**
 * [[ij.plugin.filter.PlugInFilter]] that shows asymmetrical region of a skin lesion
 */
class Asymmetry_Binary extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val src = getByteParImage(ip)
    val visual = Asymmetry.getOverlapImage (src)
    val result = Asymmetry.calculateAsymmetry (src)
    makeImagePlus("XOR-ed image", makeGreyProcessor(visual)) show()

    val table = new ResultsTable
    table.incrementCounter()
    table.addValue("Asymmetry measure", result)
    table.show("Asymmetry")
  }
}
