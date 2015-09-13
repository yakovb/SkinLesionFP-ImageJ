import core.InteropImageJ._
import dermatological.binary_ops.{BinaryImage, HolesAndSpecs}
import dermatological.colour_ops.{ColourOps, ColourVariegation}
import dermatological.other_ops.{LesionMask, MaskMaking}
import ij.ImagePlus
import ij.measure.ResultsTable
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

/**
 * [[ij.plugin.filter.PlugInFilter]] that shows colour variegation of a skin lesion
 */
class Colour_Variegation extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_ALL

  override def run(ip: ImageProcessor): Unit = {
    val src = getIntParImage(ip)

    val maskPipeline =
      ColourOps.rgb_2_grey  andThen
        BinaryImage.otsuThreshold  andThen
        HolesAndSpecs.fillHoles  andThen
        HolesAndSpecs.removeSpecs  andThen
        MaskMaking.maskSetBinary

    val mask = maskPipeline (src)
    val maskedImage = LesionMask.maskColourImage (src, mask)

    val histo = ColourVariegation.getVariegationMeasures (maskedImage)

    val table = new ResultsTable
    table.incrementCounter()
    table.addValue("Red variegation", histo("Red"))
    table.addValue("Green variegation", histo("Green"))
    table.addValue("Blue variegation", histo("Blue"))
    table.show("Variegation measures")
  }
}