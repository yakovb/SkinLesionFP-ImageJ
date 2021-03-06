import core.InteropImageJ._
import dermatological.binary_ops.{BinaryImage, HolesAndSpecs}
import dermatological.colour_ops.ColourOps
import dermatological.other_ops.{LesionMask, MaskMaking}
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

/**
 * [[ij.plugin.filter.PlugInFilter]] that masks background skin from a colour image of a skin lesion
 */
class Colour_Lesion_Mask extends PlugInFilter {
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

    makeImagePlus("Masked Image", makeColourProcessor(maskedImage)) show()
  }
}