import core.InteropImageJ
import dermatological.binary_ops._
import dermatological.colour_ops.ColourOps
import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

class Analyser_Full extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_ALL

  override def run(ip: ImageProcessor): Unit = {
    new Colour_Lesion_Mask().run(ip)
    new Colour_Variegation().run(ip)

    val original = InteropImageJ.getIntParImage(ip)
    val pipelineMakeBinary =
    ColourOps.rgb_2_grey andThen
    BinaryImage.otsuThreshold andThen
    HolesAndSpecs.fillHoles andThen
    HolesAndSpecs.removeSpecs andThen
    Rotation.rotate

    val binary = pipelineMakeBinary (original)

    val ipBinary = InteropImageJ.makeGreyProcessor(binary)
    new Asymmetry_Binary().run(ipBinary)
    new Border_Irregularity().run(ipBinary)
  }
}
