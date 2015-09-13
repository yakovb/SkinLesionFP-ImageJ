package dermatological.other_ops

import core._
import dermatological.colour_ops.RGBaccess._

/**
 * Provides methods for pre-processing (i.e. noise removal) of images
 */
object PreProcessing {

  /**
   * Partially applied function; requires colour [[core.Image]] as input to complete
   * @return [[core.Image]] with each colour channel passed through a median filter
   */
  def medianFilter: (Image[Int]) => Image[Int] = {
    def medianOp = NonLinearFilter(3, medianThreeChannel)
    TransformNeighbourhood(_: Image[Int], NeighbourTraverse(), medianOp) transform
  }

  /**
   * Partially applied function; requires grey [[core.Image]] as input to complete
   * @return [[core.Image]] with Gaussian blur of kernel size 3x3 applied
   */
  def gaussianBlurGreyImage: (Image[Byte]) => Image[Byte] = {
    TransformNeighbourhood(_: Image[Byte], NeighbourTraverse(), gaussBlur) transform
  }

  private def medianThreeChannel(mixedRegion: List[Int]) = {
    val medians = splitRgbRegion(mixedRegion) map medianOneChannel
    combineRgbDirect(medians)
  }

  private def medianOneChannel(region: List[Int]) = {
    val sorted = region.sorted
    val l = sorted.length
    if (l % 2 == 0) (sorted(l / 2) + sorted(l / 2 + 1)) / 2
    else sorted(l / 2)
  }

  private def gaussBlur =
    LinearFilter[Byte,Byte](Kernel(List(1,2,1,2,4,2,1,2,1),3,3), byte_2_float(), _.toByte, 1f/16)

  private def byte_2_float(b: Byte) = (b & 0xff).toFloat
}
