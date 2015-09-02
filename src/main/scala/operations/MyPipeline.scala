package operations

import images.Image

object MyPipeline {
  def getRedDirect(pixel: Int) = (pixel >> 16) & 0xff
  def getGreenDirect(pixel: Int) = (pixel >> 8) & 0xff
  def getBlueDirect(pixel: Int) = pixel & 0xff
  def combineRgbDirect(rgbList: List[Int]) = rgbList match {
    case List(r,g,b) =>
      ((r & 0xff) << 16) + ((g & 0xff) << 8) + (b & 0xff)
  }

  def getRed = PointOp_1Channel(getRedDirect)
  def getGreen = PointOp_1Channel(getGreenDirect)
  def getBlue = PointOp_1Channel(getBlueDirect)

  def medianOneChannel(region: List[Int]) = {
    val sorted = region.sorted
    val l = sorted.length
    if (l % 2 == 0) (sorted(l / 2) + sorted(l / 2 + 1)) / 2
    else sorted(l / 2)
  }

  def splitRgbRegion(region: List[Int]) = {
    val reds = region map getRedDirect
    val greens = region map getGreenDirect
    val blues = region map getBlueDirect
    List(reds,greens,blues)
  }





  def medianFilter = {

    def medianThreeChannel(mixedRegion: List[Int]) = {
      val medians = splitRgbRegion(mixedRegion) map medianOneChannel
      combineRgbDirect(medians)
    }
    def medianOp = NonLinearFilterNoKernel(3, medianThreeChannel)
    TransformNeighbourhood(_: Image[Int], NeighbourTraverse(), medianOp) transform
  }


  def rgb_PointToThreeChannel =
    TransformSimple(_: Image[Int], PointTraverse(), getRed, getGreen, getBlue) transform



}
