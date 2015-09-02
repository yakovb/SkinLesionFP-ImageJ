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

  def rgbStep(pixel: Int) =
    List(getRedDirect(pixel), getGreenDirect(pixel), getBlueDirect(pixel))

  def xyzStep(pixel: Int) = {
    pixel / 255f match {
      case f if f > 0.04045 => Math.pow((f + 0.055) / 1.055, 2.4) * 100
      case f => f / 12.92 * 100f
    }
  }
  def makeX(rgb: List[Double]) = rgb match {
    case List(r,g,b) => r * 0.4124 + g * 0.3576 + b * 0.1805
  }
  def makeY(rgb: List[Double]) = rgb match {
    case List(r,g,b) => r * 0.2126 + g * 0.7152 + b * 0.0722
  }
  def makeZ(rgb: List[Double]) = rgb match {
    case List(r,g,b) => r * 0.0193 + g * 0.1192 + b * 0.9505
  }

  def red_to_x = (rgbStep(_: Int) map xyzStep) andThen makeX
  def red_to_y = (rgbStep(_: Int) map xyzStep) andThen makeY
  def red_to_z = (rgbStep(_: Int) map xyzStep) andThen makeZ



}
