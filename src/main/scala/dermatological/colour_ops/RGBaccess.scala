package dermatological.colour_ops

import core.PointOp_1Channel

/**
 * Utility methods for separating and combining the colour components of an RGB pixel represented by an [[scala.Int]]
 */
object RGBaccess {

  /**
   * Takes a list of RGB pixels and returns a list of separated colour components
   * @param region input region of RGB [[scala.Int]] pixels
   * @return [[scala.collection.immutable.List]] of colour channels, themselves represented as [[scala.collection.immutable.List]]s of pixels
   */
  def splitRgbRegion(region: List[Int]) = {
    val reds = region map getRedDirect
    val greens = region map getGreenDirect
    val blues = region map getBlueDirect
    List(reds,greens,blues)
  }

  /**
   * Takes a list of colour channel pixels and combines them into a single packed [[scala.Int]]
   * @param rgbList [[scala.collection.immutable.List]] of colour channel pixels
   * @return RGB packed [[scala.Int]]
   */
  def combineRgbDirect(rgbList: List[Int]) = rgbList match {
    case List(r,g,b) =>
      ((r & 0xff) << 16) + ((g & 0xff) << 8) + (b & 0xff)
  }

  private def getRedDirect(pixel: Int) = (pixel >> 16) & 0xff
  private def getGreenDirect(pixel: Int) = (pixel >> 8) & 0xff
  private def getBlueDirect(pixel: Int) = pixel & 0xff

  private def getRed = PointOp_1Channel(getRedDirect)
  private def getGreen = PointOp_1Channel(getGreenDirect)
  private def getBlue = PointOp_1Channel(getBlueDirect)

}
