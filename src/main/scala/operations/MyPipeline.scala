package operations

import images.Image

object MyPipeline {
  def getRed(pixel: Int) = (pixel >> 16) & 0xff
  def getGreen(pixel: Int) = (pixel >> 8) & 0xff
  def getBlue(pixel: Int) = pixel & 0xff
  def combineRgb(rgbList: List[Int]) = rgbList match {
    case List(r,g,b) =>
      ((r & 0xff) << 16) + ((g & 0xff) << 8) + (b & 0xff)
  }

  def medianOneChannel(region: List[Int]) = {
    val sorted = region.sorted
    val l = sorted.length
    if (l % 2 == 0) (sorted(l / 2) + sorted(l / 2 + 1)) / 2
    else sorted(l / 2)
  }

  def splitRgbRegion(region: List[Int]) = {
    val reds = region map getRed
    val greens = region map getGreen
    val blues = region map getBlue
    List(reds,greens,blues)
  }


  def medianThreeChannel(mixedRegion: List[Int]) = {
    val medians = splitRgbRegion(mixedRegion) map medianOneChannel
    combineRgb(medians)
  }

  def medianOp = NonLinearFilterNoKernel(3, medianThreeChannel)

  def medianFilter = TransformNeighbourhood(_: Image[Int], NeighbourTraverse(), medianOp) transform
}
