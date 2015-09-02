package operations

import images.Image

object MyPipeline {
  def getRed(pixel: Int) = (pixel >> 16) & 0xff
  def getGreen(pixel: Int) = (pixel >> 8) & 0xff
  def getBlue(pixel: Int) = pixel & 0xff
  def combineRgb(rgbList: List[Int]) = rgbList match {
    case List(red,green,blue) =>
      val r = (red << 16) & 0xff
      val g = (green << 8) & 0xff
      val b = blue  & 0xff
      r + g + b
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
    List(reds,blues,greens)
  }


  def medianThreeChannel(mixedRegion: List[Int]) = {
    val medians = splitRgbRegion(mixedRegion) map medianOneChannel
    combineRgb(medians)
  }

  def medianOp = NonLinearFilterNoKernel(9, medianThreeChannel)

  def medianFilter = TransformNeighbourhood(_: Image[Int], NeighbourTraverse(), medianOp) transform
}
