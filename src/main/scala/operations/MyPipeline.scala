package operations

object MyPipeline {
  def getRed(pixel: Int): Int = 
    (pixel >> 16) & 0xff

  def getGreen(pixel: Int): Int =
    (pixel >> 8) & 0xff

  def getBlue(pixel: Int): Int =
    pixel & 0xff
}
