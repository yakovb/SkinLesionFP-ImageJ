import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

class Mask_Test extends PlugInFilter {
  val WHITE = 255.toByte
  val BLACK = 0.toByte
  val TEMPCOLOUR = 100.toByte
  val VISITED = 50.toByte
  def calculateIndex(w: Int, row: Int, col: Int) = w * row + col
  def calculateRowCol(h: Int, i: Int) = (i/h, i % h)

  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_ALL

  override def run(ip: ImageProcessor): Unit = {
    val workset = ip.getPixels.asInstanceOf[Array[Int]]
    val width = ip.getWidth
    val height = ip.getHeight

    val grey = ip.convertToByteProcessor()
    grey.autoThreshold()
//    new ImagePlus("binary", grey).show()
    val binPix = grey.getPixels.asInstanceOf[Array[Byte]]
    val mask = scala.collection.mutable.Set[Int]()
    for {
      r <- 0 until height
      c <- 0 until width
      if binPix(calculateIndex(width, r, c)) == BLACK
    } mask.add(calculateIndex(width,r,c))
    println(s"total pixels in mask: ${mask.size}")

    for {
      r <- 0 until height
      c <- 0 until width
    } {
      val i = calculateIndex(width,r,c)
      if (!mask.contains(i)) workset(i) = 255.toByte
    }

  }
}
