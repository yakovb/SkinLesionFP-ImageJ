import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

class Perimeter_Test extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val w = ip.getWidth
    val h = ip.getHeight
    def calculateIndex(row: Int, col: Int) = w * row + col

    val WHITE = 255.toByte
    val BLACK = 0.toByte
    val TEMPCOLOUR = 100.toByte

    val contour = scala.collection.mutable.Stack[Float]()
    val us, vs = List(-1,0,1)

    for {
      r <- 1 until h-1
      c <- 1 until w-1
    } {
      if (pixels(calculateIndex(r,c)) == BLACK) {
        for {
          v <- vs
          u <- us
        } {
          val i = calculateIndex(r,c)
          if (pixels(calculateIndex(r+v, c+u)) == WHITE) {
            pixels(i) = TEMPCOLOUR
            contour.push(i)
          }
        }
      }
    }
  }

}