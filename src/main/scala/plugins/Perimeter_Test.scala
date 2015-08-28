import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor

import scala.collection.mutable

class Perimeter_Test extends PlugInFilter {
  val WHITE = 255.toByte
  val BLACK = 0.toByte
  val TEMPCOLOUR = 100.toByte
  val VISITED = 50.toByte
  def calculateIndex(w: Int, row: Int, col: Int) = w * row + col
  def calculateRowCol(h: Int, i: Int) = (i/h, i % h)

  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    getAndMarkContour(ip)
  }

  def getAndMarkContour(ip: ImageProcessor): mutable.Set[Int] = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val w = ip.getWidth
    val h = ip.getHeight

    val contour = scala.collection.mutable.Set[Int]()
    val us, vs = List(-1,0,1)

    for {
      r <- 1 until h-1
      c <- 1 until w-1
    } {
      if (pixels(calculateIndex(w, r,c)) == BLACK) {
        for {
          v <- vs
          u <- us
        } {
          val i = calculateIndex(w, r,c)
          if (pixels(calculateIndex(w, r+v, c+u)) == WHITE) {
            pixels(i) = TEMPCOLOUR
            contour.add(i)
          }
        }
      }
    }

    val area = {
      for {
        r <- 0 until h
        c <- 0 until w
        if pixels(calculateIndex(w,r,c)) == BLACK || pixels(calculateIndex(w,r,c)) == TEMPCOLOUR
      } yield true
    }.size

    val size = contour.size
    val perim = size * .95
    val circularity = 4 * Math.PI * area / Math.pow(perim, 2)
    println(s"length of contour is $size and therefore perimeter measure is 0.95 * that, i.e. $perim")
    println(s"circularity measure is 4pi * area / perimeter squared, i.e. $circularity")
    contour
  }


}