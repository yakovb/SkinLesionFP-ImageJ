import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.{ByteProcessor, ImageProcessor}
import spire.algebra.Ring
import spire.std.any._

import scala.collection.parallel.mutable.ParArray

class Map_2 extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_RGB + DOES_8G + NO_CHANGES

  override def run(ip: ImageProcessor): Unit = {
    val pixelsIn = ip.getPixels.asInstanceOf[Array[Int]].par
    val pixelsOut = map2(pixelsIn)(convert)(invert)
    val greyProcessor = new ByteProcessor(ip.getWidth, ip.getHeight, pixelsOut.toArray)
    new ImagePlus("grey pic", greyProcessor) show
  }

  private def map2[@specialized(Byte,Int) A: Ring, B: Ring, C: Ring]
    (input: ParArray[A])
    (f: A => B)
    (g: B => C): ParArray[C] = input map (p => g(f(p)))

  private def invert(in: Byte): Byte =
    (255 - in).toByte

  private def convert(in: Int): Byte = {
    val red = ((in >> 16) & 255) * 0.2126
    val green = ((in >> 8) & 255) * 0.7152
    val blue = (in & 255) * 0.0722
    (red + green + blue).toByte
  }

}
