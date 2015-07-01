import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.{ByteProcessor, ImageProcessor}

import scala.collection.parallel.mutable.ParArray

class RGB_to_Grey extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_RGB + DOES_STACKS + SUPPORTS_MASKING

  override def run(ip: ImageProcessor): Unit = {
    val w = ip.getWidth
    val h = ip.getHeight

    val pixels = ip.getPixels.asInstanceOf[Array[Int]].par
    val greyProcessor = new ByteProcessor(w, h, convert(pixels).toArray)
    new ImagePlus("grey pic", greyProcessor) show
  }

  private def convert(ar: ParArray[Int]): ParArray[Byte] = {
    ar map (p => {
      val red = ((p >> 16) & 255) * 0.2126
      val green = ((p >> 8) & 255) * 0.7152
      val blue = (p & 255) * 0.0722
      (red + green + blue).toByte
    })
  }
}
