import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import ij.{IJ, ImagePlus}

import spire.algebra.Ring
import spire.syntax.ring._
import spire.std.any._

import scala.collection.parallel.mutable.ParArray

class Inverter_ extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    if (arg == "about") {showAbout(); DONE}
    else DOES_8G + DOES_STACKS + SUPPORTS_MASKING + NO_CHANGES

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]].par
    ip.setPixels(invert(pixels).toArray)
  }

  private def showAbout(): Unit =
    IJ.showMessage("this is my scala-based inverter")

  private def invert[@specialized(Byte,Int) A: Ring](ar: ParArray[A]): ParArray[A] = {
    ar map (255 - _)
  }
}
