package plugins

import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.ImageProcessor
import images.ParImage
import operations.NeighbourhoodOp

class Unit_ extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G + DOES_STACKS + SUPPORTS_MASKING

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val image = ParImage[Byte](pixels.par, ip.getWidth, ip.getHeight)
    val transformedImage = image.traverseNeighbourhoods[Byte](
      NeighbourhoodOp[Byte,Int,Byte](
        (pairs: List[(Byte,Int)]) => ((pairs map (p => p._1 * p._2)).sum * (1/9)).toByte)
        (List.fill(9)(1)
      )
    )
  }
}
