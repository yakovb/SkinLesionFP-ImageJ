package dermatological.other_ops

import core.{Image, ParImage, Transformation, Traversal}

import scala.collection.parallel.immutable.ParSet

/**
 * Provides method for creating masked colour images
 */
object LesionMask {

  private val WHITE = 16777215

  /**
   * Partially applied function; requires colour [[core.Image]] and mask
   * @return masked [[core.Image]] with [[scala.Int]] pixel array with all pixels not in the mask set to WHITE
   */
  def maskColourImage = (im: Image[Int], mask: ParSet[(Int,Int)]) =>
    MaskTransform(im, OverlayMaskTraverse(), mask) transform

  private case class MaskTransform(im: Image[Int], traversal: OverlayMaskTraverse,  mask: ParSet[(Int,Int)]) extends Transformation {
    def transform = {
      val resultMatrix = traversal traverse (im, mask)
      ParImage(resultMatrix, im.width, im.height)
    }
  }

  private case class OverlayMaskTraverse() extends Traversal {

    def traverse(im: Image[Int], mask: ParSet[(Int,Int)]) = {
      val result = for {
        r <- 0 until im.height
        c <- 0 until im.width
      }
        yield if (!mask.contains((r, c))) WHITE else im.matrix(im.width * r + c)
      result.toParArray
    }
  }
}
