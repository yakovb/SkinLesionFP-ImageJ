package dermatological

import core.{Image, ParImage, Transformation, Traversal}

import scala.collection.parallel.immutable.ParSet

object LesionMask {
  val WHITE = 16777215

  def maskColourImage = (im: Image[Int], mask: ParSet[(Int,Int)]) =>
    MaskTransform(im, OverlayMaskTraverse(), mask) transform

  case class MaskTransform(im: Image[Int], traversal: OverlayMaskTraverse,  mask: ParSet[(Int,Int)]) extends Transformation {
    def transform = {
      val resultMatrix = traversal traverse (im, mask)
      ParImage(resultMatrix, im.width, im.height)
    }
  }

  case class OverlayMaskTraverse() extends Traversal {

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
