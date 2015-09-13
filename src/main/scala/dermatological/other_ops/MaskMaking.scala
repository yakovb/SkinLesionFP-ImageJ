package dermatological.other_ops

import core.{Image, MaskTraverse, TransformToMask}

import scala.collection.parallel.immutable.ParSet
import scala.collection.parallel.mutable.ParArray

/**
 * Provides methods for creating masks out of binary or colour images
 */
object MaskMaking {

  private val WHITE_INT = 16777215
  private   val BLACK_BYTE = 0.toByte

  /**
   * Partially applied function; requires binary [[core.Image]] as input to complete
   * @return mask containing (row,column) tuples of pixels in the mask
   */
  def maskSetBinary: (Image[Byte]) => ParSet[(Int, Int)] = (im: Image[Byte]) =>
    TransformToMask(im, MaskTraverse(), (_: Byte)  == BLACK_BYTE) transform

  /**
   * Partially applied function; requires colour [[core.Image]] as input to complete
   * @return array of pixels with each pixel not in the mask showing as WHITE
   */
  def maskArrayColour: (Image[Int]) => ParArray[Int] = (im: Image[Int]) => {
    val maskSet = maskSetColour(im)
    val maskPic = for {
        r <- 0 until im.height
        c <- 0 until im.width
      if maskSet.contains((r,c))
    } yield im.matrix(im.width * r + c)
        maskPic.toParArray
  }

  private def maskSetColour: (Image[Int]) => ParSet[(Int, Int)] = (im: Image[Int]) =>
    TransformToMask(im, MaskTraverse(), (_: Int) != WHITE_INT) transform
}
