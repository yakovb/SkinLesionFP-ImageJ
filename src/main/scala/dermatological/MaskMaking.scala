package dermatological

import core.{Image, MaskTraverse, TransformToMask}

import scala.collection.parallel.immutable.ParSet

object MaskMaking {
  val WHITE_INT = 16777215
  val BLACK_BYTE = 0.toByte

  def maskSetBinary: (Image[Byte]) => ParSet[(Int, Int)] = (im: Image[Byte]) =>
    TransformToMask(im, MaskTraverse(), (_: Byte)  == BLACK_BYTE) transform

  def maskSetColour: (Image[Int]) => ParSet[(Int, Int)] = (im: Image[Int]) =>
    TransformToMask(im, MaskTraverse(), (_: Int) != WHITE_INT) transform
  
  def maskArrayColour = (im: Image[Int]) => {
    val maskSet = maskSetColour(im)
    val maskPic = for {
      r <- 0 until im.height
      c <- 0 until im.width
      if maskSet.contains((r,c))
    } yield im.matrix(im.width * r + c)
    maskPic.toParArray
  } 
}
