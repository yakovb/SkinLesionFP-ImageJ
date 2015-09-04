package dermatological

import images.Image
import operations.{NeighbourTraverse, NonLinearFilterNoKernel, TransformNeighbourhood}

object Perimeter {
  val WHITE = 255.toByte
  val BLACK = 0.toByte
  val TEMPCOLOUR = 100.toByte

  def markPerimeter = (im: Image[Byte]) =>
      TransformNeighbourhood(im, NeighbourTraverse(), markPerimeterOp) transform

  def countPerimeter = (im: Image[Byte]) =>
    TransformNeighbourhood(im, NeighbourTraverse(), countPerimeterOp) transform

  def markPerimeterOp =
    NonLinearFilterNoKernel(3, markBorderPixFunction)

  def countPerimeterOp =
    NonLinearFilterNoKernel(3, isBorderPixFunction)

  def markBorderPixFunction = (nhood: List[Byte]) =>
    if (nhood(4) == BLACK && nhood.contains(WHITE)) TEMPCOLOUR else nhood(4)

  def isBorderPixFunction = (nhood: List[Byte]) =>
    nhood(4) == BLACK && nhood.contains(WHITE)
}
