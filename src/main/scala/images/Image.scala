package images

import operations.PointOperation

import scala.collection.parallel.mutable.ParArray

sealed trait Image {
  val width: Int
  val height: Int
  val matrix: ParArray[Int]

  def processPoints(op: PointOperation): Image = {
    val newMatrix = for (pixel <- matrix)
      yield op runOn pixel
    ImageInt(newMatrix, width, height)
  }
}

case class ImageInt(pixels: ParArray[Int], w: Int, h: Int) extends Image {
  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[Int] = pixels
}

object Image {
  def makeImageInt(pixels: ParArray[Int], width: Int, height: Int) =
    ImageInt(pixels, width, height)

  def makeImageInt(pixels: Array[Int], width: Int, height: Int) =
    ImageInt(pixels.par, width, height)
}