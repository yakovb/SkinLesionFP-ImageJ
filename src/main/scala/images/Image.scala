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
  def this(pixels: Array[Int], w: Int, h: Int) = this(pixels.par, w, h)

  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[Int] = pixels
}