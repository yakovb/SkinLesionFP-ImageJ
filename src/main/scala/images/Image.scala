package images

import operations.PointOperation

import scala.collection.parallel.mutable.ParArray

sealed trait Image[A] {
  val width: Int
  val height: Int
  val matrix: ParArray[A]

  def traversePoints[B](op: PointOperation[A,B]): Image[B] = {
    val newMatrix = for (pixel <- matrix)
      yield op runOn pixel
    
    ParImage(newMatrix, width, height)
  }
}

case class ParImage[A](pixels: ParArray[A], w: Int, h: Int) extends Image[A] {
  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[A] = pixels
}