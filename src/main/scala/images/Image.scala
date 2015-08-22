package images

import scala.collection.parallel.mutable.ParArray

sealed trait Image[A] {
  val width: Int
  val height: Int
  val matrix: ParArray[A]
}

case class ParImage[A](pixels: ParArray[A], w: Int, h: Int) extends Image[A] {
  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[A] = pixels
}