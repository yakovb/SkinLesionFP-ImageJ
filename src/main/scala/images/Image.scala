package images

import scala.collection.parallel.mutable.ParArray

trait Image[A] {
  val width: Int
  val height: Int
  val matrix: ParArray[A]
}

case class ParImage[A](pixels: ParArray[A], w: Int, h: Int) extends Image[A] {
  require(pixels.nonEmpty, "cannot pass empty pixel array")
  require(w > 0, "cannot have image with no width")
  require(h > 0, "cannot have image with no height")
  require(w * h == pixels.length, s"check arguments: width * height should equal pixel array length, but you have $w * $h = ${pixels.length}")

  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[A] = pixels
}

case class Kernel(pixels: List[Float], w: Int, h: Int) extends Image[Float] {
  require(pixels.nonEmpty, "cannot pass empty pixel array")
  require(w > 1, "kernel width must be greater than 1, otherwise use a point operation")
  require(h > 1, "kernel height must be greater than 1, otherwise use a point operation")
  require(w * h == pixels.length, s"check arguments: width * height should equal pixel array length, but you have $w * $h = ${pixels.length}")

  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[Float] = pixels.toParArray
}