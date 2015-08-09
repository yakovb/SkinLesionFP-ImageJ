package images

import operations.PointOperation

import scala.collection.parallel.mutable.ParArray

sealed trait Image {
  val width: Int
  val height: Int
  val matrix: ParArray[Int]

  def processPoints(op: PointOperation)(output: String): Image = {
    val newMatrix = for (pixel <- matrix)
      yield op runOn pixel
    output match {
      case "RGB" => ImageRGB(newMatrix, width, height)
      case "Grey" => ImageGrey(newMatrix, width, height)
      case "Binary" => ImageBinary(newMatrix, width, height)
    }
  }
}

case class ImageRGB(pixels: ParArray[Int], w: Int, h: Int) extends Image {
  def this(pixels: Array[Int], w: Int, h: Int) = this(pixels.par, w, h)

  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[Int] = pixels
}

case class ImageGrey(pixels: ParArray[Int], w: Int, h: Int) extends Image {
  def this(pixels: Array[Int], w: Int, h: Int) = this(pixels.par, w, h)

  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[Int] = pixels.par
}

case class ImageBinary(pixels: ParArray[Int], w: Int, h: Int) extends Image {
  def this(pixels: Array[Int], w: Int, h: Int) = this(pixels.par, w, h)

  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[Int] = pixels.par
}