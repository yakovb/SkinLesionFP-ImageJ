package operations

sealed trait Operation

sealed trait PointOperation extends Operation {
  def run(pixel: Int): Int
}

case class PointOp(f: Int => Int) extends PointOperation {
  override def run(pixel: Int): Int = f(pixel)
}

case class PointOpRGB(redOp: Int => Int, greenOp: Int => Int, blueOp: Int => Int) extends PointOperation {
  override def run(pixel: Int): Int = {
    val red = redOp ((pixel >> 16) & 0xff)
    val green = greenOp ((pixel >> 8) & 0xff)
    val blue = blueOp (pixel & 0xff)
    (red << 16) + (green << 8) + blue
  }
}