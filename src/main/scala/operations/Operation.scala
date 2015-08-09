package operations

sealed trait Operation {
  def run(pixel: Int): Int
}

case class PointOp(f: Int => Int) extends Operation {
  override def run(pixel: Int): Int = f(pixel)
}