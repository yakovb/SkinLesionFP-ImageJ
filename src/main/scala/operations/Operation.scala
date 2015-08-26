package operations

sealed trait Operation

sealed trait PointOperation[-A,+B] extends Operation {
  def runOn(pixel: A): B
}

case class PointOp_1Channel[A,B](f: A => B) extends PointOperation[A,B] {
  override def runOn(pixel: A): B = f(pixel)
}

case class PointOp_3Channel[A,B](redOp: Int => A, greenOp: Int => A, blueOp: Int => A)
                        (combine: (A,A,A) => B) extends PointOperation[Int,B] {
  override def runOn(pixel: Int): B = {
    val red = redOp ((pixel >> 16) & 0xff)
    val green = greenOp ((pixel >> 8) & 0xff)
    val blue = blueOp (pixel & 0xff)
    combine (red, green, blue)
  }
}

sealed trait NeighbourhoodOperation[-A,+B] extends Operation {
  def runOn(neighbourhood: List[A]): B
}

case class LinearFilter[A,B](neighbourhoodConvert: A => Float,
                             kernel: List[Float], 
                             resultConvert: Float => B,
                             normalizer: Float = 1.0f) extends NeighbourhoodOperation[A,B] {

  override def runOn(neighbourhood: List[A]): B = {
    if (neighbourhood.size != kernel.size) throw new Exception("kernel and source array must be the same size")
    else {
      val zipped = (neighbourhood map neighbourhoodConvert) zip kernel
      resultConvert ((zipped map (p => p._1 * p._2) sum) * normalizer)
    }
  }
}

case class NonLinearFilterNoKernel[A,B](f: List[A] => B) extends NeighbourhoodOperation[A,B] {
  override def runOn(neighbourhood: List[A]): B =
    f (neighbourhood)
}