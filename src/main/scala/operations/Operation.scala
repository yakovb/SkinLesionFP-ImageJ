package operations

import images.Kernel

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

case class LinearFilter[A,B](kernel: Kernel,
                             neighbourhoodConvert: A => Float,
                             resultConvert: Float => B,
                             normalizer: Float = 1.0f) extends NeighbourhoodOperation[A,B] {

  override def runOn(neighbourhood: List[A]): B = {
    if (neighbourhood.size != (kernel.width * kernel.height)) throw new Exception("kernel and neighbourhood must be the same size")
    else {
      val zipped = (neighbourhood map neighbourhoodConvert) zip kernel.matrix
      resultConvert ((zipped map (p => p._1 * p._2) sum) * normalizer)
    }
  }
}

case class NonLinearFilterNoKernel[A,B](f: List[A] => B, neighbourHoodSize: Int) extends NeighbourhoodOperation[A,B] {
  override def runOn(neighbourhood: List[A]): B =
    f (neighbourhood)
}