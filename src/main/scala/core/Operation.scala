package core

/** Base trait encapsulating pixel operations */
trait Operation

/** Base trait encapsulating operations on a single pixel of A, returning a B */
trait PointOperation[-A,+B] extends Operation {

  /**
   * Abstract operation on a single pixel
   * @param pixel pixel of type A
   * @return pixel of type B
   */
  def runOn(pixel: A): B
}

/**
 * Point operation on a pixel that represents a single channel
 * @param f function taking a pixel of A to a pixel of B
 * @tparam A pixel of type A
 * @tparam B pixel of type B
 */
case class PointOp_1Channel[A,B](f: A => B) extends PointOperation[A,B] {

  /**
   * Takes pixel of A to pixel of B
   * @param pixel pixel of type A
   * @return pixel of type B
   */
  override def runOn(pixel: A): B = f(pixel)
}

/**
 * Point operation on a pixel that represents three colour channels packed into an [[scala.Int]].
 * @param redOp function processing red channel
 * @param greenOp function processing green channel
 * @param blueOp function processing blue channel
 * @param combine function combining the results of processing the colour channels
 * @tparam A intermediate result type of colour processing functions
 * @tparam B resulting pixel type
 */
case class PointOp_3Channel[A,B](redOp: Int => A, greenOp: Int => A, blueOp: Int => A)
                                (combine: (A,A,A) => B) extends PointOperation[Int,B] {

  /**
   * Takes packed [[scala.Int]] to a pixel of B
   * @param pixel pixel of type A
   * @return pixel of type B
   */
  override def runOn(pixel: Int): B = {
    val red = redOp ((pixel >> 16) & 0xff)
    val green = greenOp ((pixel >> 8) & 0xff)
    val blue = blueOp (pixel & 0xff)
    combine (red, green, blue)
  }
}

trait NeighbourhoodOperation[-A,+B] extends Operation {
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

case class NonLinearFilter[A,B](neighbourHoodSize: Int, f: List[A] => B) extends NeighbourhoodOperation[A,B] {
  require(neighbourHoodSize > 1, "neighbourhood size must be greater than one, otherwise use a point operation")

  override def runOn(neighbourhood: List[A]): B =
    f (neighbourhood)
}