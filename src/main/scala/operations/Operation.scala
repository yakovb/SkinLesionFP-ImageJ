package operations

sealed trait Operation

sealed trait PointOperation[-A,+B] extends Operation {
  def runOn(pixel: A): B
}

case class PointOp[A,B](f: A => B) extends PointOperation[A,B] {
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

//TODO normalise based on kernel vals
case class LinearFilter[-A,K,+B](f: List[(A,K)] => B)(kernel: List[K]) extends NeighbourhoodOperation[A,B] {
  override def runOn(neighbourhood: List[A]): B =
    f (neighbourhood zip kernel)
}

case class NonLinearFilterNoKernel[A,B](f: List[A] => B) extends NeighbourhoodOperation[A,B] {
  override def runOn(neighbourhood: List[A]): B =
    f (neighbourhood)
}


object Operation {
  def convolve[N,A<:N,B<:N,C<:N,D<:N](source: List[A], sourceClean: A => C = _.asInstanceOf[C], kernel: List[B])
                                     (normalizer: B = 1)
                                     (converter: N => D)
                                     (implicit n: Numeric[N]): D = {
    import n._
    if (source.size != kernel.size) throw new Exception("kernel and source array must be the same size")
    else {
      val zipped = (source map sourceClean) zip kernel
      converter ((zipped.map(p => p._1 * p._2) sum) * normalizer)
    }
  }

}