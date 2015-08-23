package operations

object Funcs {
  def rgb_2_grey: PointOpRGB[Double, Byte] =
    PointOpRGB[Double,Byte](_ * 0.2126, _ * 0.7152, _ * 0.0722) ((r,g,b) => (r+g+b).toByte)

  def id_filter: LinearFilter[Byte, Int, Byte] =
    LinearFilter[Byte,Int,Byte]((pairs: List[(Byte,Int)]) =>
      (pairs map (p => p._1 * p._2)).sum.toByte)(List(0,0,0,0,1,0,0,0,0))

  def simple_binary: PointOp[Byte, Byte] =
    PointOp[Byte,Byte]( (p:Byte) =>
      if (p < 80) 255.toByte
      else 0.toByte )

  def invert: PointOp[Byte, Byte] =
    PointOp[Byte,Byte]( (p:Byte) => (255 - p).toByte )

  def gaussBlur: LinearFilter[Byte,Int,Byte] =
    LinearFilter[Byte,Int,Byte]((pairs: List[(Byte,Int)]) =>
      ((pairs map (p => p._1 * p._2)).sum * (1/16)).toByte)(List(1,2,1,2,4,2,1,2,1)
    )
}