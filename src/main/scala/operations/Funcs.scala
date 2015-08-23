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
      ((pairs map (p => (p._1 & 0xff) * p._2)).sum * (1.0 / 16)).toByte) (List(1,2,1,2,4,2,1,2,1))

  def medianFilter: NonLinearFilterNoKernel[Byte,Byte] =
    NonLinearFilterNoKernel[Byte,Byte](region => {
      val sorted = region.sorted
      val l = sorted.length
      if (l % 2 == 0) ((sorted(l/2) + sorted(l/2 + 1)) / 2).toByte
      else sorted(l / 2)
    })
}

object BorderAction extends Enumeration {
  type BorderAction = Value
  val NoAction = Value("Leave borders unchanged")
  val Crop = Value("Crop borders by 1 pixel")
}