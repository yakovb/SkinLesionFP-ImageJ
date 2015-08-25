package operations

object Funcs {
  def rgb_2_grey: PointOp_3Channel[Double, Byte] =
    PointOp_3Channel[Double,Byte](_ * 0.2126, _ * 0.7152, _ * 0.0722) ((r,g,b) => (r+g+b).toByte)

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

  def rgb2xyz: PointOp_3Channel[Double, Array[Double]] =
    PointOp_3Channel[Double,Array[Double]](
      (r:Int) => {
        val rr = r / 255
        if (rr > .04045) Math.pow((rr + .055) / 1.055, 2.4)
        else rr / 12.92},
      (g:Int) => {
        val gg = g / 255
        if (gg > .04045) Math.pow((gg + .055) / 1.055, 2.4)
        else gg / 12.92},
      (b:Int) => {
        val bb = b / 255
        if (bb > .04045) Math.pow((bb + .055) / 1.055, 2.4)
        else bb / 12.92})(Array(_,_,_))
}

object BorderAction extends Enumeration {
  type BorderAction = Value
  val NoAction = Value("Leave borders unchanged")
  val Crop = Value("Crop borders by 1 pixel")
}