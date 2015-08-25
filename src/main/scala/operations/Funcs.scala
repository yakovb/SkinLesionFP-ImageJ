package operations

object Funcs {

  def byte_2_float(b: Byte) = (b & 0xff).toFloat

  def rgb_2_grey: PointOp_3Channel[Double, Byte] =
    PointOp_3Channel[Double,Byte](_ * 0.2126, _ * 0.7152, _ * 0.0722) ((r,g,b) => (r+g+b).toByte)

  def id_filter  =
    LinearFilter[Byte,Byte](byte_2_float, List(0,0,0,0,1,0,0,0,0), _.toByte)

  def simple_binary: PointOp[Byte, Byte] =
    PointOp[Byte,Byte]( (p:Byte) =>
      if (p < 80) 255.toByte
      else 0.toByte )

  def invert: PointOp[Byte, Byte] =
    PointOp[Byte,Byte]( (p:Byte) => (255 - p).toByte )

  def gaussBlur =
    LinearFilter[Byte,Byte](byte_2_float, List(1,2,1,2,4,2,1,2,1), _.toByte, 1f/16)

  def medianFilter: NonLinearFilterNoKernel[Byte,Byte] =
    NonLinearFilterNoKernel[Byte,Byte](region => {
      val sorted = region.sorted
      val l = sorted.length
      if (l % 2 == 0) ((sorted(l/2) + sorted(l/2 + 1)) / 2).toByte
      else sorted(l / 2)
    })

  def rgb2xyz_Op(in: Int) = {
        val temp = in / 255f
        if (temp > .04045) Math.pow((temp + .055) / 1.055, 2.4).toFloat
        else temp / 12.92f}

  def rgb2xyz_Combiner: PointOp_3Channel[Float, Array[Float]] = {
    PointOp_3Channel[Float, Array[Float]](rgb2xyz_Op, rgb2xyz_Op, rgb2xyz_Op)((r,g,b) => {
      val rr = r * 100
      val gg = g * 100
      val bb = b * 100

      val x = rr * .4124f + gg * .3576f + bb * 1805f
      val y = rr * .2126f + gg * .7152f + bb * 0722f
      val z = rr * .0193f + gg * .1192f + bb * 9505f

      Array(x,y,z)
    }

    )
  }

}

object BorderAction extends Enumeration {
  type BorderAction = Value
  val NoAction = Value("Leave borders unchanged")
  val Crop = Value("Crop borders by 1 pixel")
}