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

  def rgb2xyz_Op: PointOp[Int, Double] =
    PointOp[Int, Double]((pixel: Int) => {
        val temp = pixel / 255
        if (temp > .04045) Math.pow((temp + .055) / 1.055, 2.4)
        else temp / 12.92})

//  def rgb2xyz_Combiner(x: Int, y: Int, z: Int) =

}

object BorderAction extends Enumeration {
  type BorderAction = Value
  val NoAction = Value("Leave borders unchanged")
  val Crop = Value("Crop borders by 1 pixel")
}