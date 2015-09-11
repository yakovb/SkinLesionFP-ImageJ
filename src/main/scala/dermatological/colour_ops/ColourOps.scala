package dermatological.colour_ops

import core._

object ColourOps {

  def rgb_2_grey = {
    def toGreyOp =
      PointOp_3Channel(_ * 0.2126, _ * 0.7152, _ * 0.0722) ((r,g,b) => (r+g+b).toByte)

    TransformSimple(_: Image[Int], PointTraverse(), toGreyOp) transform
  }


  def rgb_to_xyz = {
    def xyzStep(pixel: Int) = {
      pixel / 255f match {
        case f if f > 0.04045 => (Math.pow((f + 0.055) / 1.055, 2.4) * 100).toFloat
        case f => f / (12.92 * 100).toFloat
      }
    }
    def xyzOp =
      PointOp_3Channel(xyzStep, xyzStep, xyzStep)( (r,g,b) => {
        val x = (r * 0.4124 + g * 0.3576 + b * 0.1805).toFloat
        val y = (r * 0.2126 + g * 0.7152 + b * 0.0722).toFloat
        val z = (r * 0.0193 + g * 0.1192 + b * 0.9505).toFloat
        Array(x,y,z)
      })
    TransformSimple(_: Image[Int], PointTraverse(), xyzOp) transform
  }


  def xyz_to_Lab = {
    val EPSILON = 0.008856
    val KAPPA = 909.3
    val Xn = 0.9642
    val Yn = 1.0
    val Zn = 0.8249

    def xRef(x: Float) = (x / Xn).toFloat
    def yRef(y: Float) = (y / Yn).toFloat
    def zRef(z: Float) = (z / Zn).toFloat

    def getLab(xyzArray: Array[Float]) = xyzArray match {
      case Array(x,y,z) =>
        val (xr, yr, zr) = (xRef(x), yRef(y), zRef(z))
        val fx = if (xr > EPSILON) Math.pow(xr, 1f/3f) else (KAPPA * xr + 16f) / 16f
        val fy = if (yr > EPSILON) Math.pow(yr, 1f/3f) else (KAPPA * yr + 16f) / 16f
        val fz = if (zr > EPSILON) Math.pow(zr, 1f/3f) else (KAPPA * zr + 16f) / 16f

        val L = (116f * fy - 16f).toFloat
        val a = (500f * (fx - fy)).toFloat
        val b = (200f * (fy - fz)).toFloat
        Array(L,a,b)
    }
    def labOp = PointOp_1Channel(getLab)
    TransformSimple(_: Image[Array[Float]], PointTraverse(), labOp) transform
  }


}
