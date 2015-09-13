package dermatological.binary_ops

import core._

import scala.collection.parallel.mutable.ParArray

/**
 * Provides methods for measuring the asymmetry feature descriptor of a skin lesion
 */
object Asymmetry {

  /**
   * Partially applied function; requires [[core.Image]] as input to complete
   * @return binary [[core.Image]] showing the asymmetrical parts of a skin lesion
   */
  def getOverlapImage = (im: Image[Byte]) =>
    AsymmTransform(im, AsymmTraverse(), AsymmOperation()) transform

  /**
    * Partially applied function; requires [[core.Image]] as input to complete
    * @return [[scala.Double]] of the asymmetry ratio of a skin lesion
    */
  def calculateAsymmetry = (im: Image[Byte]) => {
    val areaWhole = Moments.getCentralMoments(im)("area")
    val areaXOR = Moments.getCentralMoments(getOverlapImage(im))("area")
    areaXOR / areaWhole
  }
  
  private case class AsymmTransform(image: Image[Byte], traversal: AsymmTraverse, op: AsymmOperation) extends Transformation {
    
    def transform = {
      val resultArray = traversal traverse (image, Moments.getCentralMoments(image)("centroidY").round.toInt, op)
      ParImage(resultArray, image.width, image.height)
    }
  }

  private case class AsymmTraverse() extends Traversal {
    
    def traverse(im: Image[Byte], centroidY: Int, asymmOp: AsymmOperation): ParArray[Byte] = {
      def resolveIndex(row: Int, col: Int) = im.width * row + col
      
      val result = PointTraverse() traverse(im, PointOp_1Channel((_:Byte) => 255.toByte))
      for {
        r <- 0 until centroidY
        c <- 0 until im.width
        if im.matrix(resolveIndex(r,c)) == 0
      } {
        val distance = centroidY - r
        val bottomPoint = resolveIndex(centroidY + distance, c)
        result(bottomPoint) = asymmOp pointTest (im.matrix(resolveIndex(r,c)), im.matrix(bottomPoint))
      }
      result
    }
      
  }

  private case class AsymmOperation() extends Operation {
    def pointTest(pixelA: Byte, pixelB: Byte) =
      if (pixelA == pixelB) 255.toByte else 0.toByte
  }
}
