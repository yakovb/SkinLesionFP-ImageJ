package dermatological

import dermatological.binary_ops.Moments
import images.{Image, ParImage}
import operations._

import scala.collection.parallel.mutable.ParArray

object Asymmetry {
  
  def getOverlapImage = (im: Image[Byte]) =>
    AsymmTransform(im, AsymmTraverse(), AsymmOperation()) transform

  def calculateAsymmetry = (im: Image[Byte]) => {
    val areaWhole = Moments.getCentralMoments(im)("area")
    val areaXOR = Moments.getCentralMoments(getOverlapImage(im))("area")
    areaXOR / areaWhole
  }
  
  case class AsymmTransform(image: Image[Byte], traversal: AsymmTraverse, op: AsymmOperation) extends Transformation {
    
    def transform = {
      val resultArray = traversal traverse (image, Moments.getCentralMoments(image)("centroidY").round.toInt, op)
      ParImage(resultArray, image.width, image.height)
    }
  }

  case class AsymmTraverse() extends Traversal {
    
    def traverse(im: Image[Byte], centroidY: Int, asymmOp: AsymmOperation): ParArray[Byte] = {
      def resolveIndex(row: Int, col: Int) = im.width * row + col
      
      val result = PointTraverse() traverse(im, PointOp_1Channel((_:Byte) => 255.toByte))
      for {
        r <- 0 to centroidY
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

  case class AsymmOperation() extends Operation {
    def pointTest(pixelA: Byte, pixelB: Byte) =
      if (pixelA == pixelB) 255.toByte else 0.toByte
  }
}
