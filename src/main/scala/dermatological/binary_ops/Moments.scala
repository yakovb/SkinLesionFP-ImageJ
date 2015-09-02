package dermatological.binary_ops

import images.Image
import operations.{Transformation, Traversal}

object Moments {

  val m00 = (r: Int, c: Int) => 1
  val m10 = (r: Int, c: Int) => c
  val m01 = (r: Int, c: Int) => r
  val m11 = (r: Int, c: Int) => r * c
  val m20 = (r: Int, c: Int) => c * c
  val m02 = (r: Int, c: Int) => r * r
  val mFuncList = List(m00,m10,m01,m11,m20,m02)
  val mNames = List("m00", "m10", "m01", "m11", "m20", "m02")

  def computeMoments(image: Image[Byte]) = MomentTraverse() traverse (image, mFuncList, mNames)
}


case class MomentTraverse() extends Traversal {

  def traverse(im: Image[Byte], momentFuncs: List[(Int,Int) => Int], momentNames: List[String]) = {
    val rawData = loop(im, momentFuncs)
    val aggrData = for (i <- momentNames.indices) yield rawData map (lst => lst(i)) sum
    
    momentNames zip aggrData toMap
  }
  
  def centralMoments(im: Image[Byte], centroidX: Int, centriodY: Int, 
                     centralFuncs: List[(Int,Int) => Double], centralNames: List[String]) = {
    val rawData = loop(im, centralFuncs)
    val aggrData = for (i <- centralNames.indices) yield rawData map (lst => lst(i)) sum
    
    centralNames zip aggrData toMap
  }
  
  private def loop[A](im: Image[Byte], funcs: List[(Int,Int) => A]) =
    for {
      r <- 0 until im.height
      c <- 0 until im.width
      if im.matrix(im.width * r + c) == 0
    } yield funcs map (f => f(r,c))

}


case class MomentTransform() extends Transformation {
  import Moments._
  
  def transform(im: Image[Byte], mTraverse: MomentTraverse) = {
    val moments = mTraverse traverse (im, mFuncList, mNames)
    val centX = (moments("m10").toFloat / moments("m00").toFloat).toInt
    val centY = (moments("m01").toFloat / moments("m00").toFloat).toInt

    val c10 = (r: Int, c: Int) => (c - centX).toDouble
    val c01 = (r: Int, c: Int) => (r - centY).toDouble
    val c11 = (r: Int, c: Int) => (c - centX).toDouble * (r - centY).toDouble
    val c20 = (r: Int, c: Int) => Math.pow(c - centX, 2)
    val c02 = (r: Int, c: Int) => Math.pow(r - centY, 2)
    val centMomList = List(c10, c01, c11, c20, c02)
    val centMomNames = List("c10", "c01", "c11", "c20", "c02")

    mTraverse centralMoments (im, centX, centY, centMomList, centMomNames)
  }
}