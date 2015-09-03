package dermatological.binary_ops

import images.Image
import operations.{Transformation, Traversal}

case class Moments(im: Image[Byte]) {

  val m00 = (r: Int, c: Int) => 1
  val m10 = (r: Int, c: Int) => c
  val m01 = (r: Int, c: Int) => r
  val m11 = (r: Int, c: Int) => r * c
  val m20 = (r: Int, c: Int) => c * c
  val m02 = (r: Int, c: Int) => r * r
  val mFuncList = List(m00,m10,m01,m11,m20,m02)
  val mNames = List("m00", "m10", "m01", "m11", "m20", "m02")

  def getCentralMoments =
    MomentTransform(im, MomentTraverse()) transform
}


case class MomentTraverse() extends Traversal {

  def traverse(im: Image[Byte], momentFuncs: List[(Int,Int) => Int], momentNames: List[String]) = {
    val rawData = loop(im, momentFuncs)
    val aggrData = for (i <- momentNames.indices) yield rawData map (lst => lst(i)) sum
    
    momentNames zip aggrData toMap
  }
  
  def centralMoments(im: Image[Byte], centroidX: Double, centriodY: Double,
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


case class MomentTransform(im: Image[Byte], mTraverse: MomentTraverse) extends Transformation {
  
  def transform = {
    val moments = mTraverse traverse (im, mFuncList, mNames)
    val centX = moments("m10").toFloat / moments("m00").toDouble
    val centY = moments("m01").toFloat / moments("m00").toDouble

    println(s"cx: $centX, cy: $centY")
    val c10 = (r: Int, c: Int) => c - centX
    val c01 = (r: Int, c: Int) => r - centY
    val c11 = (r: Int, c: Int) => (c - centX) * (r - centY)
    val c20 = (r: Int, c: Int) => Math.pow(c - centX, 2)
    val c02 = (r: Int, c: Int) => Math.pow(r - centY, 2)
    val centMomList = List(c10, c01, c11, c20, c02)
    val centMomNames = List("c10", "c01", "c11", "c20", "c02")

    mTraverse centralMoments (im, centX, centY, centMomList, centMomNames)
  }
}