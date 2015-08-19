package operations

import images.{ParImage, Image}

import scala.collection.parallel.mutable.ParArray

sealed trait Transformation

case class TransformSimple[A,B](image: Image[A],
                                traversal: PointTraverse,
                                pointOp: PointOperation[A,B]) extends Transformation {

  val newMat: ParArray[B] = traversal traverse (image, pointOp)
  ParImage(newMat, image.width, image.height)
}