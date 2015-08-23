package operations

import images.{Image, ParImage}

sealed trait Transformation

case class TransformSimple[A,B](image: Image[A],
                                traversal: PointTraverse,
                                pointOp: PointOperation[A,B]) extends Transformation {

  def transform: ParImage[B] = {
    val newMat = traversal traverse (image, pointOp)
    ParImage(newMat, image.width, image.height)
  }
}

case class TransformNeighbourhood[A,B](image: Image[A],
                                        traversal: NeighbourTraverse,
                                        neighbourOp: NeighbourhoodOperation[A,B]) extends Transformation {

  def transform: Image[B] = {
    val newMat = traversal traverse (image, neighbourOp)
    ParImage(newMat, image.width, image.height)
  }

}