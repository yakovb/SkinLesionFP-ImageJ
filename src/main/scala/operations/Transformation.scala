package operations

import images.{Image, ParImage}
import operations.BorderAction.BorderAction
import scala.collection.parallel.ParMap

sealed trait Transformation

case class TransformSimple[A,B](image: Image[A],
                                traversal: PointTraverse,
                                pointOp: PointOperation[A,B]) extends Transformation {

  def transform: ParImage[B] = {
    val newMat = traversal traverse (image, pointOp)
    ParImage(newMat, image.width, image.height)
  }
}

//TODO handle border cropping based on kernel size
case class TransformNeighbourhood[A,B](image: Image[A],
                                        traversal: NeighbourTraverse,
                                        neighbourOp: NeighbourhoodOperation[A,B])
                                      (borderAction: BorderAction) extends Transformation {

  def transform: Image[B] = {
    val newMat = traversal traverse (image, neighbourOp)

    borderAction match {
      case BorderAction.NoAction => ParImage(newMat, image.width, image.height)
      case BorderAction.Crop => ParImage(newMat, image.width - 2, image.height - 2)
      case _ => throw new Exception("Unknown border action: " + borderAction.toString)
    }
  }

}

case class TransformToHistogram[A](image: Image[A],
                                    traversal: Histo_1ChannelTraverse) extends Transformation {

  def transform: ParMap[A, Int] =
    traversal traverse image
}