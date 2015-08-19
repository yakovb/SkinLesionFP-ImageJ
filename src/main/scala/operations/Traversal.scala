package operations

import images.Image

import scala.collection.parallel.mutable.ParArray

sealed trait Traversal

case class PointTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], pointOp: PointOperation[A,B]): ParArray[B] =
    for (pixel <- im.matrix) yield pointOp runOn pixel
}
