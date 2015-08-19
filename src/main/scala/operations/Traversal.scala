package operations

import images.Image

import scala.collection.parallel.mutable.ParArray

sealed trait Traversal

case class PointTraverse[A,B](im: Image[A], pointOp: PointOperation[A,B]) extends Traversal {
  def traverse: ParArray[B] =
    for (pixel <- im.matrix) yield pointOp runOn pixel
}
