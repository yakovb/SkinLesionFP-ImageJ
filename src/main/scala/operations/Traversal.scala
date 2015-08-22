package operations

import images.{ParImage, Image}

import scala.collection.immutable
import scala.collection.parallel.mutable.ParArray

sealed trait Traversal

case class PointTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], pointOp: PointOperation[A,B]): ParArray[B] =
    for (pixel <- im.matrix) yield pointOp runOn pixel
}

case class NeighbourTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], op: NeighbourhoodOperation[A,B]): Image[B] = {

    val allNeighbourhoods: immutable.IndexedSeq[immutable.IndexedSeq[A]] =
      for {
        row <- 1 until (im.height-1)
        col <- 1 until (im.width-1)
      } yield
      for {
        u <- -1 to 1
        v <- -1 to 1
      } yield
      im.matrix(im.width * (row + u) + (col + v))

    ParImage((allNeighbourhoods map (n => op runOn n.toList)).toParArray, im.width-1, im.height-1)
  }

}
