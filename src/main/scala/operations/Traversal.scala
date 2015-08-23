package operations

import images.{Image, ParImage}

import scala.collection.immutable

sealed trait Traversal

case class PointTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], pointOp: PointOperation[A,B]): Image[B] = {
    val newMat = for (pixel <- im.matrix) yield pointOp runOn pixel
    ParImage(newMat, im.width, im.height)
  }
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
