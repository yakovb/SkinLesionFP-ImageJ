package operations

import images.Image

import scala.collection.parallel.mutable.ParArray

sealed trait Traversal

case class PointTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], pointOp: PointOperation[A,B]): ParArray[B] =
    for (pixel <- im.matrix) yield pointOp runOn pixel

}

//TODO handle traversal cropping based on kernel size
case class NeighbourTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], op: NeighbourhoodOperation[A,B]): ParArray[B] = {

    val allNeighbourhoods =
      for {
        row <- 1 until (im.height-1)
        col <- 1 until (im.width-1)
      } yield

        (for {
          u <- -1 to 1
          v <- -1 to 1
        } yield
          im.matrix(im.width * (row + u) + (col + v))).toList

    val newMat = allNeighbourhoods map ((n: List[A]) => op runOn n)
    newMat.toParArray
  }

}
