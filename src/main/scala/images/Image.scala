package images

import operations.{NeighbourhoodOperation, PointOperation}

import scala.collection.immutable
import scala.collection.parallel.mutable.ParArray

sealed trait Image[A] {
  val width: Int
  val height: Int
  val matrix: ParArray[A]

  def traversePoints[B](op: PointOperation[A,B]): Image[B] = {
    val newMatrix = for (pixel <- matrix)
      yield op runOn pixel
    
    ParImage(newMatrix, width, height)
  }

  def traverseNeighbourhoods[B](op: NeighbourhoodOperation[A,B]): Image[B] = {
    val allNeighbourhoods: immutable.IndexedSeq[immutable.IndexedSeq[A]] =
      for {
      row <- 1 until width
      col <- 1 until height
    } yield
      for {
        u <- -1 to 1
        v <- -1 to 1
      } yield matrix(width * (row + u) + (col + v))

    ParImage((allNeighbourhoods map (n => op runOn n.toList)).toParArray, width-1, height-1)
  }
}

case class ParImage[A](pixels: ParArray[A], w: Int, h: Int) extends Image[A] {
  override val width: Int = w
  override val height: Int = h
  override val matrix: ParArray[A] = pixels
}