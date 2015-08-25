package operations

import images.Image

import scala.collection.parallel.ParMap
import scala.collection.parallel.mutable.ParArray

sealed trait Traversal

case class PointTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], pointOp: PointOperation[A,B]): ParArray[B] =
    for (pixel <- im.matrix) yield pointOp runOn pixel

}

case class BlockTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], blockSize: Int)(blockOps: List[PointOperation[A,B]]): ParArray[B] = {

    (for (block <- im.matrix.toIterator grouped blockSize)
      yield (for (i <- 0 until blockSize) yield blockOps(i).runOn(block(i))).toParArray).toParArray.flatten
  }
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

case class Histo_1ChannelTraverse() extends Traversal {
  def traverse[A](im: Image[A]): ParMap[A,Int] =
    im.matrix groupBy (pixel => pixel) mapValues (_ size)

}

case class Histo_3ChannelTraverse() extends Traversal {

  def traverse(im: Image[Int]): Map[String, ParMap[Int, Int]] = {

    def oneChannelTraverse(getPixelOp: Int => Int) =
      im.matrix groupBy (getPixelOp(_))

    val redHisto = oneChannelTraverse (pixel => (pixel >> 16) & 0xff) mapValues (_ size)
    val greenHisto = oneChannelTraverse (pixel => (pixel >> 8) & 0xff) mapValues (_ size)
    val blueHisto = oneChannelTraverse (pixel => pixel & 0xff) mapValues (_ size)

    Map("Red" -> redHisto, "Blue" -> blueHisto, "Green" -> greenHisto)
  }


}