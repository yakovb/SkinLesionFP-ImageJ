package operations

import images.Image

import scala.collection.parallel.ParMap
import scala.collection.parallel.immutable.ParSet
import scala.collection.parallel.mutable.ParArray
import scala.reflect.ClassTag

trait Traversal

case class MaskTraverse() extends Traversal {

  def traverse[A](im: Image[A], predicate: A => Boolean): ParSet[A] = {
    val resultArray = for (pixel <- im.matrix if predicate(pixel)) yield pixel
    resultArray.toSet
  }
}

case class PointTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], pointOp: PointOperation[A,B]): ParArray[B] =
    for (pixel <- im.matrix) yield pointOp runOn pixel

  def traverseAndExpand[A,B](im: Image[A], expansionOps: List[PointOperation[A,B]]): ParArray[B] = {

    def builder(element: A, ops: List[PointOperation[A,B]]): List[B] = ops match {
      case Nil => Nil
      case op::rest => (op runOn element) :: builder(element, rest)
    }
    (for (pixel <- im.matrix) yield builder(pixel, expansionOps)).flatten
  }

}

case class BlockTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], blockOps: List[PointOperation[A,B]]): ParArray[B] = {

    val blockSize = blockOps.size
    (for (block <- im.matrix.toIterator grouped blockSize)
      yield for (i <- 0 until blockSize)
        yield blockOps(i) runOn block(i)).toParArray.flatten
  }

  def traverseAndReduce[A,B,C <: AnyVal : ClassTag](im: Image[A],
                               blockOps: List[PointOperation[A,B]],
                                blockFold: Seq[B] => C): ParArray[C] = {

    val blockSize = blockOps.size
    val tempMat = for (block <- im.matrix.toIterator grouped blockSize)
      yield blockFold (for (i <- 0 until blockSize)
        yield blockOps(i) runOn block(i))
    tempMat.toArray.par
  }
}


case class NeighbourTraverse() extends Traversal {

  def traverse[A,B](im: Image[A], op: NeighbourhoodOperation[A,B], verticalBuffer: Int, horizontalBuffer: Int): ParArray[B] = {

    val allNeighbourhoods =
      for {
        row <- verticalBuffer until (im.height - verticalBuffer)
        col <- horizontalBuffer until (im.width - horizontalBuffer)
      } yield

        (for {
          v <- -verticalBuffer to verticalBuffer
          u <- -horizontalBuffer to horizontalBuffer
        } yield
          im.matrix(im.width * (row + v) + (col + u))).toList

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