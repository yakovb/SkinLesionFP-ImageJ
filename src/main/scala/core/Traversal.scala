package core

import scala.collection.parallel.ParMap
import scala.collection.parallel.immutable.ParSet
import scala.collection.parallel.mutable.ParArray
import scala.reflect.ClassTag

/** Base trait encapsulating traversal of an [[core.Image]]'s pixel array */
trait Traversal

/**
 * Creates a point-set mask of an [[core.Image]]'s pixels based on a predicate
 */
case class MaskTraverse() extends Traversal {

  /**
   * Traverses all pixels and adds their coordinates to a mask if they satisfy a predicate
   * @param im source [[core.Image]]
   * @param predicate the predicate used to test pixels
   * @tparam A source pixel type
   * @return [[scala.collection.parallel.ParSet]] of (row, column) tuples
   */
  def traverse[A](im: Image[A], predicate: A => Boolean): ParSet[(Int,Int)] = {
    val resultArray = for {
      r <- 0 until im.height
      c <- 0 until im.width
      if predicate(im.matrix(im.width * r + c))
    } yield (r,c)
    resultArray.toSet.par
  }
}

/**
 * Creates a new [[core.Image]] by applying a function to all of its pixels
 */
case class PointTraverse() extends Traversal {

  /**
   * Traverses all pixels and applies a [[core.PointOperation]] to them
   * @param im source [[core.Image]]
   * @param pointOp [[core.PointOperation]] applied to all pixels
   * @tparam A source pixel type
   * @tparam B result pixel type
   * @return a new pixel array of type [[scala.collection.parallel.mutable.ParArray]]
   */
  def traverse[A,B](im: Image[A], pointOp: PointOperation[A,B]): ParArray[B] =
    for (pixel <- im.matrix) yield pointOp runOn pixel

  /**
   * Traverses all pixels and expands each of them into two or more pixels by applying several [[core.PointOperation]]s to each traversed pixel
   * @param im source [[core.Image]]
   * @param expansionOps list of [[core.PointOperation]]s to apply to each pixel
   * @tparam A source pixel type
   * @tparam B result pixel type
   * @return a new pixel array of type [[scala.collection.parallel.mutable.ParArray]]
   */
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

    Map("Red" -> redHisto, "Green" -> greenHisto, "Blue" -> blueHisto)
  }


}