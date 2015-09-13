package core

import scala.collection.parallel.ParMap
import scala.collection.parallel.immutable.ParSet
import scala.reflect.ClassTag

/**
 * Base trait encapsulating image transformations
 */
trait Transformation

/**
 * Transforms an image to a mask of pixel coordinates that satisfy a predicate
 * @param image source [[core.Image]]
 * @param traversal [[core.MaskTraverse]] to traverse the pixel array
 * @param test predicate which pixels must satisfy to be part of the mask
 * @tparam A source pixel type
 */
case class TransformToMask[A](image: Image[A],
                              traversal: MaskTraverse,
                              test: A => Boolean) extends Transformation {

  /**
   * @return the image mask as a [[scala.collection.parallel.ParSet]]
   */
  def transform: ParSet[(Int,Int)] =
    traversal traverse (image, test)
}

/**
 * Transforms an image by applying a function to each of its pixels
 * @param image source [[core.Image]]
 * @param traversal point traversal method
 * @param pointOps operation to be applied to each pixel
 * @tparam A source pixel type
 * @tparam B result pixel type
 */
case class TransformSimple[A,B](image: Image[A],
                                traversal: PointTraverse,
                                pointOps: PointOperation[A,B]*) extends Transformation {

  /**
   * Executes the transformation. Detects whether the one or more [[core.PointOperation]]s have been passed as arguments:
   * If one, the transformation returns an [[core.Image]] of the same dimension as the input.
   * If more than one, returns an [[core.Image]] of dimension {{{src_pixel_array * number_of_operations}}}
   * @return transformed [[core.Image]]
   */
  def transform: ParImage[B] = pointOps.length match {
      case 1 =>
        val newMat = traversal traverse(image, pointOps.head)
        ParImage(newMat, image.width, image.height)
      case x =>
        val newMat = traversal traverseAndExpand (image, pointOps.toList)
        ParImage(newMat, image.width * x, image.height)
  }
}

/**
 * Transforms an image where channels are stored as consecutive pixel blocks by applying functions to each pixel in a block
 * @param image source [[core.Image]]
 * @param traversal block traversal method
 * @param opList list of [[core.PointOp_1Channel]] operations
 * @tparam A source pixel type
 * @tparam B result pixel type
 */
case class TransformBlock[A,B](image: Image[A],
                                traversal: BlockTraverse,
                                opList: PointOp_1Channel[A,B]*) extends Transformation {

  /**
   * @return transformed [[core.Image]]
   */
  def transform: ParImage[B] = {
    val newMat = traversal traverse (image, opList.toList)
    ParImage(newMat, image.width, image.height)
  }
}

/**
 * Transforms an image where channels are stored as consecutive pixel blocks by applying functions to each pixel in the block
 * and then reducing the block to a single value
 * @param image source [[core.Image]]
 * @param traversal block traversal method
 * @param blockFold folds transformed block into single value
 * @param opList functions operating on each pixel in the block
 * @tparam A source pixel type
 * @tparam B intermediate pixel type (result type of opList functions)
 * @tparam C result pixel type
 */
case class TransformBlockReduce[A,B,C<:AnyVal : ClassTag](image: Image[A],
                                       traversal: BlockTraverse,
                                       blockFold: Seq[B] => C,
                                       opList: PointOp_1Channel[A,B]*) extends Transformation {

  /**
   * @return transformed [[core.Image]]
   */
  def transform: ParImage[C] = {
    val newMat = traversal traverseAndReduce (image, opList.toList, blockFold)
    val reduction = opList.length
    ParImage(newMat, image.width / reduction, image.height)
  }
}

/**
 * Transforms an image by applying a function to a neighbourhood of pixels
 * @param image source [[core.Image]]
 * @param traversal neighbourhood traversal method
 * @param neighbourOp function to apply to the neighbourhood
 * @tparam A source pixel type
 * @tparam B result pixel type
 */
case class TransformNeighbourhood[A,B](image: Image[A],
                                       traversal: NeighbourTraverse,
                                       neighbourOp: NeighbourhoodOperation[A,B]) extends Transformation {

  /**
   * @return transformed [[core.Image]]
   */
  def transform: Image[B] = {
    val imageBuffer = neighbourOp match {
      case LinearFilter(kernel, _,_,_) => kernel.width
      case NonLinearFilter(b, _) => b
      case _ => throw new Exception(s"unrecognised neighbourhood operation: ${neighbourOp.toString}}")
    }
    val neighbourBuffer = imageBuffer / 2
    val newMat = traversal traverse(image, neighbourOp, neighbourBuffer, neighbourBuffer)
    ParImage(newMat, image.width - imageBuffer + 1, image.height - imageBuffer + 1)
  }
}

/**
 * Transforms an image with a one channel pixel array into a histogram of pixel intensities
 * @param image source [[core.Image]]
 * @param traversal histogram traversal method
 * @tparam A source pixel type
 */
case class TransformOneChannelToHistogram[A](image: Image[A],
                                    traversal: Histo_1ChannelTraverse) extends Transformation {

  /**
   * @return [[scala.collection.parallel.ParMap]] of pixel values to pixel counts
   */
  def transform: ParMap[A, Int] =
    traversal traverse image
}

/**
 * Transforms an image with a three channel pixel array into a histogram of pixel intensities. Assumes input is an image
 * containing a pixel array of [[scala.Int]] with the three channels packed inside each pixel
 * @param image source [[core.Image]]
 * @param traversal histogram traversal method
 */
case class TransformThreeChannelToHistogram(image: Image[Int],
                                             traversal: Histo_3ChannelTraverse) extends Transformation {

  /**
   * @return [[scala.collection.Map]] of colour channels to [[scala.collection.parallel.ParMap]] of pixel values to pixel counts
   */
  def transform: Map[String, ParMap[Int, Int]] =
    traversal traverse image
}