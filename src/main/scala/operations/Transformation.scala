package operations

import images.{Image, ParImage}

import scala.collection.parallel.ParMap
import scala.collection.parallel.immutable.ParSet
import scala.reflect.ClassTag

trait Transformation

case class TransformToMask[A](image: Image[A],
                              traversal: MaskTraverse,
                              test: A => Boolean) extends Transformation {

  def transform: ParSet[A] =
    traversal traverse (image, test)
}

case class TransformSimple[A,B](image: Image[A],
                                traversal: PointTraverse,
                                pointOps: PointOperation[A,B]*) extends Transformation {

  def transform: ParImage[B] = pointOps.length match {
      case 1 => {
        val newMat = traversal traverse(image, pointOps.head)
        ParImage(newMat, image.width, image.height)
      }
      case x => {
        val newMat = traversal traverseAndExpand (image, pointOps.toList)
        ParImage(newMat, image.width * x, image.height)
      }
    }
}

case class TransformBlock[A,B](image: Image[A],
                                traversal: BlockTraverse,
                                opList: PointOp_1Channel[A,B]*) extends Transformation {

  def transform: ParImage[B] = {
    val newMat = traversal traverse (image, opList.toList)
    ParImage(newMat, image.width, image.height)
  }
}

case class TransformBlockReduce[A,B,C<:AnyVal : ClassTag](image: Image[A],
                                       traversal: BlockTraverse,
                                       blockFold: Seq[B] => C,
                                       opList: PointOp_1Channel[A,B]*) extends Transformation {

  def transform: ParImage[C] = {
    val newMat = traversal traverseAndReduce (image, opList.toList, blockFold)
    val reduction = opList.length
    ParImage(newMat, image.width / reduction, image.height)
  }
}


case class TransformNeighbourhood[A,B](image: Image[A],
                                       traversal: NeighbourTraverse,
                                       neighbourOp: NeighbourhoodOperation[A,B]) extends Transformation {

  def transform: Image[B] = {
    val imageBuffer = neighbourOp match {
      case LinearFilter(kernel, _,_,_) => kernel.width
      case NonLinearFilterNoKernel(b, _) => b
      case _ => throw new Exception(s"unrecognised neighbourhood operation: ${neighbourOp.toString}}")
    }
    val neighbourBuffer = imageBuffer / 2
    val newMat = traversal traverse(image, neighbourOp, neighbourBuffer, neighbourBuffer)
    ParImage(newMat, image.width - imageBuffer + 1, image.height - imageBuffer + 1)
  }
}


case class TransformOneChannelToHistogram[A](image: Image[A],
                                    traversal: Histo_1ChannelTraverse) extends Transformation {

  def transform: ParMap[A, Int] =
    traversal traverse image
}

case class TransformThreeChannelToHistogram(image: Image[Int],
                                             traversal: Histo_3ChannelTraverse) extends Transformation {

  def transform: Map[String, ParMap[Int, Int]] =
    traversal traverse image
}