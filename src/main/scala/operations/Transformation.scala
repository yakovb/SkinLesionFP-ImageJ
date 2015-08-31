package operations

import images.{Image, ParImage}

import scala.collection.parallel.ParMap

sealed trait Transformation

case class TransformSimple[A,B](image: Image[A],
                                traversal: PointTraverse,
                                pointOps: PointOperation[A,B]*) extends Transformation {

  def transform: ParImage[B] = {
    val newMat = traversal traverse (image, pointOps.head)
    ParImage(newMat, image.width, image.height)
  }
  
  def transformExpand: ParImage[Array[B]] = {
    val newMat = traversal traverseAndExpand (image, pointOps)
    ParImage(newMat, image.width, image.height)
  } 
}

case class TransformBlock[A,B](image: Image[A],
                                traversal: BlockTraverse,
                                opList: List[PointOp_1Channel[A,B]]) extends Transformation {

  def transform: ParImage[B] = {
    val newMat = traversal traverse (image, opList)
    ParImage(newMat, image.width, image.height)
  }
}


case class TransformNeighbourhood[A,B](image: Image[A],
                                       traversal: NeighbourTraverse,
                                       neighbourOp: NeighbourhoodOperation[A,B]) extends Transformation {

  def transform: Image[B] = {
    val buffer = neighbourOp match {
      case LinearFilter(kernel, _,_,_) => kernel.width
      case NonLinearFilterNoKernel(b, _) => b
      case _ => throw new Exception(s"unrecognised neighbourhood operation: ${neighbourOp.toString}}")
    }
    val newMat = traversal traverse(image, neighbourOp, buffer, buffer)
    ParImage(newMat, image.width - buffer, image.height - buffer)
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