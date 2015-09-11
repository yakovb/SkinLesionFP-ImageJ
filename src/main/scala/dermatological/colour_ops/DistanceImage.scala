package dermatological.colour_ops

import core._

object DistanceImage {

  def getBackgroundMedian =
    TransformLabWindowsToMedian(_: Image[Array[Float]], TraverseBackgroundWindows(), MedianOfWindowsOp(labMedianCalc)) transform


  case class TransformLabWindowsToMedian(image: Image[Array[Float]],
                                         trav: TraverseBackgroundWindows,
                                         op: NeighbourhoodOperation[List[Array[Float]], Array[Float]]) extends Transformation {
    def transform: Array[Float] =
      trav traverse (image, 10, op)
  }


  case class TraverseBackgroundWindows() extends Traversal {
    def traverse(im: Image[Array[Float]],
                 windowSize: Int,
                 neighbourhoodOp: NeighbourhoodOperation[List[Array[Float]],Array[Float]]) = {

      def getIndex(row: Int, col: Int) = im.width * row + col

      def getCorner(rBegin: Int, rEnd: Int, cBegin: Int, cEnd: Int) = {
        for {
          r <- rBegin until rEnd
          c <- cBegin until cEnd
        } yield im.matrix(getIndex(r, c))
      }

      val upperLeft = getCorner(0, windowSize, 0, windowSize) toList
      val upperRight = getCorner(0, windowSize, im.width - windowSize, im.width) toList
      val lowerLeft = getCorner(im.height - windowSize, im.height, 0, windowSize) toList
      val lowerRight = getCorner(im.height - windowSize, im.height, im.width - windowSize, im.width) toList
      val allWindows = List(upperLeft, upperRight, lowerLeft, lowerRight)

      neighbourhoodOp runOn allWindows
    }
  }


  case class MedianOfWindowsOp(f: List[List[Array[Float]]] => Array[Float])
    extends NeighbourhoodOperation[List[Array[Float]], Array[Float]] {

    override def runOn(neighbourhood: List[List[Array[Float]]]): Array[Float] =
      f(neighbourhood)
  }


  def labMedianCalc(windows: List[List[Array[Float]]]) = {
    val all_Ls = getArrayElems(windows, 0) sorted
    val all_as = getArrayElems(windows, 1) sorted
    val all_bs = getArrayElems(windows, 2) sorted

    val median_L = all_Ls (all_Ls.length / 2)
    val median_a = all_as (all_as.length / 2)
    val median_b = all_bs (all_bs.length / 2)

    Array(median_L, median_a, median_b)
  }

  private def getArrayElems(windows: List[List[Array[Float]]], index: Int) =
    for {
      window <- windows
      labArray <- window
    }  yield labArray(index)


  def distanceImage =
    (im: Image[Array[Float]], med: Array[Float]) =>
      TransformSimple(im, PointTraverse(), PointOpDifference(med)) transform


  case class PointOpDifference(medians: Array[Float])
    extends PointOperation[Array[Float], Float] {

    override def runOn(pixels: Array[Float]): Float =
      distanceCalc(pixels, medians)

    private def distanceCalc(labPixels: Array[Float], medians: Array[Float]) = {
      require(labPixels.length == medians.length)
      val pairs = labPixels zip medians
      val diffs = pairs map (p => Math.pow(p._1 - p._2, 2))
      Math.sqrt(diffs.sum).toFloat
    }
  }



}
