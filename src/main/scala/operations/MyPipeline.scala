package operations

import images.{Image, ParImage}

object MyPipeline {
  def getRedDirect(pixel: Int) = (pixel >> 16) & 0xff
  def getGreenDirect(pixel: Int) = (pixel >> 8) & 0xff
  def getBlueDirect(pixel: Int) = pixel & 0xff
  def combineRgbDirect(rgbList: List[Int]) = rgbList match {
    case List(r,g,b) =>
      ((r & 0xff) << 16) + ((g & 0xff) << 8) + (b & 0xff)
  }

  def getRed = PointOp_1Channel(getRedDirect)
  def getGreen = PointOp_1Channel(getGreenDirect)
  def getBlue = PointOp_1Channel(getBlueDirect)

  def rgb_2_grey = {

    def toGreyOp =
      PointOp_3Channel(_ * 0.2126, _ * 0.7152, _ * 0.0722) ((r,g,b) => (r+g+b).toByte)

    TransformSimple(_: Image[Int], PointTraverse(), toGreyOp) transform
  }


  def medianOneChannel(region: List[Int]) = {
    val sorted = region.sorted
    val l = sorted.length
    if (l % 2 == 0) (sorted(l / 2) + sorted(l / 2 + 1)) / 2
    else sorted(l / 2)
  }

  def splitRgbRegion(region: List[Int]) = {
    val reds = region map getRedDirect
    val greens = region map getGreenDirect
    val blues = region map getBlueDirect
    List(reds,greens,blues)
  }





  def medianFilter = {

    def medianThreeChannel(mixedRegion: List[Int]) = {
      val medians = splitRgbRegion(mixedRegion) map medianOneChannel
      combineRgbDirect(medians)
    }
    def medianOp = NonLinearFilterNoKernel(3, medianThreeChannel)
    TransformNeighbourhood(_: Image[Int], NeighbourTraverse(), medianOp) transform
  }

  def rgb_to_xyz = {
    def xyzStep(pixel: Int) = {
      pixel / 255f match {
        case f if f > 0.04045 => (Math.pow((f + 0.055) / 1.055, 2.4) * 100).toFloat
        case f => f / (12.92 * 100).toFloat
      }
    }
    def xyzOp =
      PointOp_3Channel(xyzStep, xyzStep, xyzStep)( (r,g,b) => {
        val x = (r * 0.4124 + g * 0.3576 + b * 0.1805).toFloat
        val y = (r * 0.2126 + g * 0.7152 + b * 0.0722).toFloat
        val z = (r * 0.0193 + g * 0.1192 + b * 0.9505).toFloat
        Array(x,y,z)
      })
    TransformSimple(_: Image[Int], PointTraverse(), xyzOp) transform
  }


  def xyz_to_Lab = {
    val EPSILON = 0.008856
    val KAPPA = 909.3
    val Xn = 0.9642
    val Yn = 1.0
    val Zn = 0.8249

    def xRef(x: Float) = (x / Xn).toFloat
    def yRef(y: Float) = (y / Yn).toFloat
    def zRef(z: Float) = (z / Zn).toFloat

    def getLab(xyzArray: Array[Float]) = xyzArray match {
      case Array(x,y,z) => {
        val (xr, yr, zr) = (xRef(x), yRef(y), zRef(z))
        val fx = if (xr > EPSILON) Math.pow(xr, 1f/3f) else (KAPPA * xr + 16f) / 16f
        val fy = if (yr > EPSILON) Math.pow(yr, 1f/3f) else (KAPPA * yr + 16f) / 16f
        val fz = if (zr > EPSILON) Math.pow(zr, 1f/3f) else (KAPPA * zr + 16f) / 16f

        val L = (116f * fy - 16f).toFloat
        val a = (500f * (fx - fy)).toFloat
        val b = (200f * (fy - fz)).toFloat
        Array(L,a,b)
      }
    }
    def labOp = PointOp_1Channel(getLab)
    TransformSimple(_: Image[Array[Float]], PointTraverse(), labOp) transform
  }


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


  def otsuThreshold =
    OtsuTransform(_: Image[Byte]) transform


  def greyHistogram = TransformOneChannelToHistogram(_: Image[Byte], Histo_1ChannelTraverse()) transform


  case class MakeBinary(threshold: Int) extends PointOperation[Byte,Byte] {
    override def runOn(pixel: Byte): Byte =
      doThreshold (pixel, threshold)

    private def doThreshold(pixel: Byte, t: Int) =
      if (pixel < t.toByte) 255.toByte else 0.toByte
  }

  case class OtsuTransform(im: Image[Byte]) extends Transformation {

    def transform: Image[Byte] = {
      val newMat = PointTraverse() traverse (im, MakeBinary(getThreshold))
      ParImage(newMat, im.width, im.height)
    }

    private def getThreshold = {
      val histo = greyHistogram (im)
      val sumTotal = histo.foldLeft(0){ case (sum, (pixVal, count)) => sum + (pixVal * count) }

      def go(grey: Int, sumBack: Int, wBack: Int, wFront: Int, varMax: Double, thresh: Int): Int = {
        if (grey > 255) thresh

        else {
          val tempWB = wBack + histo.getOrElse(grey.toByte, 0)
          val tempWF = im.matrix.length - tempWB

          if (tempWB == 0) go(grey+1, sumBack, tempWB, wFront, varMax, thresh)

          else if (tempWF == 0) thresh

          else {
            val tempSumB = sumBack + grey * histo.getOrElse(grey.toByte, 0)
            val meanBack = tempSumB.toFloat / tempWB.toFloat
            val meanFront = (sumTotal.toFloat - tempSumB.toFloat) / tempWF.toFloat
            val btwVar = tempWB.toFloat * tempWF.toFloat * Math.pow(meanBack - meanFront, 2)

            val (newVarMax, newThresh) = if (btwVar > varMax) (btwVar, grey) else (varMax, thresh)
            go(grey+1, tempSumB, tempWB, tempWF, newVarMax, newThresh)
          }
        }
      }
      go(0,0,0,0,0,0)
    }

  }

}
