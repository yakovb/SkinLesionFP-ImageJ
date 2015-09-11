package dermatological

import images.{Image, ParImage}
import operations.{Histo_3ChannelTraverse, TransformThreeChannelToHistogram}

import scala.collection.parallel.ParMap

object ColourVariegation {

  def getVariegationMeasures = (im: Image[Int]) => {
    val mask = MaskMaking.maskArrayColour (im)
    val dummyImage = ParImage(mask, 1,mask.length)
    val histos: Map[String, ParMap[Int, Int]] = TransformThreeChannelToHistogram(dummyImage, Histo_3ChannelTraverse()) transform

    histos mapValues calcNormalisedStdDeviation
  }

  private def calcNormalisedStdDeviation(histo: ParMap[Int,Int]) =
    calcStdDeviation(histo) / getMax(histo)

  private def calcStdDeviation(histo: ParMap[Int,Int]) =
    Math.sqrt ( calcVariance (histo, calcMean (histo)) )

  private def calcVariance(histo: ParMap[Int,Int], mean: Double) =
    histo.foldLeft(0.0){ case (sum, (pixVal, count)) => sum + (Math.pow(pixVal - mean, 2) * count) } / histo.values.sum.toDouble

  private def calcMean(histo: ParMap[Int,Int]) =
  histo.foldLeft(0.0){ case (sum, (pixVal, count)) => sum + (pixVal * count) } / histo.values.sum.toDouble

  private def getMax(histo: ParMap[Int,Int]) =
    histo.toList.sortWith((p1,p2) => p1._2 > p2._2).head._1
}
