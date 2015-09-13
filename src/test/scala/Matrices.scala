import core.{Image, ParImage}

import scala.io.Source

class Matrices {
  val root = "SkinLesionFP-ImageJ/src/test/resources/"
  val greyList = List("allZeros.csv", "allOnes.csv", "allRand.csv", "lena_grayscale.csv")
  val colourList = List("colourBlue.csv", "colourGreen.csv", "colourRed.csv", "colourRandom.csv")
  val completeList = greyList ++ colourList

  def getGreyIntImages = greyList.map(makeImage)
  def getColourIntImages = colourList.map(makeImage)
  def getAllIntImages = completeList.map(makeImage)

  private def makeImage(file: String): Image[Int] = {
    val stringLines: List[Array[String]] = Source.fromFile(root + file).getLines().map(line => line.split(',')).toList
    val dim = stringLines.size
    val numLines = stringLines.map(line => line.map(_.toInt)).toArray
    ParImage(numLines.flatten.par, dim, dim)
  }
}