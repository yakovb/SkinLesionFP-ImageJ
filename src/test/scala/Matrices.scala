import images.{Image, ParImage}

import scala.io.Source

class Matrices {
  val root = "/home/yakov/src/SkinLesionFP-ImageJ/src/test/resources/"
  val fileList = List("allZeros.csv", "allOnes.csv", "allRand.csv", "lena_grayscale.csv")

  def getIntImages = fileList.map(f => makeImage(f))

  private def makeImage(file: String): Image[Int] = {
    val stringLines: List[Array[String]] = Source.fromFile(root + file).getLines().map(line => line.split(',')).toList
    val dim = stringLines.size
    val numLines = stringLines.map(line => line.map(_.toInt)).toArray
    ParImage(numLines.flatten.par, dim, dim)
  }
}
//
//object MTest extends App {
//  val m = new Matrices
//  val z = m.getIntImages.head
//  z.matrix.foreach(println)
//}