package operations

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import dermatological._
import dermatological.binary_ops.{BinaryImage, Moments}
import dermatological.colour_ops.ColourOps
import ij.io.Opener

case class Analyser(directory: String) {

  def getResults = for (file <- new File(directory).listFiles().toIterator) yield {
    analyseImage(file.getAbsolutePath)
  }

  private def analyseImage(imageFile: String): Map[String, Double] = {
    val src = new Opener().openImage(imageFile)
    val original = InteropImageJ.getIntParImage(src.getProcessor)

    val pipelineMakeBinary =
      ColourOps.rgb_2_grey andThen
        BinaryImage.otsuThreshold andThen
        HolesAndSpecs.fillHoles andThen
        HolesAndSpecs.removeSpecs
    val binary = pipelineMakeBinary (original)

    val asymmetryPipeline =
      Rotation.rotate andThen
        Asymmetry.calculateAsymmetry
    val asymmetryMeasure = asymmetryPipeline (binary)

    val perimeterMeasure = Perimeter.countPerimeter (binary)
    val areaMeasure = Moments.getCentralMoments (binary) ("area")
    val circularityMeasure = 4 * Math.PI * areaMeasure / Math.pow(perimeterMeasure, 2)

    val colourVarMap = ColourVariegation.getVariegationMeasures (original)

    Map(
      imageFile.takeRight(6) -> 0f,
      "Asymmetry" -> asymmetryMeasure.toFloat,
      "Circularity" -> circularityMeasure.toFloat,
      "RedVariegation" -> colourVarMap("Red").toFloat,
      "GreenVariegation" -> colourVarMap("Green").toFloat,
      "BlueVariegation" -> colourVarMap("Blue").toFloat )
  }

}

object RunBulkAnalysis extends App {
  val analyser = Analyser("/home/yakov/Dropbox/Birkbeck/Project/MoleChecker/Moles/Mine/Input")
  val results = analyser.getResults
  var output = new StringBuilder("Feature-Descriptor,Measure\n")
  for {
    map <- results.toList
    (k,v) <- map
  } output.append(s"$k,$v\n")
  Files.write(Paths.get("/home/yakov/Dropbox/Birkbeck/Project/MoleChecker/Moles/Mine/output.csv"),
    output.toString().getBytes(StandardCharsets.UTF_8))
}