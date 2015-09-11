package core

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import dermatological._
import dermatological.binary_ops.{Asymmetry, BinaryImage, Moments}
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

    val mask = MaskMaking.maskSetBinary (binary)
    val maskedImage = LesionMask.maskColourImage (original, mask)
    val colourVarMap = ColourVariegation.getVariegationMeasures (maskedImage)

    Map(
      " File: "+imageFile.takeRight(6) -> 0f,
      "Asymmetry" -> asymmetryMeasure.toFloat,
      "Border Irreg." -> circularityMeasure.toFloat,
      "Red Variegation" -> colourVarMap("Red").toFloat,
      "Green Variegation" -> colourVarMap("Green").toFloat,
      "Blue Variegation" -> colourVarMap("Blue").toFloat )
  }

}

object RunBulkAnalysis extends App {
  val analyser = Analyser("/home/yakov/Dropbox/Birkbeck/Project/MoleChecker/Moles/Mine/Input")
  val results = analyser.getResults
  var output = new StringBuilder("Feature-Descriptor,Measure\n")
  for {
    map <- results.toList
    (k,v) <- map.toList.sorted
  } output.append(s"$k,$v\n")
  Files.write(Paths.get("/home/yakov/Dropbox/Birkbeck/Project/MoleChecker/Moles/Mine/Input/output.csv"),
    output.toString().getBytes(StandardCharsets.UTF_8))
}