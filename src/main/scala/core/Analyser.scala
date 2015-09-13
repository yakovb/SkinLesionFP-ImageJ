package core

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import dermatological.binary_ops._
import dermatological.colour_ops.{ColourOps, ColourVariegation}
import dermatological.other_ops.{LesionMask, MaskMaking, Perimeter}
import ij.io.Opener

/**
 * Utility class which takes a directory and runs a bulk dermatological analysis of the images contained in it
 * @param directory location of source images
 */
case class Analyser(directory: String) {

  /**
   * @return [[scala.collection.Iterator]] of [[scala.collection.Map]]s mapping a feature descriptor to its resulting
   *        measure for each image file
   */
  def getResults: Iterator[Map[String, Double]] = for (file <- new File(directory).listFiles().toIterator) yield {
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

    val pathString = Paths.get(imageFile).getFileName.toString

    Map(
      " File: " + pathString -> 0f,
      "Asymmetry" -> asymmetryMeasure.toFloat,
      "Border Irreg." -> circularityMeasure.toFloat,
      "Red Variegation" -> colourVarMap("Red").toFloat,
      "Green Variegation" -> colourVarMap("Green").toFloat,
      "Blue Variegation" -> colourVarMap("Blue").toFloat )
  }

}

/**
 * Programme entry point. Shows welcome text and instructions on how to run a bulk analysis of skin lesion images.
 * Outputs results as a CSV file written to the directory containing the skin lesion images.
 */
object RunBulkAnalysis extends App {
  println("Welcome to the Skin Lesion analysis programme, written by Yakov Boglev for his Birkbeck MSc CS final project.")
  println()
  println("Please enter the full path of the directory holding the image you want to process.")
  println("Make sure the folder contains images and that there is NO EXISTING output.csv file in it!")
  val inputDirectory = scala.io.StdIn.readLine("> ")
  val analyser = Analyser(inputDirectory)
  println("\nprocessing...")
  val results = analyser.getResults
  var output = new StringBuilder("Feature-Descriptor,Measure\n")
  for {
    map <- results.toList
    (k,v) <- map.toList.sorted
  } output.append(s"$k,$v\n")
  Files.write(Paths.get("/home/yakov/Dropbox/Birkbeck/Project/MoleChecker/Moles/Mine/Input/output.csv"),
    output.toString().getBytes(StandardCharsets.UTF_8))
  println("Complete. Check your folder for the output.")
}