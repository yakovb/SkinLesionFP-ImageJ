package core

import ij.process.{ByteProcessor, ColorProcessor, FloatProcessor, ImageProcessor}
import ij.{ImagePlus, ImageStack}

object InteropImageJ {

  def getByteParImage(ip: ImageProcessor) =
    ParImage(ip.getPixels.asInstanceOf[Array[Byte]].par, ip.getWidth, ip.getHeight)

  def getIntParImage(ip: ImageProcessor) =
    ParImage(ip.getPixels.asInstanceOf[Array[Int]].par, ip.getWidth, ip.getHeight)

  def makeGreyProcessor(image: Image[Byte]) =
    new ByteProcessor(image.width, image.height, image.matrix.toArray)

  def makeColourProcessor(image: Image[Int]) =
    new ColorProcessor(image.width, image.height, image.matrix.toArray)

  def makeFloatProcessor(image: Image[Float]) =
    new FloatProcessor(image.width, image.height, image.matrix.toArray)

  def makeImagePlus(title: String, ip: ImageProcessor) =
    new ImagePlus(title, ip)

  def arrayFloatImage_to_IJstack(title: String, image: Image[Array[Float]]) = {
    val c1 = for (i <- 0 until image.matrix.length) yield image.matrix(i)(0)
    val c2 = for (i <- 0 until image.matrix.length) yield image.matrix(i)(1)
    val c3 = for (i <- 0 until image.matrix.length) yield image.matrix(i)(2)

    val stack = new ImageStack(image.width, image.height)
    List(c1,c2,c3) foreach (arr => stack addSlice ("slice", arr.toArray))
    new ImagePlus(title, stack)
  }

  def blockImageFloat_to_IJstack(title: String, image: Image[Float], blocksize: Int) = {
    def go(count: Int): List[Array[Float]] = count match {
      case b if b == blocksize => Nil
      case b =>
        (for (i <- Range(b - 1,image.matrix.length,blocksize))
          yield image.matrix(i)).toArray :: go(count + 1)
    }
    val stack = new ImageStack(image.width, image.height)
    println("got here")
    val imList = go(1)
    imList foreach (im => stack addSlice (imList.indexOf(im).toString, im))
    new ImagePlus(title, stack)
  }

}
