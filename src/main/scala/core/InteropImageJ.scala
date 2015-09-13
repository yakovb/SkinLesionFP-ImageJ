package core

import ij.process.{ByteProcessor, ColorProcessor, FloatProcessor, ImageProcessor}
import ij.{ImagePlus, ImageStack}

/**
 * Utility methods to convert into and out of ImageJ formats
 */
object InteropImageJ {

  /**
   * Creates an [[core.Image]] with [[scala.Byte]] pixel array from an [[ij.process.ImageProcessor]]
   * @param ip [[ij.process.ImageProcessor]] containing the image
   * @return [[core.Image]]
   */
  def getByteParImage(ip: ImageProcessor): Image[Byte] =
    ParImage(ip.getPixels.asInstanceOf[Array[Byte]].par, ip.getWidth, ip.getHeight)

  /**
   * Creates an [[core.Image]] with [[scala.Int]] pixel array from an [[ij.process.ImageProcessor]]
   * @param ip [[ij.process.ImageProcessor]] containing the image
   * @return [[core.Image]]
   */
  def getIntParImage(ip: ImageProcessor): Image[Int] =
    ParImage(ip.getPixels.asInstanceOf[Array[Int]].par, ip.getWidth, ip.getHeight)

  /**
   * Creates [[ij.process.ByteProcessor]] from an [[core.Image]] with a [[scala.Byte]] pixel array
   * @param image source [[core.Image]]
   * @return [[ij.process.ByteProcessor]]
   */
  def makeGreyProcessor(image: Image[Byte]) =
    new ByteProcessor(image.width, image.height, image.matrix.toArray)

  /**
   * Creates [[ij.process.ColorProcessor]] from an [[core.Image]] with a [[scala.Int]] pixel array
   * @param image source [[core.Image]]
   * @return [[ij.process.ColorProcessor]]
   */
  def makeColourProcessor(image: Image[Int]) =
    new ColorProcessor(image.width, image.height, image.matrix.toArray)

  /**
   * Creates [[ij.process.FloatProcessor]] from an [[core.Image]] with a [[scala.Float]] pixel array
   * @param image source [[core.Image]]
   * @return [[ij.process.FloatProcessor]]
   */
  def makeFloatProcessor(image: Image[Float]) =
    new FloatProcessor(image.width, image.height, image.matrix.toArray)

  /**
   * Creates an [[ij.ImagePlus]] from an [[ij.process.ImageProcessor]]
   * @param title title of the resulting image
   * @param ip source [[ij.process.ImageProcessor]]
   * @return [[ij.ImagePlus]]
   */
  def makeImagePlus(title: String, ip: ImageProcessor) =
    new ImagePlus(title, ip)

  /**
   * Creates [[ij.ImagePlus]] wrapping an [[ij.ImageStack]] from an [[core.Image]] with an underlying pixel array in which
   * each element contains multiple channels represented by an array of [[scala.Float]]
   * @param title title of the resulting image stack
   * @param image source [[core.Image]]
   * @return [[ij.ImagePlus]]
   */
  def arrayFloatImage_to_IJstack(title: String, image: Image[Array[Float]]) = {
    val c1 = for (i <- 0 until image.matrix.length) yield image.matrix(i)(0)
    val c2 = for (i <- 0 until image.matrix.length) yield image.matrix(i)(1)
    val c3 = for (i <- 0 until image.matrix.length) yield image.matrix(i)(2)

    val stack = new ImageStack(image.width, image.height)
    List(c1,c2,c3) foreach (arr => stack addSlice ("slice", arr.toArray))
    new ImagePlus(title, stack)
  }

  /**
   * Creates [[ij.ImagePlus]] wrapping an [[ij.ImageStack]] from an [[core.Image]] with an underlying pixel array in which
   * channels are represented by consecutive blocks of pixels
   * @param title title of resulting image stack
   * @param image source [[core.Image]]
   * @param blocksize size of pixel blocks (i.e. number of channels)
   * @return [[ij.ImagePlus]]
   */
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
