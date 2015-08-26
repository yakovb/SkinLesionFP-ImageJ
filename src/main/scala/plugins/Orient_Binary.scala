import ij.ImagePlus
import ij.plugin.filter.PlugInFilter
import ij.plugin.filter.PlugInFilter._
import ij.process.{ByteProcessor, ImageProcessor}

class Orient_Binary extends PlugInFilter {
  override def setup(arg: String, imp: ImagePlus): Int =
    DOES_8G

  override def run(ip: ImageProcessor): Unit = {
    val pixels = ip.getPixels.asInstanceOf[Array[Byte]]
    val w = ip.getWidth
    val h = ip.getHeight

    val area = pixels.foldLeft(0){ case (sum, pix) => if (pix == 0) sum+1 else sum }

    var m00 = 0.0
    var m10 = 0.0
    var cent10 = 0.0
    var m01 = 0.0
    var cent01 = 0.0
    var m11 = 0.0
    var cent11 = 0.0
    var m20 = 0.0
    var cent20 = 0.0
    var m02 = 0.0
    var cent02 = 0.0

    // get moments
    for {
      r <- 0 until h
      c <- 0 until w
      if pixels(w * r + c) == 0
    } {
      m00 += 1
      m10 += c
      m01 += r
      m11 += r * c
      m20 += c*c
      m02 += r*r
    }

    val centroidX = m10 / area
    val centroidY = m01 / area

    // get central moments
    for {
                      r <- 0 until h
      c <- 0 until w
      if pixels(w * r + c) == 0
    } {
      cent10 += c - centroidX
      cent01 += r - centroidY
      cent11 += (c - centroidX) * (r - centroidY)
      cent20 += Math.pow(c - centroidX, 2)
      cent02 += Math.pow(r - centroidY, 2)
    }

    val orientation = Math.atan2(2 * cent11, cent20 - cent02) / 2

    println("area is " + area)
    println("centroid is ("+centroidX.toInt + ", "+centroidY.toInt +")")
    println("orientation is "+ orientation)

    def rotate(a: Double, x: Int, y: Int): (Int,Int) = {
      val s = Math.sin(a)
      val c = Math.cos(a)

      val xnew = x - centroidX
      val ynew = y - centroidY

      val rotx = xnew * c - ynew * s
      val roty = xnew * s + ynew * c

      ((rotx + centroidX).round.toInt, (roty + centroidY).round.toInt)
    }

    def rotation(angle: Double) = {
      val region = scala.collection.mutable.Set[(Int, Int)]()
      for {
        r <- 0 until h
        c <- 0 until w
      } {
        if (pixels(w * r + c) == 0) {
          region.add(rotate(angle, c, r))
        }
        pixels(w*r+c) = 255.toByte
      }
      for {
        r <- 0 until h
        c <- 0 until w
      } {
        if (region.contains((c,r))) pixels(w*r+c) = 0
      }
    }
    rotation(- orientation)

    val top = new ByteProcessor(w, h)
    val topPix = new Array[Byte](w * h)
    val bottom = new ByteProcessor(w, h)
    val botPix = new Array[Byte](w * h)
    val overlap = new ByteProcessor(w, h)
    val overPix = new Array[Byte](w * h)
    for {
      r <- 0 until h
      c <- 0 until w
    } {
      topPix(w*r+c) = 255.toByte
      botPix(w*r+c) = 255.toByte
      overPix(w*r+c) = 255.toByte
    }

    def topHalf() = {
      for {
        r <- 0 until centroidY.round.toInt
        c <- 0 until w
      } topPix(w * r + c) = pixels(w * r + c)
      top.setPixels(topPix)
    }
    def bottomHalf() = {
      for {
        r <- centroidY.round.toInt until h
        c <- 0 until w
      } botPix(w * r + c) = pixels(w * r + c)
      bottom.setPixels(botPix)
    }
    var xorArea = 0
    def overHalf() = {
      for {
        r <- centroidY.round.toInt+1 until h
        c <- 0 until w
      } {
        val testY = centroidY.round.toInt - (r - centroidY.round.toInt)
        overPix(w*r+c) = (topPix(w * testY + c), botPix(w*r+c)) match {
          case (0,0) => 255.toByte
          case (0, white) if white == 255.toByte => {xorArea += 1; 0}
          case (white,0) if white == 255.toByte => {xorArea += 1; 0}
          case _ => 255.toByte
        }
      }
      overlap.setPixels(overPix)
    }

    topHalf()
    bottomHalf()
    overHalf()
    new ImagePlus("Top", top).show()
    new ImagePlus("Bottom", bottom).show()
    new ImagePlus("Overlap", overlap).show()

    println("Asymmetry (XOR area over total area) is: " + xorArea / m00)
  }
}
