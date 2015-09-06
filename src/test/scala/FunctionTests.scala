import images.Kernel
import operations._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FunctionTests extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val greyIntImageList = (new Matrices).getGreyIntImages
  val colourIntImageList = (new Matrices).getColourIntImages
  val allIntImageList = (new Matrices).getAllIntImages

  val greyIntImagesTable = Table("grey Int images", greyIntImageList: _*)
  val colourIntImagesTable = Table("colour Int images", colourIntImageList: _*)
  val allIntImagesTable = Table("all Int images", allIntImageList: _*)

  property("PointTraverse traverse with 1-channel op (x + 1) should result in sum of array + length of array") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformSimple[Int,Int](image, PointTraverse(), PointOp_1Channel(_ + 1)) transform;
      newImage.matrix.sum should equal (image.matrix.sum + image.matrix.length)
    }
  }

  property("PointTraverse traverse with 3-channel op (rgb => 3) should result in image of all 3's") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformSimple[Int,Int](image, PointTraverse(), PointOp_3Channel(
        r => 1,
        g => 1,
        b => 1)
        ((r:Int,g:Int,b:Int) => (r + g + b).toByte)) transform;
      newImage.matrix should equal (Array.fill(newImage.width * newImage.height)(3))
    }
  }

  property("PointTraverse traverseAndExpand with 1-channel op (x => x x x) should result in new image length of original * 3") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformSimple[Int,Int](image, PointTraverse(),
        PointOp_1Channel((x:Int) => x),
        PointOp_1Channel((x:Int) => x),
        PointOp_1Channel((x:Int) => x)) transform;
      newImage.matrix.length should equal (image.matrix.length * 3)
    }
  }

  property("PointTraverse traverseAndExpand with 1-channel op (x => x x x) should result in summed image of 3 * original") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformSimple[Int,Int](image, PointTraverse(),
        PointOp_1Channel((x:Int) => x),
        PointOp_1Channel((x:Int) => x),
        PointOp_1Channel((x:Int) => x)) transform;
      newImage.matrix.sum should be (image.matrix.sum * 3)
    }
  }

  property("BlockTraverse traverse with ops (x => 1, y => 0) should give image of same pixel array length") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformBlock[Int,Int](image, BlockTraverse(),
        PointOp_1Channel(x => 1),
        PointOp_1Channel(x => 0)) transform;
      newImage.matrix.length should be (image.matrix.length)
    }
  }

  property("BlockTraverse traverse with ops (x => 1, y => 0) should give summed image of array.length / 2") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformBlock[Int,Int](image, BlockTraverse(),
        PointOp_1Channel(x => 1),
        PointOp_1Channel(x => 0)) transform;
      newImage.matrix.sum.toFloat should be (image.matrix.length / 2.0)
    }
  }

  property("BlockTraverse traverseAndReduce with ops ((x => x, y => y) => x + y) should give image with halved array length") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformBlockReduce[Int,Int,Int](image, BlockTraverse(),(list: Seq[Int]) => list.sum,
        PointOp_1Channel(x => x),
        PointOp_1Channel(y => y)) transform;
      newImage.matrix.length should be (image.matrix.length / 2)
    }
  }

  property("BlockTraverse traverseAndReduce with ops ((x => x, y => y) => x + y) should give image with same array sum") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformBlockReduce[Int,Int,Int](image, BlockTraverse(),(list: Seq[Int]) => list.sum,
        PointOp_1Channel(x => x),
        PointOp_1Channel(y => y)) transform;
      newImage.matrix.sum should be (image.matrix.sum)
    }
  }

  property("Neighbourhood linear filter with empty kernel should throw exception") {
    forAll(allIntImagesTable) { image =>
      an [IllegalArgumentException] should be thrownBy {
        TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
          LinearFilter(
            Kernel(List[Float](),0,0),
            _.toFloat,
            _.toInt))
      }
    }
  }

  property("Neighbourhood non-linear filter with zero neighbourhood size should throw exception") {
    forAll(allIntImagesTable) { image =>
      an [IllegalArgumentException] should be thrownBy {
        TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
          NonLinearFilter(0, lst => lst.max)) transform
      }
    }
  }

  property("Neighbourhood linear filter with kernel size 1 should throw exception") {
    forAll(allIntImagesTable) { image =>
      an [IllegalArgumentException] should be thrownBy {
        TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
          LinearFilter(Kernel(List(1),1,1), _.toFloat, _.toInt)) transform
      }
    }
  }

  property("Neighbourhood non-linear filter with neighbourhood size 1 should throw exception") {
    forAll(allIntImagesTable) { image =>
      an [IllegalArgumentException] should be thrownBy {
        TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
          NonLinearFilter(1, lst => lst.max)) transform
      }
    }
  }

  property("Neighbourhood linear filter with kernel size 3x3 should crop result image width and height by 2 pixels ") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        LinearFilter(Kernel(List.fill(9)(1),3,3), _.toFloat, _.toInt)) transform
      val (newLength, newWidth, newHeigh) = (newImage.matrix.length, newImage.width, newImage.height)
      val (oldLength, oldWidth, oldHeight) = (image.matrix.length, image.width, image.height)

      (newLength, newWidth, newHeigh) should be
      (oldLength - oldWidth*2 - oldHeight*2, oldWidth-2, oldHeight-2)
    }
  }

  property("Neighbourhood linear filter with kernel size 3x3, id filter: result image first row should equal " +
    "cropped original second row") {
    forAll(colourIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        LinearFilter(Kernel(List(0,0,0,0,1,0,0,0,0),3,3), _.toFloat, _.toInt)) transform
      val newFirstRow = newImage.matrix.take(newImage.width)
      val oldCroppedSecondRow = image.matrix.slice(image.width, image.width + image.width).drop(1).init
      newFirstRow should equal (oldCroppedSecondRow)
    }
  }

  property("Neighbourhood non-linear filter with neighbourhood size 3x3 should crop result image width and height " +
    "by 2 pixels ") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        NonLinearFilter(3, _ => 1)) transform
      val (newLength, newWidth, newHeigh) = (newImage.matrix.length, newImage.width, newImage.height)
      val (oldLength, oldWidth, oldHeight) = (image.matrix.length, image.width, image.height)

      (newLength, newWidth, newHeigh) should be
      (oldLength - oldWidth*2 - oldHeight*2, oldWidth-2, oldHeight-2)
    }
  }

  property("Neighbourhood non-linear filter with kernel neighbourhood 3x3, id filter: result image first row should equal " +
    "cropped original second row") {
    forAll(colourIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        NonLinearFilter(3, l => l(l.length / 2))) transform
      val newFirstRow = newImage.matrix.take(newImage.width)
      val oldCroppedSecondRow = image.matrix.slice(image.width, image.width + image.width).drop(1).init
      newFirstRow should equal (oldCroppedSecondRow)
    }
  }

  property("Neighbourhood non-linear filter with kernel neighbourhood 3x3, zero filter: result image pixel value sum " +
    "should be zero") {
    forAll(colourIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        NonLinearFilter(3, _ => 0)) transform;
      newImage.matrix.sum should equal (0)
    }
  }

  property("Neighbourhood linear filter with kernel size 9x9 should crop result image width and height by 4 pixels ") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        LinearFilter(Kernel(List.fill(81)(1),9,9), _.toFloat, _.toInt)) transform
      val (newLength, newWidth, newHeigh) = (newImage.matrix.length, newImage.width, newImage.height)
      val (oldLength, oldWidth, oldHeight) = (image.matrix.length, image.width, image.height)

      (newLength, newWidth, newHeigh) should be
      (oldLength - oldWidth*4 - oldHeight*4, oldWidth-4, oldHeight-4)
    }
  }

  property("Neighbourhood linear filter with kernel size 9x9, id filter: result image first row should equal " +
    "cropped original fifth row") {
    forAll(colourIntImagesTable) { image =>
      val kernel = List[Float](
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,1f,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0)
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        LinearFilter(Kernel(kernel,9,9), _.toFloat, _.toInt)) transform
      val newFirstRow = newImage.matrix.take(newImage.width)
      val oldCroppedSecondRow = image.matrix.slice(image.width * 4, image.width * 4 + image.width).drop(4).reverse.drop(4).reverse
      newFirstRow should equal (oldCroppedSecondRow)
    }
  }

  property("Neighbourhood non-linear filter with neighbourhood size 9x9 should crop result image width and height " +
    "by 4 pixels ") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        NonLinearFilter(9, _ => 1)) transform
      val (newLength, newWidth, newHeigh) = (newImage.matrix.length, newImage.width, newImage.height)
      val (oldLength, oldWidth, oldHeight) = (image.matrix.length, image.width, image.height)

      (newLength, newWidth, newHeigh) should be
      (oldLength - oldWidth*4 - oldHeight*4, oldWidth-4, oldHeight-4)
    }
  }

  property("Neighbourhood non-linear filter with kernel neighbourhood 9x9, id filter: result image first row should equal " +
    "cropped original fifth row") {
    forAll(colourIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        NonLinearFilter(9, l => l(l.length / 2))) transform
      val newFirstRow = newImage.matrix.take(newImage.width)
      val oldCroppedSecondRow = image.matrix.slice(image.width * 4, image.width * 4 + image.width).drop(4).reverse.drop(4).reverse
      newFirstRow should equal (oldCroppedSecondRow)
    }
  }

  property("Neighbourhood non-linear filter with kernel neighbourhood 9x9, zero filter: result image pixel value sum " +
    "should be zero") {
    forAll(colourIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        NonLinearFilter(9, _ => 0)) transform;
      newImage.matrix.sum should equal (0)
    }
  }

  property("Grey image histograms correspond to counts of distinct values in pixel arrays") {
    forAll(greyIntImagesTable) { image =>
      val manualHisto = {
        val distinct = image.matrix.distinct
        val mapPairs = distinct.map(v => (v, image.matrix.count(_ == v)))
        mapPairs.toMap
      }
      val autoHisto = TransformOneChannelToHistogram[Int](image, Histo_1ChannelTraverse()) transform;
      manualHisto should equal (autoHisto)
    }
  }

  property("Colour image histograms correspond to counts of distinct RGB values in pixel arrays") {
    forAll(colourIntImagesTable) { image =>
      val manualRed = {
        val reds = image.matrix.map(p => (p >> 16) & 0xff)
        val redPairs = reds.distinct.map(v => (v, reds.count(_ == v)))
        redPairs.toMap
      }
      val manualGreen = {
        val greens = image.matrix.map(p => (p >> 8) & 0xff)
        val greenPairs = greens.distinct.map(v => (v, greens.count(_ == v)))
        greenPairs.toMap
      }
      val manualBlue = {
        val blues = image.matrix.map(p => p & 0xff)
        val bluePairs = blues.distinct.map(v => (v, blues.count(_ == v)))
        bluePairs.toMap
      }
      val autoHisto = TransformThreeChannelToHistogram(image, Histo_3ChannelTraverse()) transform
      val autoRed = autoHisto("Red")
      val autoGreen = autoHisto("Green")
      val autoBlue = autoHisto("Blue")

      (autoRed, autoGreen, autoBlue) should equal ((manualRed, manualGreen, manualBlue))
    }
  }

}
