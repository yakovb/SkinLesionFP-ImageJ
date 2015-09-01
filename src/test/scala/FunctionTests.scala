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
          NonLinearFilterNoKernel(0, lst => lst.max)) transform
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
          NonLinearFilterNoKernel(1, lst => lst.max)) transform
      }
    }
  }

  property("Neighbourhood linear filter with kernel size 3 should crop result image width and height by 2 pixels ") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        LinearFilter(Kernel(List.fill(9)(1),3,3), _.toFloat, _.toInt)) transform
      val (newLength, newWidth, newHeigh) = (newImage.matrix.length, newImage.width, newImage.height)
      val (oldLength, oldWidth, oldHeight) = (image.matrix.length, image.width, image.height)

      (newLength, newWidth, newHeigh) should be
      (oldLength - oldWidth*2 - oldHeight*2, oldWidth-2, oldHeight-2)
    }
  }

  property("Neighbourhood linear filter with kernel size 3, id filter: result image first row should equal " +
    "cropped original second row") {
    forAll(colourIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        LinearFilter(Kernel(List(0,0,0,0,1,0,0,0,0),3,3), _.toFloat, _.toInt)) transform
      val newFirstRow = newImage.matrix.take(newImage.width)
      val oldCroppedSecondRow = image.matrix.slice(image.width, image.width + image.width).drop(1).init
      newFirstRow should equal (oldCroppedSecondRow)
    }
  }

  property("Neighbourhood non-linear filter with neighbourhood size 3 should crop result image width and height by 2 pixels ") {
    forAll(allIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        NonLinearFilterNoKernel(3, _ => 1)) transform
      val (newLength, newWidth, newHeigh) = (newImage.matrix.length, newImage.width, newImage.height)
      val (oldLength, oldWidth, oldHeight) = (image.matrix.length, image.width, image.height)

      (newLength, newWidth, newHeigh) should be
      (oldLength - oldWidth*2 - oldHeight*2, oldWidth-2, oldHeight-2)
    }
  }

  property("Neighbourhood non-linear filter with kernel neighbourhood 3, id filter: result image first row should equal " +
    "cropped original second row") {
    forAll(colourIntImagesTable) { image =>
      val newImage = TransformNeighbourhood[Int,Int](image, NeighbourTraverse(),
        NonLinearFilterNoKernel(3, l => l(l.length / 2))) transform
      val newFirstRow = newImage.matrix.take(newImage.width)
      val oldCroppedSecondRow = image.matrix.slice(image.width, image.width + image.width).drop(1).init
      newFirstRow should equal (oldCroppedSecondRow)
    }
  }

  //TODO nhood transform - nonlin filter - hood size 3x3: zero filter correct

  //TODO nhood transform - linear filter - kernel size 5x5: 2 pix crop; id filter correct
  //TODO nhood transform - nonlin filter - hood size 5x5: 2 pix crop; id filter correct; zero filter correct

  //TODO nhood transform - linear filter - kernel size 9x9: 4 pix crop; id filter correct
  //TODO nhood transform - nonlin filter - hood size 9x9: 4 pix crop; id filter correct; zero filter correct

  //TODO 1 channel histo - only for grey images: ones; rand; zeros; lena
  //TODO 3 channel histo - only for Int colour images: all red; all green; all blue; random

}
