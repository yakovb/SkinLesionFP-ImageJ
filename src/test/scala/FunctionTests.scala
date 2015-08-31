import operations._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FunctionTests extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val imageList = (new Matrices).getIntImages
  val images = Table("images", imageList : _*)

  property("PointTraverse traverse with 1-channel op (x + 1) should result in sum of array + length of array") {
    forAll(images) { image =>
      val newImage = TransformSimple[Int,Int](image, PointTraverse(), PointOp_1Channel(_ + 1)) transform;
      newImage.matrix.sum should equal (image.matrix.sum + image.matrix.length)
    }
  }

  property("PointTraverse traverse with 3-channel op (rgb => 3) should result in image of all 3's") {
    forAll(images) { image =>
      val newImage = TransformSimple[Int,Int](image, PointTraverse(), PointOp_3Channel(
        r => 1,
        g => 1,
        b => 1)
        ((r:Int,g:Int,b:Int) => (r + g + b).toByte)) transform;
      newImage.matrix should equal (Array.fill(newImage.width * newImage.height)(3))
    }
  }

  property("PointTraverse traverseAndExpand with 1-channel op (x => x x x) should result in new image length of original * 3") {
    forAll(images) { image =>
      val newImage = TransformSimple[Int,Int](image, PointTraverse(),
        PointOp_1Channel((x:Int) => x),
        PointOp_1Channel((x:Int) => x),
        PointOp_1Channel((x:Int) => x)) transform;
      newImage.matrix.length should equal (image.matrix.length * 3)
    }
  }

  property("PointTraverse traverseAndExpand with 1-channel op (x => x x x) should result in summed image of 3 * original") {
    forAll(images) { image =>
      val newImage = TransformSimple[Int,Int](image, PointTraverse(),
        PointOp_1Channel((x:Int) => x),
        PointOp_1Channel((x:Int) => x),
        PointOp_1Channel((x:Int) => x)) transform;
      newImage.matrix.sum should be (image.matrix.sum * 3)
    }
  }

  property("BlockTraverse traverse with ops (x => 1, y => 0) should give image of same pixel array length") {
    forAll(images) { image =>
      val newImage = TransformBlock[Int,Int](image, BlockTraverse(),
        PointOp_1Channel(x => 1),
        PointOp_1Channel(x => 0)) transform;
      newImage.matrix.length should be (image.matrix.length)
    }
  }

  property("BlockTraverse traverse with ops (x => 1, y => 0) should give summed image of array.length / 2") {
    forAll(images) { image =>
      val newImage = TransformBlock[Int,Int](image, BlockTraverse(),
        PointOp_1Channel(x => 1),
        PointOp_1Channel(x => 0)) transform;
      newImage.matrix.sum.toFloat should be (image.matrix.length / 2.0)
    }
  }

  property("BlockTraverse traverseAndReduce with ops ((x => x, y => y) => x + y) should give image with halved array length") {
    forAll(images) { image =>
      val newImage = TransformBlockReduce[Int,Int,Int](image, BlockTraverse(),(list: Seq[Int]) => list.sum,
        PointOp_1Channel(x => x),
        PointOp_1Channel(y => y)) transform;
      newImage.matrix.length should be (image.matrix.length / 2)
    }
  }

  property("BlockTraverse traverseAndReduce with ops ((x => x, y => y) => x + y) should give image with same array sum") {
    forAll(images) { image =>
      val newImage = TransformBlockReduce[Int,Int,Int](image, BlockTraverse(),(list: Seq[Int]) => list.sum,
        PointOp_1Channel(x => x),
        PointOp_1Channel(y => y)) transform;
      newImage.matrix.sum should be (image.matrix.sum)
    }

  }

}
