import images.{Image, ParImage}
import operations.{PointOp_1Channel, PointOp_3Channel, PointTraverse, TransformSimple}
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FunctionTests extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val imageList = (new Matrices).getIntImages
  val images = Table("images", imageList : _*)

  property("point op '+1' should result in sum of array + length of array") {
    forAll(images) { (image: Image[Int]) =>
      val newImage: ParImage[Int] = TransformSimple[Int,Int](image, PointTraverse(), PointOp_1Channel(_ + 1)) transform;
      newImage.matrix.sum should equal (image.matrix.sum + image.matrix.length)
    }
  }

  property("point op 3-channel (where the channel is split up and recomposed) should result in original image") {
    forAll(images) { image =>
      val newImage = TransformSimple[Int,Int](image, PointTraverse(), PointOp_3Channel(
        r => 1,
        g => 1,
        b => 1)
        ((r:Int,g:Int,b:Int) => (r + g + b).toByte)) transform;
      newImage.matrix should equal (Array.fill(400)(3))
    }
  }

}
