import images.{Image, ParImage}
import operations.{PointOp_1Channel, PointTraverse, TransformSimple}
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FunctionTests extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val imageList = (new Matrices).getIntImages
  val images = Table("images", imageList : _*)

  property("point op '+1' on all-zeroes image should give all-ones image") {
    forAll(images) { (image: Image[Int]) =>
      val newImage: ParImage[Int] = TransformSimple[Int,Int](image, PointTraverse(), PointOp_1Channel(_ + 1)) transform;
      newImage.matrix.sum should equal (image.matrix.sum + image.matrix.length)
    }
  }


}
