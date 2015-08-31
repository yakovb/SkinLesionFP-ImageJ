import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FunctionTests extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val zeros = (new Matrices).getIntImages.head
  val mats = Table(
    "images",
    zeros
  )

  property("zero image + 1 should be all 1s") {
    forAll(mats) { image =>
      image.matrix.map(_ + 1).sum should be (20 * 20)
    }
  }
}
