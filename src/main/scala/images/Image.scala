package images

import scala.collection.parallel.mutable.ParArray

sealed trait Image {
  val width: Int
  val height: Int
  val matrix: ParArray[Int]
}
