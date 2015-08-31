//package operations
//
//object MyPipeline {
//  def getRed = PointOp_1Channel((pixel: Int) => (pixel >> 16) & 0xff)
//
//  def getGreen = PointOp_1Channel((pixel: Int) => (pixel >> 8) & 0xff)
//
//  def getBlue = PointOp_1Channel((pixel: Int) => pixel & 0xff)
//
//  def transformToThreeChannel = TransformSimple(_, PointTraverse(), getRed,getGreen,getBlue)
//}
