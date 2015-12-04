package models

object AUC {

  private def trapezoid(points: Seq[(Double, Double)]): Double = {
    require(points.length == 2)
    val x = points.head
    val y = points.last
    (y._1 - x._1) * (y._2 + x._2) / 2.0
  }

  /* spark
  def apply(curve: RDD[(Double, Double)]): Double = {
    curve.sliding(2).aggregate(0.0)(
      seqOp = (auc: Double, points: Array[(Double, Double)]) => auc + trapezoid(points),
      combOp = _ + _
    )
  }
  */

  def apply(curve: Iterable[(Double, Double)]): Double = {
    curve.toIterator.sliding(2).withPartial(false).aggregate(0.0)(
      seqop = (auc: Double, points: Seq[(Double, Double)]) => auc + trapezoid(points),
      combop = _ + _
    )
  }
}