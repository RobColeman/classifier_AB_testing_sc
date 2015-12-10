package helpers


object math {
  def squared(x: Double): Double = scala.math.pow(x, 2)
  def round(x: Double, prec: Int): Double = {
    val scale: Double = scala.math.pow(10,prec)
    scala.math.round(x * scale) / scale
  }
}
