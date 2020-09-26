import scala.math.abs

object week2 {
  def isgood(x:Double,y:Double):Boolean =
    abs((x - y)/x)/x < 0.001

  def fixedPoint(f: Double => Double)(ini:Double):Double ={
    val next = f(ini)
    if (isgood(ini, next)) next
    else fixedPoint(f)(next)
  }
  def sqrt(k:Int) = fixedPoint(x => k / x)(1)
}
