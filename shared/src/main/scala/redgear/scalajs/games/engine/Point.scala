package redgear.scalajs.games.engine

/**
 * Created by LordBlackHole on 3/19/2015.
 *
 */
case class Point(x: Double, y: Double) {
  def +(p: Point) = Point(x + p.x, y + p.y)

  def -(p: Point) = Point(x - p.x, y - p.y)

  def *(d: Point) = Point(x * d.x, y * d.y)

  def *(d: Double) = Point(x * d, y * d)

  def /(d: Point) = Point(x / d.x, y / d.y)

  def /(d: Double) = Point(x / d, y / d)

  def %(d: Point) = Point(x % d.x, y % d.y)

  def <(other: Point) = x < other.x && y < other.y

  def >(other: Point) = x > other.x && y > other.y

  def length = math.sqrt(x * x + y * y)

  def mirrorX = copy(x = -x)

  def mirrorY = copy(y = -y)

  def within(a: Point, b: Point, extra: Point = Point(0, 0)) = {
    x >= math.min(a.x, b.x) - extra.x &&
      x < math.max(a.x, b.x) + extra.y &&
      y >= math.min(a.y, b.y) - extra.x &&
      y < math.max(a.y, b.y) + extra.y
  }

  def rotate(theta: Double) = {
    val (cos, sin) = (math.cos(theta), math.sin(theta))
    Point(cos * x - sin * y, sin * x + cos * y)
  }

  def distanceTo(other: Point) = Math.sqrt((other.x - x) * (other.x - x) + (other.y - y) * (other.y - y))


}

object Point {

  implicit def toPoint(pair: (Double, Double)): Point = Point(pair._1, pair._2)

}