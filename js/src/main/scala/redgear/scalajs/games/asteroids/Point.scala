package redgear.scalajs.games.asteroids

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

}

object Point {

  implicit def toPoint(pair: (Double, Double)): Point = Point(pair._1, pair._2)

}

trait Shape {

  def location: Point

  //def intersect(other: Shape): Boolean

}


trait Box extends Shape {

  def location: Point

  def size: Point

  def left = location.x

  def top = location.y

  def right = left + size.x

  def bottom = top + size.y

  def width = size.x

  def height = size.y

  def center = location + (size / 2.0)


  def intersects(other: Box): Boolean = horizontal(other) && vertical(other)

  def horizontal(other: Box): Boolean = Math.min(right, other.right) - Math.max(left, other.left) > 0

  def vertical(other: Box): Boolean = Math.min(bottom, other.bottom) - Math.max(top, other.top) > 0

  def contains(point: Point): Boolean = left <= point.x && top <= point.y && right >= point.x && bottom >= point.y

}