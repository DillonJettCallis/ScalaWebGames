package redgear.scalajs.games.asteroids

/**
 * Created by LordBlackHole on 3/19/2015.
 */
case class Point(x: Double, y: Double) {
   def +(p: Point) = Point(x + p.x, y + p.y)
   def -(p: Point) = Point(x - p.x, y - p.y)
   def /(d: Point) = Point(x / d.x, y / d.y)
   def *(d: Point) = Point(x * d.x, y * d.y)
   def length = Math.sqrt(x * x + y * y)

   def mirrorX = copy(x = -x)
   def mirrorY = copy(y = -y)

}

object Point {

  implicit def toPoint(d: Double): Point = Point(d, d)

}

trait Shape{

  def location: Point

  //def intersect(other: Shape): Boolean

}


case class Box(location: Point, size: Point) extends Shape {

  def left = location.x
  def top = location.y
  def right = left + size.x
  def bottom = top + size.y

  def width = size.x
  def height = size.y
  def center = location + (size / 2.0)


  def intersects(other: Box): Boolean = horizontal(other) && vertical(other)

  def horizontal(other: Box): Boolean = Math.min(right,other.right) - Math.max(left,other.left) > 0

  def vertical(other: Box): Boolean = Math.min(bottom,other.bottom) - Math.max(top,other.top) > 0

  def contains(point: Point): Boolean = left <= point.x && top <= point.y &&  right >= point.x && bottom >= point.y

}