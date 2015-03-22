package redgear.scalajs.games

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