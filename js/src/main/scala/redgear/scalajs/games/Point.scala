package redgear.scalajs.games

/**
 * Created by Jett on 3/19/2015.
 */
case class Point(x: Double, y: Double) {
   def +(p: Point) = Point(x + p.x, y + p.y)
   def -(p: Point) = Point(x - p.x, y - p.y)
   def /(d: Double) = Point(x / d, y / d)
   def *(d: Double) = Point(x * d, y * d)
   def length = Math.sqrt(x * x + y * y)

   def mirrorX = copy(x = -x)
   def mirrorY = copy(y = -y)
 }