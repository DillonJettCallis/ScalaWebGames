package redgear.scalajs.games.engine

/**
 * Created by LordBlackHole on 11/8/2015.
 */
object CollisionDetector {
  
  
  sealed trait Shape {

    /**
     * @return an approximation of this shape using a perfect bounding box. Used to run first level checks.
     */
    def box: Box

    def isInside(other: Point): Boolean
    
  }

  trait Polygon { self: Shape =>

    def points: Stream[Point]

  }

  /** Box is a perfectly level rectangle. It's lines are parallel to the screen, unlike a quad which could be rotated.
    *
    * @param min Minimum x and y point
    * @param max Maximum x and y point
    */
  case class Box(min: Point, max: Point) extends Shape with Polygon {

    val box = this

    val points = Stream(min, max, Point(min.x, max.y), Point(max.x, min.y))

    def left = min.x
    def top = min.y
    def right = max.x
    def bottom = max.y

    def width = max.x - min.x
    def height = max.y - min.y

    def location = min
    def size = Point(width, height)
    def center = min + (size / 2.0)

    def isInside(p: Point): Boolean = min < p && max > p

    def |(other: Box): Boolean = horizontal(other) && vertical(other)

    def horizontal(other: Box): Boolean = Math.min(right,other.right) - Math.max(left,other.left) > 0

    def vertical(other: Box): Boolean = Math.min(bottom,other.bottom) - Math.max(top,other.top) > 0

  }
  
  case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape with Polygon{

    val points = Stream(p1, p2, p3)

    val box = {
      val x = points.map(_.x)
      val y = points.map(_.y)

      Box(Point(x.min, y.min), Point(x.max, y.max))
    }

    /**
     * Checks to see if point p is inside this triangle
     * via the Barycentric method.
     */
    def isInside(p: Point): Boolean = {
      val alpha = ((p2.y - p3.y)*(p.x - p3.x) + (p3.x - p2.x)*(p.y - p3.y)) /
        ((p2.y - p3.y)*(p1.x - p3.x) + (p3.x - p2.x)*(p1.y - p3.y))
      val beta = ((p3.y - p1.y)*(p.x - p3.x) + (p1.x - p3.x)*(p.y - p3.y)) /
        ((p2.y - p3.y)*(p1.x - p3.x) + (p3.x - p2.x)*(p1.y - p3.y))
      val gamma = 1.0f - alpha - beta

      alpha > 0 && beta > 0 && gamma > 0
    }

  }
  
  case class Circle(location: Point, radius: Double) extends Shape {

    val box = {
      val rad = Point(radius, radius)

      Box(location - rad, location + rad)
    }

    def isInside(other: Point): Boolean = location.distanceTo(other) <= radius
    
  }

  case class Arbitrary(shapes: Seq[Shape]) extends Shape {

    val box = {
      val boxes = shapes.map(_.box)

      val min = boxes.map(_.min)
      val max = boxes.map(_.max)

      Box(Point(min.map(_.x).min, min.map(_.y).min), Point(max.map(_.x).max, max.map(_.y).max))
    }

    def isInside(other: Point): Boolean = shapes.toStream.exists(_.isInside(other))

  }
  
  def isColliding(first: Shape, second: Shape): Boolean = {
    val box1 = first.box
    val box2 = second.box

    val isPossibleCollide = Math.min(box1.right, box2.right) - Math.max(box1.left, box2.left) > 0  && Math.min(box1.bottom, box2.bottom) - Math.max(box1.top, box2.top) > 0

    isPossibleCollide && {
      (first, second) match {
        case (first: Box, second: Box) => true //Since possible collide uses boxes, these ones are perfect.
        case (first: Polygon, second: Polygon) => first.points.exists(second.isInside) || second.points.exists(first.isInside)

        case (first: Circle, second: Circle) => first.location.distanceTo(second.location) < first.radius + second.radius

        case (first: Circle, second: Polygon) => second.isInside(first.location) || second.points.exists(first.isInside)
        case (first: Polygon, second: Circle) => first.isInside(second.location) || first.points.exists(second.isInside)

        case (first: Arbitrary, second: Shape) => first.shapes.exists(isColliding(_, second))
        case (first: Shape, second: Arbitrary) => second.shapes.exists(isColliding(_, first))
        case _ => false
      }
    }
  }
  
  
  
  
  

}
