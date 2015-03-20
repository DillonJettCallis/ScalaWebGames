package redgear.scalajs.games

import org.scalajs.dom

import scala.annotation.tailrec
import scala.scalajs.js.annotation.JSExport
import scala.util.Random
import scalatags.JsDom.all._



@JSExport
object GameBreakout extends ClientUtils{

  /** The game window, where all objects are rendered **/
  val window = canvas.render
  /** Drawing context for the window **/
  val drawContext = window.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  window.width = 1500
  window.height = 600

  dom.document.body.appendChild(window)

  val randomGen = new Random()
  val blockColors = List("red", "blue", "green", "yellow")
  val keysDown = collection.mutable.Set.empty[Int]


  var gameObjects = new Paddle(Point(50, window.height - 100)) :: new Ball(Point(50, window.height - 120), Point(3, -4)) :: createBlocks

  def run(): Unit = {
    gameObjects = gameObjects.flatMap{obj => obj.step}
  }

  def draw(): Unit = {
    drawContext.fillStyle = "black"
    drawContext.fillRect(0, 0, window.width, window.height)

    gameObjects.foreach{obj => obj.draw(drawContext)}
  }

  def createBlocks: List[Entity] = {
    (
      for{ x <- 5 to 500 by 50
           y <- 5 to 200 by 20
      } yield Block(Point(x, y), blockColors(randomGen.nextInt(blockColors.size)))
      ).toList
  }

  @JSExport
  def main(): Unit = {

    dom.onkeydown = (e: dom.KeyboardEvent) => keysDown.add(e.keyCode)
    dom.onkeyup   = (e: dom.KeyboardEvent) => keysDown.remove(e.keyCode)

    dom.setInterval(() => {run(); draw()}, 20)
  }
}


case class Paddle(location: Point) extends Box{

  val size = Point(80,10)
  val color = "red"

  val speed = 4

  def step: List[Entity] = {
    (
      if(GameBreakout.keysDown(65)) Paddle(location - Point(speed, 0)) else //A
      if(GameBreakout.keysDown(37)) Paddle(location - Point(speed, 0)) else //left arrow
      if(GameBreakout.keysDown(68)) Paddle(location - Point(-speed, 0)) else //D
      if(GameBreakout.keysDown(39)) Paddle(location - Point(-speed, 0)) else //right arrow
        this //No keys were pressed. Don't move.
      ) :: Nil
  }
}

case class Ball(location: Point, velocity: Point) extends Box {

  val size = Point(10,10)
  val color = "red"

  def step: List[Entity] = {

    val next = location + velocity

    def checkWalls: Boolean = next.x < 0 || next.x > GameBreakout.window.width || next.y < 0 || next.y > GameBreakout.window.height

    @tailrec
    def collideRecurse(entities: List[Entity]): List[Entity] = {
      entities match {
        case ::(head: Block, tail) => if(this | head) calcBlockCollision(head) :: Nil else collideRecurse(tail)
        case ::(head: Paddle, tail) => if(this | head) calcPaddleCollision(head) :: Nil else collideRecurse(tail)
        case _ :: tail => collideRecurse(tail)
        case Nil => copy(location = next) :: Nil
      }
    }

    def bounce(x: Boolean, y: Boolean): Ball = {
      val horizontal = if(x) velocity.mirrorX else velocity
      val vertical = if(y) horizontal.mirrorY else horizontal

      Ball(next, vertical)
    }

    def calcBlockCollision(b: Box): Ball = bounce(horizontal(b), vertical(b))

    /**The paddle change the reflect velocity based on position, and hence is different than a block collision **/
    def calcPaddleCollision(b: Paddle): Ball = {
      val xDiff = center.x - b.center.x
      val fraction = xDiff / (size.x + b.size.x)
      val speed = velocity.x.abs + velocity.y.abs
      val xSpeed = speed * fraction
      val ySpeed = -(speed - xSpeed.abs)

      //dom.console.log(s"Diff: $xDiff, Fraction: $fraction, Speed: $speed, Velocity: ($xSpeed, $ySpeed)")
      copy(next, Point(xSpeed, ySpeed))
    }

    if(checkWalls)
      bounce(next.x < 0 || next.x > GameBreakout.window.width, next.y < 0 || next.y > GameBreakout.window.height) :: Nil
    else
      collideRecurse(GameBreakout.gameObjects)
  }
}

case class Block(location: Point, color: String) extends Box {

  val size = Point(20, 10)


  def step: List[Entity] = {
    @tailrec
    def collideRecurse(entities: List[Entity]): List[Entity] = {
      entities match {
        case ::(head: Ball, tail) => if(this | head) Nil else collideRecurse(tail)
        case _ :: tail => collideRecurse(tail)
        case Nil => this :: Nil
      }
    }
    collideRecurse(GameBreakout.gameObjects)
  }

}





