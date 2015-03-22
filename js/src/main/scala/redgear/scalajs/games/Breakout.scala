package redgear.scalajs.games

import org.scalajs.dom
import rx._

import scala.annotation.tailrec
import scala.scalajs.js.annotation.JSExport
import scala.util.Random
import scalatags.JsDom.all._
import org.scalajs.dom.html._



@JSExport
object GameBreakout extends ClientUtils{
  
  val scaleX: Double = 1000
  val scaleY: Double = 600

  val width = Var(scaleX)
  val height = Var(scaleY)

  val drawScale = Rx{Point(width() / scaleX, height() / scaleY)}

  val randomGen = new Random()
  val blockColors = List("red", "blue", "green", "yellow", "purple", "cyan", "lime")
  val keysDown = collection.mutable.Set.empty[Int]


  var gameObjects = new Paddle(Point(50, 550)) :: new Ball(Point(50, 530), Point(3, -4)) :: createBlocks

  def run(): Unit = {
    gameObjects = gameObjects.flatMap{obj => obj.step}
  }

  def draw(drawContext: dom.CanvasRenderingContext2D): Unit = {
    drawContext.fillStyle = "black"
    drawContext.fillRect(0, 0, width(), height())

    gameObjects.foreach{obj => obj.draw(drawContext, drawScale())}
  }

  def createBlocks: List[Entity] = {
    (
      for{ x <- 5 to 1000 by 50
           y <- 5 to 200 by 20
      } yield Block(Point(x, y), blockColors(randomGen.nextInt(blockColors.size)))
      ).toList
  }

  @JSExport
  /** The game window is where all objects are rendered **/
  def main(window: Canvas): Unit = {

    /** Drawing context for the window **/
    val drawContext = window.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    val resize = () => {width() = dom.innerWidth - 20; height() = dom.innerHeight - 20}

    Obs(width){window.width = width().toInt}
    Obs(height){window.height = height().toInt}

    dom.document.body.appendChild(window)

    dom.onkeydown = (e: dom.KeyboardEvent) => keysDown.add(e.keyCode)
    dom.onkeyup   = (e: dom.KeyboardEvent) => keysDown.remove(e.keyCode)
    dom.onresize = (e: dom.UIEvent) => {resize()}

    resize()

    dom.setInterval(() => {run(); draw(drawContext)}, 20)
  }
}

case class Paddle(location: Point) extends Box{

  import redgear.scalajs.games.GameBreakout._

  val size = Point(80,10)
  val color = "red"

  val speed = 4

  def step: List[Entity] = {

    def left = if(location.x - speed > 0) Point(-speed, 0) else Point(0, 0)
    def right = if(location.x + speed < scaleX - size.x) Point(speed, 0) else Point(0, 0)


    (
      if(GameBreakout.keysDown(65)) Paddle(location + left) else //A
      if(GameBreakout.keysDown(37)) Paddle(location + left) else //left arrow
      if(GameBreakout.keysDown(68)) Paddle(location + right) else //D
      if(GameBreakout.keysDown(39)) Paddle(location + right) else //right arrow
        this //No keys were pressed. Don't move.
      ) :: Nil
  }
}

case class Ball(location: Point, velocity: Point) extends Box {

  val size = Point(10,10)
  val color = "red"

  def step: List[Entity] = {

    val next = copy(location = location + velocity)

    def checkWalls =  checkX || checkY

    def checkX = next.location.x < 0 || next.location.x + width > GameBreakout.scaleX

    def checkY = next.location.y < 0 || next.location.y + height > GameBreakout.scaleY

    @tailrec
    def collideRecurse(entities: List[Entity]): List[Entity] = {
      entities match {
        case ::(head: Block, tail) => if(next | head) calcBlockCollision(head) :: Nil else collideRecurse(tail)
        case ::(head: Paddle, tail) => if(next | head) calcPaddleCollision(head) :: Nil else collideRecurse(tail)
        case _ :: tail => collideRecurse(tail)
        case Nil => next :: Nil
      }
    }

    def bounce(x: Boolean, y: Boolean): Ball = {
      val horizontal = if(x) velocity.mirrorX else velocity
      val vertical = if(y) horizontal.mirrorY else horizontal

      next.copy(velocity = vertical)
    }

    def calcBlockCollision(b: Block): Ball = {
      bounce(horizontal(b.copy(b.location.copy(y = 0))), vertical(b.copy(b.location.copy(x = 0))))
    }

    /**The paddle change the reflect velocity based on position, and hence is different than a block collision **/
    def calcPaddleCollision(b: Paddle): Ball = {
      val xDiff = center.x - b.center.x
      val fraction = xDiff / (size.x + b.size.x)
      val speed = velocity.x.abs + velocity.y.abs
      val xSpeed = speed * fraction
      val ySpeed = -(speed - xSpeed.abs)

      //dom.console.log(s"Diff: $xDiff, Fraction: $fraction, Speed: $speed, Velocity: ($xSpeed, $ySpeed)")
      next.copy(velocity = Point(xSpeed, ySpeed))
    }

    if(checkWalls)
      bounce(checkX, checkY) :: Nil
    else
      collideRecurse(GameBreakout.gameObjects)
  }
}

case class Block(location: Point, color: String) extends Box {

  val size = Point(40, 15)


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





