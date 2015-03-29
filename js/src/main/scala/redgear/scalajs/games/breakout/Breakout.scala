package redgear.scalajs.games.breakout

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html._
import rx._

import scala.annotation.tailrec
import scala.scalajs.js.annotation.JSExport
import scala.util.Random



@JSExport
object GameBreakout {
  
  val scaleX: Double = 1000
  val scaleY: Double = 600

  val width = Var(scaleX)
  val height = Var(scaleY)

  val drawScale = Rx{Point(width() / scaleX, height() / scaleY)}

  val mouseIn = Var(false)
  val mouseLoc = Var(Point(0, 0))
  val mouseScaleLoc = Rx{mouseLoc() / drawScale()}

  val randomGen = new Random()
  val blockColors = List("red", "blue", "green", "yellow", "purple", "cyan", "lime")
  val keysDown = collection.mutable.Set.empty[Int]


  var gameObjects = Score :: new Paddle(Point(50, 550)) :: new Ball(Point(50, 530), Point(3, -4)) :: Ball(Point(40, 530), Point(-4, -3)) :: createBlocks

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
    dom.onresize = (e: dom.UIEvent) => resize()

    dom.onmouseover = (e: dom.MouseEvent) => mouseIn() = true
    dom.onmouseout = (e: dom.MouseEvent) => mouseIn() = false

    dom.onmousemove = (e: dom.MouseEvent) => mouseLoc() = Point(e.pageX, e.pageY)

    resize()

    dom.setInterval(() => {run(); draw(drawContext)}, 20)
  }
}

case class Paddle(location: Point) extends Box{

  val size = Point(80,10)
  val color = "red"

  val speed = 4

  def step: List[Entity] = {

    if(mouseIn()){
      val temp = mouseScaleLoc().x - size.x / 2

      val x = if(temp > scaleX - size.x) scaleX - size.x else if(temp < 0) 0 else temp

      Paddle(location.copy(x = x))
    }
    else {

      def left = if (location.x - speed > 0) Point(-speed, 0) else Point(0, 0)
      def right = if (location.x + speed < scaleX - size.x) Point(speed, 0) else Point(0, 0)

      if (GameBreakout.keysDown(65)) Paddle(location + left)
      else //A
      if (GameBreakout.keysDown(37)) Paddle(location + left)
      else //left arrow
      if (GameBreakout.keysDown(68)) Paddle(location + right)
      else //D
      if (GameBreakout.keysDown(39)) Paddle(location + right)
      else //right arrow
        this //No keys were pressed. Don't move.

    }
  }
}

case class Ball(location: Point, velocity: Point) extends Box {

  val size = Point(10,10)
  val color = "red"

  def step: List[Entity] = {

    val next = copy(location = location + velocity)

    def checkWalls =  checkX || checkY

    def checkX = next.location.x < 0 || next.location.x + width > GameBreakout.scaleX

    def checkY = checkBottom || checkTop

    def checkBottom = next.location.y + height > GameBreakout.scaleY

    def checkTop = next.location.y < 0

    @tailrec
    def collideRecurse(entities: List[Entity]): List[Entity] = {
      entities match {
        case ::(head: Block, tail) => if(next | head) calcBlockCollision(head) else collideRecurse(tail)
        case ::(head: Paddle, tail) => if(next | head) calcPaddleCollision(head) else collideRecurse(tail)
        case _ :: tail => collideRecurse(tail)
        case Nil => next
      }
    }

    def bounce(x: Boolean, y: Boolean): Ball = {
      val horizontal = if(x) velocity.mirrorX else velocity
      val vertical = if(y) horizontal.mirrorY else horizontal

      if(checkBottom)
        Score.resetScore()

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

      Score.resetCombo()
      //dom.console.log(s"Diff: $xDiff, Fraction: $fraction, Speed: $speed, Velocity: ($xSpeed, $ySpeed)")
      next.copy(velocity = Point(xSpeed, ySpeed))
    }

    if(checkWalls)
      bounce(checkX, checkY)
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
        case ::(head: Ball, tail) => if(this | head) {Score.update(1); Nil} else collideRecurse(tail)
        case _ :: tail => collideRecurse(tail)
        case Nil => this
      }
    }
    collideRecurse(GameBreakout.gameObjects)
  }

}

object Score extends Entity {

  var highScore = 0
  var score = 0
  var combo = 1
  val location = Point(850, 500)

  def update(add: Int) = {
    score = score + add * combo
    if(combo < 10)
      combo += 1
  }

  def resetCombo(): Unit = {
    combo = 1
  }

  def resetScore(): Unit = {
    if(score > highScore)
      highScore = score
    score = 0
    combo = 0
  }

  override def step: List[Entity] = this

  val max = 200
  val font = "px Georgia"

  override def draw(drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    val relative = location * scale
    drawContext.fillStyle = "blue"

    val text = s"${if (highScore > 0) s"High Score: $highScore, " else ""}Score: ${score.toString}"
    val size = 10
    drawContext.font = s"$size$font"
    val textScale = drawContext.measureText(text).width

    drawContext.font = s"${size * ((max / textScale) - 0.1) }$font"
    drawContext.fillText(text, relative.x, relative.y)
  }

}





