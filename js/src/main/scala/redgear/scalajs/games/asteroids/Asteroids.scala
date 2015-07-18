package redgear.scalajs.games.asteroids

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html._
import redgear.scalajs.games.asteroids.Engine._
import rx._

import scala.scalajs.js.annotation.JSExport
import scala.util.Random
import scalaz.{State, Lens}

/**
 * Created by LordBlackHole on 3/28/2015.
 *
 */
@JSExport
object GameAsteroids {


  val scaleX: Double = 1000
  val scaleY: Double = 600

  val width = Var(scaleX)
  val height = Var(scaleY)

  val drawScale = Rx{Point(width() / scaleX, height() / scaleY)}

  val mouseIn = Var(false)
  val mouseLoc = Var(Point(0, 0))
  val mouseScaleLoc = Rx{mouseLoc() / drawScale()}

  val randomGen = new Random()

  val keysDown = collection.mutable.Set.empty[Int]

  val currentGame = newGame

  def run(): Unit = {
    currentGame.tick(1)
  }

  def draw(drawContext: dom.CanvasRenderingContext2D): Unit = {
    drawContext.fillStyle = "black"
    drawContext.fillRect(0, 0, width(), height())
    currentGame.draw(drawContext, drawScale())
  }

  def newGame: Game = {
    val ship = Ship(Point(500, 300), 0, 0, 0)

    val asteroids = for (i <- 0 to 4) yield Asteroid(Point(randomGen.nextInt(scaleX.toInt), randomGen.nextInt(scaleY.toInt)), Point(randomGen.nextInt(4) - 2, randomGen.nextInt(4) - 2), randomGen.nextInt(20) + 8)

    GameBuider(ship :: asteroids.toList,
      ShipBehavior :: AsteroidBehavior :: Nil,
      ShipArtist :: AsteroidArtist :: Nil
    ).buildGame
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



case class Ship(location: Point, direction: Double, speed: Double, fireCountDown: Int) extends Entity {

  val isSolid = true
}

object ShipBehavior extends Behavior {

  override def update = State.modify{ world =>
    World.lensEntities.modify(_.collect {
      case me: Ship => move(me)
      case e: Entity => e
    })(world)
  }

  //TODO: Actual movement code
  private def move(input: Ship): Ship = input
  
  
}

object ShipArtist extends Artist {
  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    drawContext.fillStyle = "white"

    for (Ship(Point(x, y), direct, _, _) <- world.entities){
      //TODO: Actual draw code
      drawContext.fillRect(x * scale.x, y * scale.y, 10 * scale.x, 10 * scale.y)
    }
  }
}

case class Asteroid(location: Point, direction: Point, size: Double) extends Entity {

  val isSolid = true
}

object AsteroidBehavior extends Behavior {

  override def update = State.modify{world =>
    World.lensEntities.modify(_.collect {
      case me: Asteroid => move(me)
      case e: Entity => e
    })(world)
  }

  private def move(input: Asteroid): Asteroid = {
    input.copy(location = (input.location + input.direction match {//Check the x axis
      case Point(x, y) if x > GameAsteroids.scaleX => Point(x - GameAsteroids.scaleX, y)
      case Point(x, y) if x < 0 => Point(x + GameAsteroids.scaleX, y)
      case p: Point => p
    }) match { //Then check the y axis
      case Point(x, y) if y > GameAsteroids.scaleY => Point(x, y - GameAsteroids.scaleY)
      case Point(x, y) if y < 0 => Point(x, y + GameAsteroids.scaleY)
      case p: Point => p
    })
  }
}

object AsteroidArtist extends Artist {

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    drawContext.fillStyle = "white"

    for (Asteroid(Point(x, y), _, size) <- world.entities){
      drawContext.beginPath()
      drawContext.arc(x * scale.x, y * scale.y, size * scale.x, 0, scala.scalajs.js.Math.PI * 2)
      drawContext.closePath()
      drawContext.fill()
    }
  }

}
