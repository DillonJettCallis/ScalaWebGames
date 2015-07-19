package redgear.scalajs.games.asteroids

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html._
import redgear.scalajs.games.asteroids.Engine._
import rx._

import scala.scalajs.js
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

  val randomGen = new Random()

  @JSExport
  /** The game window is where all objects are rendered **/
  def main(window: Canvas): Unit = {
    val ship = Ship(Point(500, 300), 0, 0, 0)

    val asteroids = for (i <- 0 to 4)
      yield Asteroid(Point(randomGen.nextInt(scaleX.toInt), randomGen.nextInt(scaleY.toInt)), Point(randomGen.nextInt(4) - 2, randomGen.nextInt(4) - 2), randomGen.nextInt(20) + 8)

    GameBuider(
      startingEntities =  ship :: asteroids.toList,
      behaviors =         ShipBehavior :: AsteroidBehavior :: Nil,
      artists =           ShipArtist :: AsteroidArtist :: Nil,
      eventHandlers =     ShipControlHandler :: Nil
    ).buildGame(
        window =          window,
        scale =           Point(scaleX, scaleY),
        backgroundColor = "black")
  }
}



case class Ship(location: Point, direction: Double, speed: Double, fireCountDown: Int) extends Entity

object ShipBehavior extends Behavior {

  override def update = State.modify{ world =>
    World.lensEntities.modify(_.collect {
      case me: Ship => move(me)
      case e: Entity => e
    })(world)
  }

  //TODO: Actual movement code
  private def move(input: Ship): Ship = {



    input
  }
}

object ShipArtist extends Artist {

  val h = 30
  val w = 20

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    drawContext.fillStyle = "white"

    for (Ship(loc, direct, _, _) <- world.entities){
      val translated = loc * scale
      drawContext.save()
      drawContext.translate(translated.x, translated.y)
      drawContext.beginPath()
      drawContext.rotate(direct * js.Math.PI / 180)
      drawContext.beginPath()
      drawContext.moveTo(0, -h / 2)
      drawContext.lineTo(w / 2, h / 2)
      drawContext.lineTo(-w / 2, h / 2)
      drawContext.lineTo(0, -h / 2)
      drawContext.fill()
      drawContext.closePath()
      drawContext.restore()
    }
  }
}

object ShipControlHandler extends EventHandler {

  val turnArc = 10

  override def react(event: Event): WorldState[Unit] = State.modify{ world =>
    event match {
      case KeyPressEvent(65) => turn(world, -turnArc) //A
      case KeyPressEvent(37) => turn(world, -turnArc) //Left Arrow
      case KeyPressEvent(68) => turn(world, turnArc) //D
      case KeyPressEvent(39) => turn(world, turnArc) //Right Arrow
      case other: Event => world
    }
  }

  def turn(world: World, turnArc: Int): World = {
    World.lensEntities.modify(_.collect{
      case ship: Ship => ship.copy(direction = ship.direction + turnArc)
      case other: Entity => other
    })(world)
  }
}

case class Asteroid(location: Point, direction: Point, size: Double) extends Entity

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
      drawContext.arc(x * scale.x, y * scale.y, size * scale.x, 0, js.Math.PI * 2)
      drawContext.closePath()
      drawContext.fill()
    }
  }

}
