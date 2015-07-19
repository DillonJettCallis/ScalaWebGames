package redgear.scalajs.games.asteroids

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html._
import redgear.scalajs.games.asteroids.Engine._
import redgear.scalajs.games.asteroids.DrawingUtils._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.Random
import scalaz.State

/**
 * Created by LordBlackHole on 3/28/2015.
 *
 */
@JSExport
object GameAsteroids {


  val scaleX: Double = 1000
  val scaleY: Double = 600
  val scale = Point(scaleX, scaleY)

  val randomGen = new Random()

  @JSExport
  /** The game window is where all objects are rendered **/
  def main(window: Canvas): Unit = {
    val ship = Ship(
      location = Point(500, 300),
      direction = 0
    )

    val asteroids = for (i <- 0 to 4)
      yield Asteroid(
        location = Point(randomGen.nextInt(scaleX.toInt), randomGen.nextInt(scaleY.toInt)),
        velocity = Point(randomGen.nextInt(4) - 2, randomGen.nextInt(4) - 2),
        size = randomGen.nextInt(20) + 8
      )

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

  def wrapAround(loc: Point): Point = (loc + scale) % scale

}



case class Ship(location: Point, direction: Double = 0, velocity: Point = Point(0, 0), fireCountDown: Int = 0, turnedFlag: Boolean = false, movedFlag: Boolean = false) extends Entity

object ShipBehavior extends Behavior {

  override def update = State.modify{ world =>
    World.lensEntities.modify(_.collect {
      case me: Ship => move(me)
      case e: Entity => e
    })(world)
  }

  private def move(input: Ship): Ship = {
    input.copy(
      location = GameAsteroids.wrapAround(input.location + input.velocity),
      turnedFlag = false,
      movedFlag = false
    )
  }
}

object ShipArtist extends Artist {

  val points = {
    val h = 30
    val w = 20

    Seq(
      Point(h / 2, 0),
      Point(-h / 2, -w / 2),
      Point(-h / 2, w / 2)
    )
  }

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    drawContext.fillStyle = "white"

    for (Ship(loc, direct, _, _, _, _) <- world.entities){
      drawContext.renderPathWithPoints(scale)(points.map(_.rotate(direct) + loc))
    }
  }
}

object ShipControlHandler extends EventHandler {

  val turnArc = 0.1
  val acceleration = Point(0.2, 0)

  override def react(event: Event): WorldState[Unit] = State.modify{ world =>
    event match {
      case KeyPressEvent(65) | KeyPressEvent(37) => turn(world, -turnArc) //A or Left Arrow
      case KeyPressEvent(68) | KeyPressEvent(39) => turn(world, turnArc) //D or Right Arrow
      case KeyPressEvent(87) | KeyPressEvent(38) => accelerate(world, acceleration) //W or Up Arrow
      case KeyPressEvent(83) | KeyPressEvent(40) => accelerate(world, acceleration * -1) //S or Down Arrow
      case other: Event => world
    }
  }

  def turn(world: World, turnArc: Double): World = {
    World.lensEntities.modify(_.collect{
      case ship: Ship if !ship.turnedFlag => ship.copy(direction = ship.direction + turnArc, turnedFlag = true)
      case other: Entity => other
    })(world)
  }

  def accelerate(world: World, a: Point): World = {
    World.lensEntities.modify(_.collect{
      case ship: Ship if !ship.movedFlag => ship.copy(velocity = ship.velocity + a.rotate(ship.direction), movedFlag = true)
      case other: Entity => other
    })(world)
  }
}

case class Asteroid(location: Point, velocity: Point, size: Double) extends Entity

object AsteroidBehavior extends Behavior {

  override def update = State.modify{world =>
    World.lensEntities.modify(_.collect {
      case me: Asteroid => move(me)
      case e: Entity => e
    })(world)
  }

  private def move(input: Asteroid): Asteroid = {
    input.copy(location = GameAsteroids.wrapAround(input.location + input.velocity))
  }
}

object AsteroidArtist extends Artist {

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    drawContext.fillStyle = "white"

    for (Asteroid(Point(x, y), _, size) <- world.entities){
      drawContext.renderPath(scale) {
        drawContext.arc(x, y, size, 0, js.Math.PI * 2)
      }
    }
  }

}
