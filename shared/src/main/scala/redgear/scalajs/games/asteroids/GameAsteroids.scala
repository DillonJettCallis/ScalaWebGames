package redgear.scalajs.games.asteroids

import redgear.scalajs.games.engine.Engine._
import redgear.scalajs.games.engine.Point

import scala.util.Random
import scalaz.State

/**
 * Created by LordBlackHole on 7/19/2015.
 *
 */
object GameAsteroids {

  val scale = Point(1000, 600)

  val randomGen = new Random

  def wrapAround(loc: Point): Point = (loc + scale) % scale


  def initBuilder: GameBuilder = {
    val ship = Ship(
      location = Point(500, 300)
    )

    val asteroids = for (i <- 0 to 4)
      yield Asteroid(
        location = Point(randomGen.nextDouble(), randomGen.nextDouble()) * scale,
        velocity = Point(randomGen.nextDouble(), randomGen.nextDouble()) * Point(4, 4) - Point(2, 2),
        size = randomGen.nextInt(20) + 8
      )

    GameBuilder(
      startingEntities = ship :: asteroids.toList,
      behaviors = ShipBehavior :: AsteroidBehavior :: Nil,
      eventHandlers = ShipControlHandler :: Nil
    )
  }
}


case class Ship(location: Point, direction: Double = 0, velocity: Point = Point(0, 0), controlFlags: ShipControlFlags = ShipControlFlags()) extends Entity
case class ShipControlFlags(fireCountDown: Int = 0, turnedFlag: Boolean = false, movedFlag: Boolean = false)

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
      controlFlags = ShipControlFlags()
    )
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
      case ship: Ship if !ship.controlFlags.turnedFlag => ship.copy(direction = ship.direction + turnArc, controlFlags = ship.controlFlags.copy(turnedFlag = true))
      case other: Entity => other
    })(world)
  }

  def accelerate(world: World, a: Point): World = {
    World.lensEntities.modify(_.collect{
      case ship: Ship if !ship.controlFlags.movedFlag => ship.copy(velocity = ship.velocity + a.rotate(ship.direction), controlFlags = ship.controlFlags.copy(movedFlag = true))
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