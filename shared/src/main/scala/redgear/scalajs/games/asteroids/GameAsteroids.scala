package redgear.scalajs.games.asteroids

import redgear.scalajs.games.engine.CollisionDetector.{Circle, Triangle}
import redgear.scalajs.games.engine.Engine._
import redgear.scalajs.games.engine.{CollisionDetector, Point}

import scala.util.Random

/**
 * Created by LordBlackHole on 7/19/2015.
 *
 */
object GameAsteroids {

  val scale = Point(1000, 600)

  val randomGen = new Random

  val shipStartingPoint = scale / 2

  def wrapAround(loc: Point): Point = (loc + scale) % scale


  def initBuilder: GameBuilder = {
    val ship = Ship()

    val asteroids = for (i <- 0 to 4)
      yield Asteroid(
        location = Point(randomGen.nextDouble(), randomGen.nextDouble()) * scale,
        velocity = Point(randomGen.nextDouble(), randomGen.nextDouble()) * Point(4, 4) - Point(2, 2),
        size = randomGen.nextInt(20) + 8
      )

    GameBuilder(
      startingEntities = ScoreBoard() :: ship :: asteroids.toList,
      behaviors = ShipExplosionBehavior :: ShipBehavior :: BulletBehavior :: AsteroidBehavior :: Nil,
      eventHandlers = DestroyAsteroidEventHandler :: ShipDeathExplosionEventHandler :: ShipControlHandler :: Nil
    )
  }
}


case class Ship(location: Point = GameAsteroids.shipStartingPoint, direction: Double = 0, velocity: Point = Point(0, 0), controlFlags: ShipControlFlags = ShipControlFlags()) extends Entity
case class ShipControlFlags(fireCountDown: Int = 0, turnedFlag: Boolean = false, movedFlag: Boolean = false)

object ShipBehavior extends Behavior {

  override def update(world: World) = {
    world.fold{ (w, e) =>
      e match {
        case me: Ship => if(checkCollide(me, w)) w.announce(ShipDeathEvent(me.location)).replaceEntity(me, ShipExplosion(me.location)) else w.replaceEntity(me, move(me))
        case e: Entity => w
      }
    }
  }

  private def move(input: Ship): Ship = {
    val coolDown = input.controlFlags.fireCountDown - 1

    input.copy(
      location = GameAsteroids.wrapAround(input.location + input.velocity),
      controlFlags = ShipControlFlags(fireCountDown = if(coolDown < 0) 0 else coolDown)
    )
  }

  private def checkCollide(ship: Ship, world: World): Boolean = {
    val points = {
      val h = 30
      val w = 20

      List(
        Point(h / 2, 0),
        Point(-h / 2, -w / 2),
        Point(-h / 2, w / 2)
      )
    }.map(_.rotate(ship.direction) + ship.location)

    val tri = Triangle(points.head, points(1), points(2))

    val collisions = for (Asteroid(loc, _, size) <- world.entities.toStream)
      yield CollisionDetector.isColliding(tri, Circle(loc, size))

    collisions.exists(identity)
  }

}

object ShipControlHandler extends EventHandler {

  val turnArc = 0.1
  val acceleration = Point(0.2, 0)

  override def react(event: Event, world: World) = {
    event match {
      case KeyPressEvent(65) | KeyPressEvent(37) => turn(world, -turnArc) //A or Left Arrow
      case KeyPressEvent(68) | KeyPressEvent(39) => turn(world, turnArc) //D or Right Arrow
      case KeyPressEvent(87) | KeyPressEvent(38) => accelerate(world, acceleration) //W or Up Arrow
      case KeyPressEvent(83) | KeyPressEvent(40) => accelerate(world, acceleration * -1) //S or Down Arrow
      case KeyPressEvent(32) => shoot(world) //Space bar
      case other: Event => world
    }
  }

  def turn(world: World, turnArc: Double): World = {
    world.fold { (w, e) =>
      e match {
        case ship: Ship if !ship.controlFlags.turnedFlag => w.replaceEntity(ship, ship.copy(direction = ship.direction + turnArc, controlFlags = ship.controlFlags.copy(turnedFlag = true)))
        case other: Entity => w
      }
    }
  }

  def accelerate(world: World, a: Point): World = {
    world.fold { (w, e) =>
      e match {
        case ship: Ship if !ship.controlFlags.movedFlag => w.replaceEntity(ship, ship.copy(velocity = ship.velocity + a.rotate(ship.direction), controlFlags = ship.controlFlags.copy(movedFlag = true)))
        case other: Entity => w
      }
    }
  }

  def shoot(world: World): World ={
    world.fold { (w, e) =>
      e match {
        case ship: Ship if ship.controlFlags.fireCountDown == 0 =>
          w.createEntity(Bullet(ship.location, Point(5, 0) rotate ship.direction))
            .replaceEntity(ship, ship.copy(controlFlags = ship.controlFlags.copy(fireCountDown = 20)))
        case other: Entity => w
      }
    }
  }
}

case class ShipDeathEvent(location: Point) extends Event

case class Asteroid(location: Point, velocity: Point, size: Double) extends Entity

object AsteroidBehavior extends Behavior {

  override def update(world: World): World = {
    world.fold { (w, e) =>
      e match {
        case me: Asteroid => w.replaceEntity(me, move(me))
        case e: Entity => w
      }
    }
  }

  private def move(input: Asteroid): Asteroid = {
    input.copy(location = GameAsteroids.wrapAround(input.location + input.velocity))
  }
}

case class ShipExplosion(location: Point, deathTimer: Int = 50) extends Entity

object ShipExplosionBehavior extends Behavior {

  override def update(world: World): World = {

   world.fold { (w, e) =>
     e match {
       case explosion: ShipExplosion if explosion.deathTimer == 0 => w.replaceEntity(explosion, Ship())
       case explosion: ShipExplosion => w.replaceEntity(explosion, explosion.copy(deathTimer = explosion.deathTimer - 1))
       case e: Entity => w
     }
   }
  }
}

object ShipDeathExplosionEventHandler extends EventHandler {

  override def react(event: Event, world: World): World = event match {
    case ShipDeathEvent(location) =>
      world.fold{(w, e) =>
        e match {
          case score: ScoreBoard => w.replaceEntity(score, score.copy(lives = score.lives - 1))
          case e: Entity => w
        }
      }
    case _ => world
  }
}

case class ScoreBoard(lives: Int = 3, points: Int = 0) extends Entity

case class Bullet(location: Point, velocity: Point) extends Entity
case class DestroyAsteroidEvent(asteroid: Asteroid) extends Event

object BulletBehavior extends Behavior {

  override def update(world: World): World = {
    world.fold{ (w, e) =>
      e match {
        case bullet: Bullet =>
          val collisions = for {
            asteroid: Asteroid <- world.entities.toStream.filter(_.isInstanceOf[Asteroid]).map(_.asInstanceOf[Asteroid])
            checked = asteroid if CollisionDetector.isColliding(Circle(bullet.location, 2), Circle(asteroid.location, asteroid.size))
          } yield checked

          val collide = collisions.headOption

          if(collide.isDefined){
            println("Collision")
            w.destroyEntity(bullet)
              .destroyEntity(collide.get)
              .announce(DestroyAsteroidEvent(collide.get))
          } else {
            val next = bullet.location + bullet.velocity
            val Point(maxX, maxY) = GameAsteroids.scale

            if(next.x < 0 || next.y < 0 || next.x > maxX || next.y > maxY) {
              println("Destroy")
              w.destroyEntity(bullet)
            } else {
              println("Move")
              w.replaceEntity(bullet, bullet.copy(location = next))
            }
          }
        case _: Entity => w
      }
    }
  }
}

object DestroyAsteroidEventHandler extends EventHandler {

  override def react(event: Event, world: World): World = event match {
    case DestroyAsteroidEvent(_) => world.fold { (w, e) =>
      e match {
        case score: ScoreBoard => w.replaceEntity(score, score.copy(points = score.points + 100))
        case _: Entity => w
      }
    }
    case _ => world
  }

}