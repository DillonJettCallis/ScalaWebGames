package redgear.scalajs.games.breakout

import redgear.scalajs.games.engine.CollisionDetector.Box
import redgear.scalajs.games.engine.Engine._
import redgear.scalajs.games.engine.{CollisionDetector, Point}

import scala.util.Random

/**
 * Created by LordBlackHole on 11/16/2015.
 */
object GameBreakout {

  val scale = Point(1000, 600)

  val randomGen = new Random

  val blockColors = List("red", "blue", "green", "yellow", "purple", "cyan", "lime")




  def createBlocks: List[Entity] = {
    (
      for{ x <- 5 to 1000 by 50
           y <- 5 to 200 by 20
      } yield Block(Point(x, y), blockColors(randomGen.nextInt(blockColors.size)))
      ).toList
  }

  def initBuilder: GameBuilder =
    GameBuilder(
      startingEntities = ScoreBoard() :: Paddle(Point(50, 550)) :: Ball(Point(50, 530), Point(3, -4)) :: Ball(Point(40, 530), Point(-4, -3)) :: createBlocks,
      behaviors = PaddleBehavior :: BallBehavior :: Nil,
      eventHandlers = ScoreEventHandler :: PaddleInputHandler :: Nil
    )
}

case class Paddle(location: Point, hasMoved: Boolean = false) extends Entity {

  def box = Box(location, location + PaddleInputHandler.size)

}

object PaddleBehavior extends Behavior {

  override def update(world: World): World = world.foldMatch { w => {
      case p: Paddle => world.replaceEntity(p, p.copy(hasMoved = false))
    }
  }

}

object PaddleInputHandler extends EventHandler {

  val size = Point(80,10)
  val speed = 4

  override def react(event: Event, world: World): World = event match {
    case MouseEvent(in, Point(x1, _)) if in =>
      val temp = x1 - size.x / 2
      val x = if(temp > GameBreakout.scale.x - size.x) GameBreakout.scale.x - size.x else if(temp < 0) 0 else temp

      world.foldMatch {w => {
        case p: Paddle if !p.hasMoved => w.replaceEntity(p, p.copy(location = p.location.copy(x = x), hasMoved = true))
      }
    }
    case KeyPressEvent(65) | KeyPressEvent(37) => world.foldMatch {w => { //A or Left Arrow
        case p: Paddle if !p.hasMoved => w.replaceEntity(p, move(p, true))
      }
    }
    case KeyPressEvent(68) | KeyPressEvent(39) => world.foldMatch {w => {//D or Right Arrow
        case p: Paddle if !p.hasMoved => w.replaceEntity(p, move(p, false))
      }
    }
    case _: Event => world
  }

  def move(p: Paddle, goLeft: Boolean) = {
    val Paddle(location, _) = p

    def left = if (location.x - speed > 0) Point(-speed, 0) else Point(0, 0)
    def right = if (location.x + speed < GameBreakout.scale.x - size.x) Point(speed, 0) else Point(0, 0)

    if(goLeft)
      p.copy(location = location + left, hasMoved = true)
    else
      p.copy(location = location + right, hasMoved = true)
  }
}


case class Ball(location: Point, velocity: Point) extends Entity {

  def box = Box(location, location + BallBehavior.size)

}

object BallBehavior extends Behavior {

  val size = Point(10,10)

  def width = size.x
  def height = size.y

  override def update(world: World): World = world.foldMatch { w => {
    case b: Ball =>
      move(b, w)
    }
  }

  def move(b: Ball, w: World): World = {
    val next = b.copy(location = b.location + b.velocity)

    val box = next.box

    def checkWalls =  checkX || checkY

    def checkX = next.location.x < 0 || next.location.x + width > GameBreakout.scale.x

    def checkY = checkBottom || checkTop

    def checkBottom = next.location.y + height > GameBreakout.scale.y

    def checkTop = next.location.y < 0

    def bounce(x: Boolean, y: Boolean): Ball = {
      val horizontal = if(x) b.velocity.mirrorX else b.velocity
      val vertical = if(y) horizontal.mirrorY else horizontal

      next.copy(velocity = vertical)
    }

    def calcBlockCollision(b: Box): Ball = {
      bounce(box.horizontal(b.copy(b.location.copy(y = 0))), box.vertical(b.copy(b.location.copy(x = 0))))
    }

    /**The paddle change the reflect velocity based on position, and hence is different than a block collision **/
    def calcPaddleCollision(p: Paddle): Ball = {
      val xDiff = box.center.x - p.box.center.x
      val fraction = xDiff / (size.x + p.box.size.x)
      val speed = b.velocity.x.abs + b.velocity.y.abs
      val xSpeed = speed * fraction
      val ySpeed = -(speed - xSpeed.abs)

      //dom.console.log(s"Diff: $xDiff, Fraction: $fraction, Speed: $speed, Velocity: ($xSpeed, $ySpeed)")
      next.copy(velocity = Point(xSpeed, ySpeed))
    }

    def checkCollidePaddle: Option[Paddle] = w.entities.filter(_.isInstanceOf[Paddle]).map(_.asInstanceOf[Paddle]).find(pd => CollisionDetector.isColliding(box, pd.box))
    def checkCollideBlocks: Option[Block] = w.entities.filter(_.isInstanceOf[Block]).map(_.asInstanceOf[Block]).find(bl => CollisionDetector.isColliding(box, bl.box))



    if(checkWalls)
      if(checkBottom)
        w.replaceEntity(b, bounce(checkX, checkY)).announce(ScoreResetEvent(next)) //If the ball hit the bottom, move and reset score
      else
        w.replaceEntity(b, bounce(checkX, checkY)) //If the ball hit a wall, or ceiling, just move.
    else {
      val checkPaddle = checkCollidePaddle

      if (checkPaddle.isDefined) {
        val last = calcPaddleCollision(checkPaddle.get)
        w.replaceEntity(b, last).announce(ResetComboEvent(last)) //If the ball hit the paddle, move and reset the combo
      } else {
        val checkBlock = checkCollideBlocks

        if (checkBlock.isDefined) {
          val last = calcBlockCollision(checkBlock.get.box)
          w.replaceEntity(b, last).announce(BlockBreakEvent(checkBlock.get)) //If the ball hit a block, move and break the block
        }
        else
          w.replaceEntity(b, next) //If the ball didn't hit anything, just move.
      }
    }
  }

}

case class Block(location: Point, color: String) extends Entity {

  def box = Box(location, location + Point(40, 15))

}


case class ScoreBoard(score: Int = 0, combo: Int = 1, highScore: Int = 0) extends Entity

case class BlockBreakEvent(block: Block) extends Event
case class ResetComboEvent(ball: Ball) extends Event
case class ScoreResetEvent(ball: Ball) extends Event

object ScoreEventHandler extends EventHandler {
  override def react(event: Event, world: World): World = event match {
    case BlockBreakEvent(block) => world.destroyEntity(block).foldMatch{w => {
      case s: ScoreBoard => w.replaceEntity(s, s.copy(score = s.score + 1 * s.combo, combo = if (s.combo < 10) s.combo + 1 else s.combo))
    }}
    case _: ResetComboEvent => world.foldMatch{w => {
      case s: ScoreBoard => w.replaceEntity(s, s.copy(combo = 0))
    }}
    case _: ScoreResetEvent => world.foldMatch{w => {
      case s: ScoreBoard => w.replaceEntity(s, s.copy(score = 0, combo = 0, highScore = Math.max(s.score, s.highScore)))
    }}
    case _: Event => world
  }
}