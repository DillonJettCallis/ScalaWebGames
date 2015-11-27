package redgear.scalajs.games.asteroids

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html._
import redgear.scalajs.games.engine.DrawingUtils._
import redgear.scalajs.games.engine.ClientEngine._
import redgear.scalajs.games.engine.Engine._
import redgear.scalajs.games.engine.Point

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

/**
 * Created by LordBlackHole on 3/28/2015.
 *
 */
@JSExport
object ClientGameAsteroids extends GameDefinition{

  @JSExport
  def main(windowId: String): Unit = {
    val window = dom.document.getElementById(windowId).asInstanceOf[Canvas]

    val builder: ClientGameBuilder = GameAsteroids.initBuilder

    builder.copy(artists = AsteroidArtist :: BulletArtist :: ShipArtist :: ShipExplosionArtist :: ScoreArtist :: Nil
    )
      .buildGame(
        window =          window,
        scale =           GameAsteroids.scale,
        backgroundColor = "black")
  }
}

object ShipArtist extends Artist {

  val points = {
    val h = 30
    val w = 20

    List(
      Point(h / 2, 0),
      Point(-h / 2, -w / 2),
      Point(-h / 2, w / 2)
    )
  }

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    drawContext.fillStyle = "white"

    for (Ship(loc, direct, _, _) <- world.entities){
      drawContext.renderPathWithPoints(scale)(points.map(_.rotate(direct) + loc))
    }
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



object ShipExplosionArtist extends Artist {

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    for(ShipExplosion(Point(x, y), deathTimer) <- world.entities) {
      drawContext.fillStyle = deathTimer % 4 match {
        case 0 => "red"
        case 1 => "orange"
        case 2 => "yellow"
        case 3 => "orange"
      }

      drawContext.renderPath(scale) {
        drawContext.arc(x, y, 20 - (deathTimer / 5), 0, js.Math.PI * 2)
      }
    }
  }
}

object ScoreArtist extends Artist {

  val max = 200
  val font = "px Georgia"
  val location = Point(850, 500)

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    val relative = location * scale
    drawContext.fillStyle = "white"

    val ScoreBoard(lives, score) = world.entities.find(e => e.isInstanceOf[ScoreBoard]).getOrElse(() => ScoreBoard()).asInstanceOf[ScoreBoard]

    val text = s"Lives: $lives\nScore: $score"
    val size = 10
    drawContext.font = s"$size$font"
    val textScale = drawContext.measureText(text).width

    drawContext.font = s"${size * ((max / textScale) - 0.1) }$font"
    drawContext.fillText(text, relative.x, relative.y)
  }
}

object BulletArtist extends Artist {

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    drawContext.fillStyle = "white"

    for (Bullet(Point(x, y), _) <- world.entities){
      drawContext.renderPath(scale) {
        drawContext.arc(x, y, 2, 0, js.Math.PI * 2)
      }
    }
  }

}


