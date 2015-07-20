package redgear.scalajs.games.asteroids

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html._
import redgear.scalajs.games.engine.{DrawingUtils, Point}
import DrawingUtils._
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

    builder.copy(artists = ShipArtist :: AsteroidArtist :: Nil)
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
