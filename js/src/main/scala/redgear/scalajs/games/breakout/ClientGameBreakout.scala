package redgear.scalajs.games.breakout

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html._
import redgear.scalajs.games.engine.ClientEngine.{Artist, ClientGameBuilder}
import redgear.scalajs.games.engine.Engine.{World, GameDefinition}
import redgear.scalajs.games.engine.{CollisionDetector, Point}
import redgear.scalajs.games.engine.DrawingUtils._
import scala.scalajs.js.annotation.JSExport



@JSExport
object ClientGameBreakout extends GameDefinition{
  
  @JSExport
  def main(windowId: String): Unit = {
    val window = dom.document.getElementById(windowId).asInstanceOf[Canvas]

    val builder: ClientGameBuilder = GameBreakout.initBuilder

    builder.copy(artists = BoxArtist :: ScoreArtist :: Nil
    )
      .buildGame(
        window =          window,
        scale =           GameBreakout.scale,
        backgroundColor = "black")
  }
}

object BoxArtist extends Artist {

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {

    def draw(box: CollisionDetector.Box, color: String) =
      drawContext.render(scale) {
        drawContext.fillStyle = color
        drawContext.fillBox(box)
      }


    world.entities.foreach {
      case b: Paddle => draw(b.box, "red")
      case b: Block => draw(b.box, b.color)
      case b: Ball => draw(b.box, "red")
      case _ =>
    }
  }

}

object ScoreArtist extends Artist {

  val location = Point(850, 500)

  val max = 200
  val font = "px Georgia"

  override def draw(world: World, drawContext: CanvasRenderingContext2D, scale: Point): Unit = {
    val relative = location * scale
    drawContext.fillStyle = "blue"

    world.entities.foreach {
      case ScoreBoard(score, combo, highScore) =>
        val text = s"${if (highScore > 0) s"High Score: $highScore, " else ""}Score: ${score.toString}"
        val size = 10
        drawContext.font = s"$size$font"
        val textScale = drawContext.measureText(text).width

        drawContext.font = s"${size * ((max / textScale) - 0.1) }$font"
        drawContext.fillText(text, relative.x, relative.y)
      case _ =>
    }


  }

}





