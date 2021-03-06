package redgear.scalajs.games.engine

import org.scalajs.dom._
import CollisionDetector.Box

/**
 * Created by LordBlackHole on 7/19/2015.
 *
 */
object DrawingUtils {


  implicit class PointCanvasContext(drawContext: CanvasRenderingContext2D) {

    def translate(loc: Point): Unit = {
      drawContext.translate(loc.x, loc.y)
    }

    def scale(scale: Point): Unit = {
      drawContext.scale(scale.x, scale.y)
    }

    def lineTo(scale: Point): Unit = {
      drawContext.lineTo(scale.x, scale.y)
    }

    def translateAndScale(scale: Point): Unit = {
      this.translate(scale)
      this.scale(scale)
    }

    def fillBox(box: Box): Unit = {
      drawContext.fillRect(box.left, box.top, box.width, box.height)
    }

    def render(scale: Point)(renderFunc: => Unit): Unit = {
      drawContext.save()
      translateAndScale(scale)
      renderFunc
      drawContext.restore()
    }

    def renderPath(scale: Point)(renderFunc: => Unit): Unit = {
      render(scale){
        drawContext.beginPath()
        renderFunc
        drawContext.closePath()
        drawContext.fill()
      }
    }

    def renderPathWithPoints(scale: Point)(points: Seq[Point]): Unit = {
      render(scale){
        drawContext.beginPath()
        points.foreach(p => lineTo(p))
        drawContext.closePath()
        drawContext.fill()
      }
    }

    def renderPathWithColor(scale: Point, color: String)(renderFunc: => Unit): Unit = {
      render(scale){
        drawContext.fillStyle = color
        drawContext.beginPath()
        renderFunc
        drawContext.closePath()
        drawContext.fill()
      }
    }
  }

}
