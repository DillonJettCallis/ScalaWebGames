package redgear.scalajs.games.asteroids

import org.scalajs.dom
import org.scalajs.dom.html._
import redgear.scalajs.games.breakout.Point
import rx._

import scala.scalajs.js.annotation.JSExport
import scala.util.Random

/**
 * Created by LordBlackHole on 3/28/2015.
 */
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



  def run(): Unit = {

  }

  def draw(drawContext: dom.CanvasRenderingContext2D): Unit = {

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
