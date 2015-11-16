package redgear.scalajs.games.engine

import org.scalajs.dom
import org.scalajs.dom.html._
import redgear.scalajs.games.engine.Engine._
import rx._

import scalaz.std.list._
import scalaz.syntax.traverse._

/**
 * Created by LordBlackHole on 7/19/2015.
 *
 */
object ClientEngine {

  trait Artist {

    def draw(world: World, drawContext: dom.CanvasRenderingContext2D, scale: Point)
  }

  class ClientInputHandler(window: Canvas, scale: Point) extends InputHandler {

    //Window resizing doesn't trigger any internal events but it's still an external one.

    val scaleX = scale.x
    val scaleY = scale.y

    val width = Var(scaleX.toInt)
    val height = Var(scaleY.toInt)

    val drawScale = Rx{Point(width() / scaleX, height() / scaleY)}

    val mouseIn = Var(false)
    val mouseLoc = Var(Point(0, 0))
    val mouseScaleLoc = Rx{mouseLoc() / drawScale()}

    val keysDown = collection.mutable.Set.empty[Int]

    def resize(): Unit = {width() = dom.innerWidth; height() = dom.innerHeight}

    Obs(width){window.width = width()}
    Obs(height){window.height = height()}

    resize()

    dom.onkeydown = (e: dom.KeyboardEvent) => keysDown.add(e.keyCode)
    dom.onkeyup   = (e: dom.KeyboardEvent) => keysDown.remove(e.keyCode)
    dom.onresize = (e: dom.UIEvent) => resize()

    dom.onmouseover = (e: dom.MouseEvent) => mouseIn() = true
    dom.onmouseout = (e: dom.MouseEvent) => mouseIn() = false

    dom.onmousemove = (e: dom.MouseEvent) => mouseLoc() = Point(e.pageX, e.pageY)



    override def processInput: List[Event] = {
      val keys = (for(id <- keysDown) yield KeyPressEvent(id)).toList

      //TODO: Mouse events, touch events

      keys
    }
  }

  class ClientGame(val drawContext: dom.CanvasRenderingContext2D,
             val input: ClientInputHandler,
             val backgroundColor: String,
             val start: Game => World,
             val behaviors: List[Behavior],
             val eventHandlers: List[EventHandler],
             val artists: List[Artist]) extends Game {

    var previous: World = start(this)

    dom.setInterval(run _, 20)

    override def run(): Unit = {
      previous = tick(previous)
      draw()
    }

    def draw() = {
      val scale = input.drawScale()

      drawContext.fillStyle = backgroundColor
      drawContext.fillRect(0, 0, input.width(), input.height())

      artists.foreach(_.draw(previous, drawContext, scale))
    }
  }

  implicit def toClientGameBuilder(shared: GameBuilder): ClientGameBuilder = {
    ClientGameBuilder(startingEntities = shared.startingEntities,
      behaviors = shared.behaviors,
      eventHandlers = shared.eventHandlers
    )
  }

  case class ClientGameBuilder(startingEntities: List[Entity] = Nil, behaviors: List[Behavior] = Nil, eventHandlers: List[EventHandler] = Nil, artists: List[Artist] = Nil){

    def buildGame(window: Canvas, scale: Point, backgroundColor: String = "black"): Game = {
      /** Drawing context for the window **/
      val drawContext = window.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

      dom.document.body.appendChild(window)

      val start = (game: Game) => World(game, startingEntities.map(e => (e.id, e))(collection.breakOut))

      val inputHandler = new ClientInputHandler(window, scale)

      new ClientGame(
        drawContext = drawContext,
        input = inputHandler,
        backgroundColor = backgroundColor,
        start = start,
        behaviors = behaviors,
        eventHandlers = eventHandlers,
        artists = artists
      )
    }

  }

}
