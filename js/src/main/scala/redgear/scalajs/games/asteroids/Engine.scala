package redgear.scalajs.games.asteroids

import monocle.macros.GenLens
import org.scalajs.dom
import org.scalajs.dom.html._
import rx._

import scala.collection.mutable
import scalaz.State
import scalaz.std.list._
import scalaz.syntax.traverse._

object Engine {

  type WorldState[A] = State[World, A]

  trait Entity {


  }

  trait Behavior {

    def update: WorldState[Unit]


  }

  trait Artist {

    def draw(world: World, drawContext: dom.CanvasRenderingContext2D, scale: Point)
  }

  trait Event {


  }

  case class KeyPressEvent(keyId: Int) extends Event

  trait EventHandler {

    def react(event: Event): WorldState[Unit]

  }

  class InputHandler(window: Canvas, scale: Point) {

    //Window resizing doesn't trigger any internal events but it's still an external one.

    val scaleX = scale.x
    val scaleY = scale.y

    val width = Var(scaleX)
    val height = Var(scaleY)

    val drawScale = Rx{Point(width() / scaleX, height() / scaleY)}

    val mouseIn = Var(false)
    val mouseLoc = Var(Point(0, 0))
    val mouseScaleLoc = Rx{mouseLoc() / drawScale()}

    val keysDown = collection.mutable.Set.empty[Int]

    val resize = () => {width() = dom.innerWidth - 20; height() = dom.innerHeight - 20}

    Obs(width){window.width = width().toInt}
    Obs(height){window.height = height().toInt}

    resize()

    dom.onkeydown = (e: dom.KeyboardEvent) => keysDown.add(e.keyCode)
    dom.onkeyup   = (e: dom.KeyboardEvent) => keysDown.remove(e.keyCode)
    dom.onresize = (e: dom.UIEvent) => resize()

    dom.onmouseover = (e: dom.MouseEvent) => mouseIn() = true
    dom.onmouseout = (e: dom.MouseEvent) => mouseIn() = false

    dom.onmousemove = (e: dom.MouseEvent) => mouseLoc() = Point(e.pageX, e.pageY)

    def processInput: List[Event] = {
      val keys = (for(id <- keysDown) yield KeyPressEvent(id)).toList

      //TODO: Mouse events, touch events

      keys
    }
  }


  case class World(private val game: Game, entities: List[Entity] = Nil) {

    def createEntity(e: Entity): World = copy(entities = entities :+ e)

    def destroyEntity(e: Entity): World = copy(entities = entities.filterNot(_ == e))

    //def replace(previous: Entity,next: Entity) = ???

    def entitiesOf[Type](clazz: Class[Type]): List[Type] = entities.filterNot{clazz == _.getClass}.map(e => e.asInstanceOf[Type])

    def announce(event: Event): World = game.announce(event, this)

  }

  object World {

    val lensEntities = GenLens[World](_.entities)

  }

  class Game(drawContext: dom.CanvasRenderingContext2D, input: InputHandler, backgroundColor: String, start: Game => World, body: WorldState[List[Unit]], eventHandlers: List[EventHandler], artists: List[Artist]) {

    private var previous: World = start(this)
    private val world: mutable.Queue[World] = new mutable.Queue()

//    val input = new InputHandler(boardScale)

    dom.setInterval(run _, 20)

    def run(): Unit = {
      tick(1)
      draw(drawContext, backgroundColor)
    }

    def tick(tickRate: Int): Unit = {
      previous = input.processInput.foldLeft(previous)((world, event) => announce(event, world))
      previous = body.exec(previous)

      //The queue right now has no use. It will be used in the future for multiplayer as a way for the server to track states.
      world enqueue previous

      //Roll the queue if we have reached max elements
      if(world.size > 10)
        world.dequeue()
    }

    def draw(drawContext: dom.CanvasRenderingContext2D, backgroundColor: String) = {
      val scale = input.drawScale()

      drawContext.fillStyle = backgroundColor
      drawContext.fillRect(0, 0, input.width(), input.height())

      artists.foreach(_.draw(previous, drawContext, scale))
    }

    def announce(event: Event, world: World): World = {
     eventHandlers.traverse[WorldState, Unit](eh => eh.react(event)).exec(world)
    }


  }

  case class GameBuider(startingEntities: List[Entity] = Nil, behaviors: List[Behavior] = Nil, eventHandlers: List[EventHandler] = Nil, artists: List[Artist] = Nil){

    def buildGame(window: Canvas, scale: Point = Point(1000, 600), backgroundColor: String = "black"): Game = {
      /** Drawing context for the window **/
      val drawContext = window.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

      dom.document.body.appendChild(window)

      val start = (game: Game) => World(game, startingEntities)

      val behave: WorldState[List[Unit]] = behaviors.traverse[WorldState, Unit](be => be.update)

      val events = eventHandlers.traverse[WorldState, Unit](eh => eh.react(null))

      val inputHandler = new InputHandler(window, scale)

      //val test: WorldState[Event] = eventHandlers.map(eh => eh.react).reduce((l, r) => l.flatMap(r))

      new Game(drawContext, inputHandler, backgroundColor, start, behave, eventHandlers, artists)
    }

  }

}