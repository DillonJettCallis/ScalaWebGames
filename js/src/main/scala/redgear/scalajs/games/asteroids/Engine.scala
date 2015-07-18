package redgear.scalajs.games.asteroids

import monocle.macros.GenLens
import org.scalajs.dom

import scala.collection.mutable
import scalaz.{Monoid, Scalaz, State}

object Engine {

  type WorldState[A] = State[World, A]

  trait Entity {

    def isSolid: Boolean

//    def react(event: Event)(game: Game): Game

  }

  trait Behavior {

    def update: WorldState[Unit]


  }

  trait Artist {

    def draw(world: World, drawContext: dom.CanvasRenderingContext2D, scale: Point)
  }

  trait Event {




  }


  case class World(tickRate: Int = 1, entities: List[Entity]) {

    def createEntity(e: Entity): World = copy(entities = entities :+ e)

    def destroyEntity(e: Entity): World = copy(entities = entities.filterNot(_ == e))

    def isTicking: Boolean = true

    def entitiesOf[Type](clazz: Class[Type]): List[Type] = entities.filterNot{clazz == _.getClass}.map(e => e.asInstanceOf[Type])

//    def announce[Type <: Entity](clazz: Class[Type], event: Event): Game = entitiesOf[Type](clazz).foldLeft(this){(g, e) => e.react(event)(g)}
  }

  object World {

    val lensEntities = GenLens[World](_.entities)

  }

  class Game(start: World, body: WorldState[List[Unit]], artists: List[Artist]) {

    private var previous: World = start
    private val world: mutable.Queue[World] = new mutable.Queue()

    def tick(tickRate: Int): Unit = {
      previous = body.exec(previous.copy(tickRate = tickRate))
      world enqueue previous

      //Roll the queue if we have reached max elements
      if(world.size > 10)
        world.dequeue()
    }

    def draw(drawContext: dom.CanvasRenderingContext2D, scale: Point) = {
      artists.foreach(_.draw(previous, drawContext, scale))
    }


  }

  case class GameBuider(startingEntities: List[Entity], behaviors: List[Behavior], artists: List[Artist]){

    import scalaz.syntax.traverse._
    import scalaz.std.list._

    def buildGame: Game = {
      val start = World(1, startingEntities)

      val test: WorldState[List[Unit]] = behaviors.traverse[WorldState, Unit](be => be.update)

      new Game(start, test, artists)
    }

  }

}