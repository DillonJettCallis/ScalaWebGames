package redgear.scalajs.games.engine

import monocle.macros.GenLens

import scala.collection.mutable
import scalaz._
import scalaz.std.list._
import scalaz.syntax.traverse._

/**
 * Created by LordBlackHole on 7/19/2015.
 *
 */
object Engine {

  trait GameDefinition {

    def main(windowId: String): Unit

  }

  trait Entity {


  }

  trait Behavior {

    def update(world: World): World

  }



  trait Event {


  }

  case class KeyPressEvent(keyId: Int) extends Event

  trait EventHandler {

    def react(event: Event, world: World): World

  }

  trait InputHandler {

    def processInput: List[Event]

  }

  case class World(private val game: Game, entities: List[Entity] = Nil) {

    def createEntity(e: Entity): World = copy(entities = entities :+ e)

    def destroyEntity(e: Entity): World = copy(entities = entities.filterNot(_ == e))

    def replaceEntity(previous: Entity, next: Entity) = destroyEntity(previous).createEntity(next)

    def announce(event: Event): World = game.announce(event, this)

  }

  object World {

    val lensEntities = GenLens[World](_.entities)

  }

  trait Game {

    def input: InputHandler

    def behaviors: List[Behavior]

    def eventHandlers: List[EventHandler]

    def run(): Unit

    def tick(previous: World): World = {
      val temp: World = input.processInput.foldLeft(previous)((world, event) => announce(event, world))
      behaviors.foldLeft(temp)((world, eh) => eh.update(world))
    }

    def announce(event: Event, world: World): World = {
      eventHandlers.foldLeft(world)((world, eh) => eh.react(event, world))
    }


  }

  case class GameBuilder(startingEntities: List[Entity] = Nil, behaviors: List[Behavior] = Nil, eventHandlers: List[EventHandler] = Nil)

}
