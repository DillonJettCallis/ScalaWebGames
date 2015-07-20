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

  type WorldState[A] = State[World, A]

  trait GameDefinition {

    def main(windowId: String): Unit

  }

  trait Entity {


  }

  trait Behavior {

    def update: WorldState[Unit]


  }



  trait Event {


  }

  case class KeyPressEvent(keyId: Int) extends Event

  trait EventHandler {

    def react(event: Event): WorldState[Unit]

  }

  trait InputHandler {

    def processInput: List[Event]

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

  trait Game {

    def input: InputHandler

    def body: WorldState[List[Unit]]

    def eventHandlers: List[EventHandler]

    def run(): Unit

    def tick(previous: World): World = {
      val temp = input.processInput.foldLeft(previous)((world, event) => announce(event, world))
      body.exec(temp)
    }

    def announce(event: Event, world: World): World = {
      eventHandlers.traverse[WorldState, Unit](eh => eh.react(event)).exec(world)
    }


  }

  case class GameBuilder(startingEntities: List[Entity] = Nil, behaviors: List[Behavior] = Nil, eventHandlers: List[EventHandler] = Nil)

}
