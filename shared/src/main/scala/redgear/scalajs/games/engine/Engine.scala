package redgear.scalajs.games.engine

/**
 * Created by LordBlackHole on 7/19/2015.
 *
 */
object Engine {

  trait GameDefinition {

    def main(windowId: String): Unit

  }

  trait Entity {

    val id: Int = World.nextId

  }

  trait Behavior {

    def update(world: World): World

  }



  trait Event {


  }

  case class KeyPressEvent(keyId: Int) extends Event
  case class MouseEvent(in: Boolean, loc: Point) extends Event


  trait EventHandler {

    def react(event: Event, world: World): World

  }

  trait InputHandler {

    def processInput: List[Event]

  }

  case class World(private val game: Game, entityMap: Map[Int, Entity] = Map()) {

    def entities = entityMap.values

    def createEntity(e: Entity): World = copy(entityMap = entityMap + (e.id -> e))

    def destroyEntity(e: Entity): World = copy(entityMap = entityMap - e.id)

    def replaceEntity(previous: Entity, next: Entity) = destroyEntity(previous).createEntity(next)

    def announce(event: Event): World = game.announce(event, this)

    def fold(func: (World, Entity) => World): World = entities.foldLeft(this)(func)

    def foldMatch(func: World => PartialFunction[Entity, World]) = {
      fold{
        (w: World, e: Entity) => {
//          val fun: PartialFunction[Entity, World] = func(w).orElse { case _: Entity => w }

          func(w).applyOrElse[Entity, World](x = e, default = (j) => w)

//          fun(e)
        }
      }

    }

  }

  object World {

    private var idGen = 0

    private[Engine] def nextId = {
      idGen = idGen + 1
      idGen
    }

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
