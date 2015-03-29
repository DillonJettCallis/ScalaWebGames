package redgear.scalajs.games.asteroids

/**
 * Created by LordBlackHole on 3/23/2015.
 */
case class Game(entities: List[Entity], private val added: List[Entity], private val removed: List[Entity]) {

  def createEntity(e: Entity): Game = copy(added = e :: added)

  def destroyEntity(e: Entity): Game = copy(removed = e :: removed)

  def isTicking: Boolean = true

  def tickRate: Int = 1

  def entitiesOf[Type <: Entity]: List[Type] = entities.collect{case x: Type => x}

  def announce[Type <: Entity](event: Event): Game = entitiesOf[Type].foldLeft(this){(g, e) => e.react(event)(g)}

  def tick(tickRate: Int = tickRate): Game = entities.filter{_.isTickable}.foldLeft(this){(g, e) => e.update(g, tickRate)}

  def merge: Game = Game(entities = entities ++ added diff removed, added = Nil, removed = Nil)
}
