package redgear.scalajs.games.asteroids

/**
 * Created by LordBlackHole on 3/23/2015.
 */
trait Entity {

  def update(game: Game, tickRate:Int): Game

  def isTickable: Boolean

  def isSolid: Boolean

  def react(event: Event)(game: Game): Game

}
