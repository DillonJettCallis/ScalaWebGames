package redgear.scalajs.games.breakout

import org.scalajs.dom

/**
 * Created by LordBlackHole on 3/19/2015.
 */
trait Entity {

   def step: List[Entity]

   def draw(drawContext: dom.CanvasRenderingContext2D, scale: Point)
 }
