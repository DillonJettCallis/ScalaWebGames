package redgear.scalajs.games.asteroids

import org.scalajs.dom

/**
 * Created by LordBlackHole on 3/28/2015.
 */
trait Drawable {


  def draw(drawContext: dom.CanvasRenderingContext2D, scale: Point)
}
