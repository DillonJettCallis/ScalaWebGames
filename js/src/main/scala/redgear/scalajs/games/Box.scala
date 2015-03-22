package redgear.scalajs.games

import org.scalajs.dom
import redgear.scalajs.games.Point.toPoint

/**
 * Created by LordBlackHole on 3/19/2015.
 */
trait Box extends Entity {

  def location: Point
  def size: Point


  def left = location.x
  def top = location.y
  def right = left + size.x
  def bottom = top + size.y

  def width = size.x
  def height = size.y
  def center = location + (size / 2.0)

  def color: String

  def draw(drawContext: dom.CanvasRenderingContext2D, scale: Point) = {
    drawContext.fillStyle = color
    drawContext.fillRect(left * scale.x, top * scale.y, size.x * scale.x, size.y * scale.y)
  }

  def |(other: Box): Boolean = horizontal(other) && vertical(other)

  def horizontal(other: Box): Boolean = Math.min(right,other.right) - Math.max(left,other.left) > 0

  def vertical(other: Box): Boolean = Math.min(bottom,other.bottom) - Math.max(top,other.top) > 0

 }
