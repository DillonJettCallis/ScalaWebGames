package redgear.scalajs.games

/**
 * Created by Jett on 3/19/2015.
 */
trait ClientUtils {
  import org.scalajs.dom
  import rx.core._
  import scalatags.JsDom.all._

   implicit def rxFrag[T <: Frag](r: Rx[T]): Frag = {
     def rSafe: dom.Node = span(r()).render
     var last = rSafe
     Obs(r, skipInitial = true){
       val newLast = rSafe
       scalajs.js.Dynamic.global.last = last
       last.parentNode.replaceChild(newLast, last)
       last = newLast
     }
     last
   }
 }
