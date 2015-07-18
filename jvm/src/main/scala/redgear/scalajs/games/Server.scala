package redgear.scalajs.games

import scalatags.Text.all._
import scalatags.Text.tags2.title
import spray.routing.SimpleRoutingApp
import akka.actor.ActorSystem
import spray.http.{MediaTypes, HttpEntity}

object Template {

  val canvasId = "gamespace"

  val txt =
    "<!DOCTYPE html>" +
    html(
      head(
        title("Scala.js Breakout"),
        meta(httpEquiv:="Content-Type", content:="text/html; charset=UTF-8"),
        script(`type`:="text/javascript", src:="/client-fastopt.js"),
        script(`type`:="text/javascript", src:="//localhost:12345/workbench.js"),
        link(
          rel:="stylesheet",
          `type`:="text/css",
          href:="META-INF/resources/webjars/bootstrap/3.2.0/css/bootstrap.min.css"
        )
      ),
      body(margin:=0)(
          canvas(id:=canvasId),
          script(s"window.onload = function(){redgear.scalajs.games.asteroids.GameAsteroids().main(document.getElementById('$canvasId'))}")
      )
    )
}


object Server extends SimpleRoutingApp {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    startServer("0.0.0.0", port = 8080) {
      get{
        pathSingleSlash {
          complete{
            HttpEntity(
              MediaTypes.`text/html`,
              Template.txt
            )
          }
        } ~
        getFromResourceDirectory("")
      }
    }
  }
}
