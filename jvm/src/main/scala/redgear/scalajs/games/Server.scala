package redgear.scalajs.games

import scalatags.Text.all._
import scalatags.Text.tags2.title
import spray.routing.SimpleRoutingApp
import akka.actor.ActorSystem
import spray.http.{MediaTypes, HttpEntity}

object Template {

  def gamePage(main: String) = {
    val canvasId = "gamespace"

    "<!DOCTYPE html>" +
      html(
        head(
          title("Scala.js Aseroids"),
          meta(httpEquiv := "Content-Type", content := "text/html; charset=UTF-8"),
          script(`type` := "text/javascript", src := "/client-fastopt.js"),
          script(`type` := "text/javascript", src := "/workbench.js"),
          link(
            rel := "stylesheet",
            `type` := "text/css",
            href := "META-INF/resources/webjars/bootstrap/3.2.0/css/bootstrap.min.css"
          )
        ),
        body(margin := 0)(
          canvas(id := canvasId, display := "block"),
          script(s"window.onload = function(){$main().main(document.getElementById('$canvasId'))}")
        )
      )

  }
}


object Server extends SimpleRoutingApp {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    startServer("0.0.0.0", port = 8080) {
      get{
        path("asteroids") {
          complete{
            HttpEntity(
              MediaTypes.`text/html`,
              Template.gamePage("redgear.scalajs.games.asteroids.GameAsteroids")
            )
          }
        } ~ path("breakout"){
          complete{
            HttpEntity(
              MediaTypes.`text/html`,
              Template.gamePage("redgear.scalajs.games.breakout.GameBreakout")
            )
          }
        } ~ getFromResourceDirectory("")
      }
    }
  }
}
