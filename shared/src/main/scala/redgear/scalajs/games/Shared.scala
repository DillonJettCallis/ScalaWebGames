package redgear.scalajs.games

trait Api{
  def list(path: String): Seq[String]
}