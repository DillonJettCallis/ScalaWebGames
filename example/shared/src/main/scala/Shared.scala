package redgear.js.games


trait Api{
  def list(path: String): Seq[String]
}