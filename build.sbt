import com.lihaoyi.workbench.Plugin._
import sbt.Keys._
import spray.revolver.RevolverPlugin.Revolver

// Turn this project into a Scala.js project by importing these settings
lazy val root = project.enablePlugins(ScalaJSPlugin)


val scalazVersion = "7.1.2"
val monocleVersion = "1.1.1"
val scalaJsDomVersion = "0.8.1"

val example = crossProject.in(file(".")).settings(
  name := "ScalaWebGames",
  scalaVersion := "2.11.6",
  version := "0.1-SNAPSHOT",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "upickle" % "0.2.6",
    "com.lihaoyi" %%% "autowire" % "0.2.4",
    "com.lihaoyi" %%% "scalatags" % "0.4.5",
    "com.lihaoyi" %%% "scalarx" % "0.2.7"
  )
).jsSettings(
  workbenchSettings:_*
).jsSettings(
  name := "Client",
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
    "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % scalazVersion,
    "com.github.japgolly.fork.monocle" %%% "monocle-core" % monocleVersion,
    "com.github.japgolly.fork.monocle" %%% "monocle-macro" % monocleVersion
  ),
    bootSnippet := "example.ScalaJSExample().main();"
).jvmSettings(
  Revolver.settings:_*
).jvmSettings(
  name := "Server",
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.1",
    "io.spray" %% "spray-routing" % "1.3.1",
    "com.typesafe.akka" %% "akka-actor" % "2.3.2",
    "org.webjars" % "bootstrap" % "3.2.0",
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "com.github.julien-truffaut" %%  "monocle-core" % monocleVersion,
    "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
  )
)

addCompilerPlugin(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full))

val exampleJS = example.js
val exampleJVM = example.jvm.settings(
  (resources in Compile) += {
    (fastOptJS in (exampleJS, Compile)).value
    (artifactPath in (exampleJS, Compile, fastOptJS)).value
  }
)
