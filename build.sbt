import sbt.Keys._
import sbt.TestFramework

scalaVersion in ThisBuild := "2.11.8"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-unchecked",
  "-feature",
  "-deprecation:false",
  "-Xlint",
  "-Xcheckinit",
  "-Ywarn-unused-import",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code",
  "-Yno-adapted-args",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding", "UTF-8"
)

val catsVer = "0.6.0"
val scalajsVer = "0.9.0"
val specs2Ver = "3.8.2"
val monocleVer = "1.2.2"
val simulacrumVer = "0.7.0"

libraryDependencies ++= Seq(
  "org.scala-js"  %%% "scalajs-dom" % scalajsVer,
  "com.github.mpilquist" %% "simulacrum" % simulacrumVer,

  "org.typelevel" %%% "cats-kernel" % catsVer,
  "org.typelevel" %%% "cats-macros" % catsVer,
  "org.typelevel" %%% "cats-core"   % catsVer,
  "org.typelevel" %%% "cats-free"   % catsVer,

  "com.github.julien-truffaut" %%% "monocle-core"    % monocleVer,
  "com.github.julien-truffaut" %%% "monocle-generic" % monocleVer,
  "com.github.julien-truffaut" %%% "monocle-macro"   % monocleVer,
  "com.github.julien-truffaut" %%% "monocle-state"   % monocleVer,
  "com.github.julien-truffaut" %%% "monocle-refined" % monocleVer,

  "org.specs2" %% "specs2-core"       % specs2Ver % "test",
  "org.specs2" %% "specs2-scalacheck" % specs2Ver % "test"
)

lazy val dancher = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "dancher",
    version := "0.1-SNAPSHOT",
    publish := {},
    publishLocal := {},
    version := "0.1-SNAPSHOT",
    organization := "com.github.unisay",
    scalacOptions in Test ++= Seq("-Yrangepos"),
    maxErrors := 5,
    logLevel := Level.Warn,
    requiresDOM := true,
    scalaJSUseRhino := false
  )
