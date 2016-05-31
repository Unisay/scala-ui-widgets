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

lazy val dancher = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "dancher",
    version := "0.1-SNAPSHOT",
    publish := {},
    publishLocal := {},
    version      := "0.1-SNAPSHOT",
    organization := "com.github.unisay",
    scalacOptions in Test ++= Seq("-Yrangepos"),
    maxErrors := 5,
    logLevel := Level.Warn,
    testFrameworks += TestFramework("utest.runner.Framework"),
    requiresDOM     := true,
    scalaJSUseRhino := false,
    libraryDependencies ++= Seq(
      "org.scalaz"   %%% "scalaz-core" % "7.2.2",
      "com.github.julien-truffaut" %%% "monocle-core"  % "1.2.1",
      "com.github.julien-truffaut" %%% "monocle-macro" % "1.2.1",
      "org.scala-js" %%% "scalajs-dom" % "0.9.0",

      "org.specs2"   %% "specs2-core"  % "3.8.2"  % "test",
      "com.lihaoyi"  %%% "utest"       % "0.4.3"  % "test"
    )
  )
