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
    version := "0.1-SNAPSHOT",
    organization := "com.github.unisay",
    scalacOptions in Test ++= Seq("-Yrangepos"),
    maxErrors := 5,
    logLevel := Level.Warn,
    testFrameworks += TestFramework("utest.runner.Framework"),
    requiresDOM := true,
    scalaJSUseRhino := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-kernel" % "0.6.0",
      "org.typelevel" %%% "cats-macros" % "0.6.0",
      "org.typelevel" %%% "cats-core"   % "0.6.0",
      "org.typelevel" %%% "cats-free"   % "0.6.0",
      "org.scala-js"  %%% "scalajs-dom" % "0.9.0",

      "org.specs2" %% "specs2-core" % "3.8.2" % "test",
      "org.specs2" %% "specs2-scalacheck" % "3.8.2" % "test"
    )
  )
