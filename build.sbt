import sbt.Keys._
import sbt.TestFramework

scalaVersion in ThisBuild := "2.11.8"

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
      "org.scala-js" %%% "scalajs-dom" % "0.9.0",
      "com.lihaoyi"  %%% "utest"       % "0.4.3"  % "test"
    )
  )