import sbt.Keys._
import sbt.TestFramework
import com.lihaoyi.workbench.Plugin._

scalaVersion in ThisBuild := "2.11.8"

lazy val dancher = crossProject.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "dancher",
    version := "0.1-SNAPSHOT",
    publish := {},
    publishLocal := {},
    version      := "0.1-SNAPSHOT",
    organization := "com.github.unisay",
    scalacOptions in Test ++= Seq("-Yrangepos"),
    logLevel := Level.Warn,
    testFrameworks += TestFramework("utest.runner.Framework"),
    requiresDOM     := true,
    scalaJSUseRhino := false,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.0",
      "com.lihaoyi" %%% "utest" % "0.4.3" % "test"
    ),
    bootSnippet := "com.github.unisay.dancher.App().main();",
    refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)
  )
  .settings(workbenchSettings:_*)
  .jvmSettings(/* Add JVM-specific settings here */)
  .jsSettings(/* Add JS-specific settings here */)

lazy val dancherJVM = dancher.jvm
lazy val dancherJS = dancher.js
