import sbt.Keys._

scalacOptions in Test ++= Seq("-Yrangepos")

lazy val commonSettings = Seq(
  version      := "0.1-SNAPSHOT",
  organization := "com.github.unisay",
  scalaVersion := "2.11.8"
)

lazy val bus = crossProject.in(file("bus"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings: _*)
  .settings(
    jsEnv := PhantomJSEnv().value,
    jsDependencies += RuntimeDOM % "test",
    requiresDOM := true,
    persistLauncher := true,
    persistLauncher in Test := false,
    libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.9.0"
      , "com.lihaoyi"  %%% "utest"       % "0.4.3" % "test"
    )
    , testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .jvmSettings(/* Add JVM-specific settings here*/)
  .jsSettings(/* Add JS-specific settings here*/)

lazy val busJVM = bus.jvm
lazy val busJS = bus.js

lazy val dancher = project.in(file("."))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "3.8.2" % "test"
    )
  )
  .dependsOn(busJVM, busJS)
  .aggregate(busJVM, busJS)

