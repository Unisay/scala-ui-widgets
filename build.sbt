import sbt.Keys._

scalaVersion in ThisBuild := "2.11.8"

scalacOptions ++= Seq(
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

val catsVer       = "0.7.2"
val scalajsDomVer = "0.9.1"
val monocleVer    = "1.2.2"
val fs2Ver        = "0.9.1"
val fs2CatsVer    = "0.1.0"
val refinedVer    = "0.5.0"
val scalatestVer  = "3.0.0"
val scalacheckVer = "1.13.2"

libraryDependencies ++= Seq(
  "org.scala-js"    %%% "scalajs-dom" % scalajsDomVer,
  "co.fs2"          %%% "fs2-core"    % fs2Ver,
  "co.fs2"          %%% "fs2-cats"    % fs2CatsVer,
  "org.typelevel"   %%% "cats-kernel" % catsVer,
  "org.typelevel"   %%% "cats-macros" % catsVer,
  "org.typelevel"   %%% "cats-core"   % catsVer,
  "org.typelevel"   %%% "cats-free"   % catsVer,
  "eu.timepit"      %%% "refined"     % refinedVer,

  "com.github.julien-truffaut" %%% "monocle-core"    % monocleVer,
  "com.github.julien-truffaut" %%% "monocle-generic" % monocleVer,
  "com.github.julien-truffaut" %%% "monocle-macro"   % monocleVer,

  "org.scalatest"  %%% "scalatest"          % scalatestVer  % "test",
  "org.scalacheck" %%% "scalacheck"         % scalacheckVer % "test",
  "eu.timepit"     %%% "refined-scalacheck" % refinedVer    % "test"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

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
    logLevel := Level.Info,
    requiresDOM := true,
    scalaJSUseRhino := false
  )
