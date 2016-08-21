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

val catsVer       = "0.7.0"
val scalajsDomVer = "0.9.1"
val specs2Ver     = "3.8.2"
val monocleVer    = "1.2.2"
val monixVer      = "2.0-RC11"

libraryDependencies ++= Seq(
  "org.scala-js"  %%% "scalajs-dom" % scalajsDomVer,

  "io.monix"      %%% "monix"       % monixVer,
  "io.monix"      %%% "monix-eval"  % monixVer,

  //  "io.monix"      %%% "monix-cats"  % monixVer,

  "org.typelevel" %%% "cats-kernel" % catsVer,
  "org.typelevel" %%% "cats-macros" % catsVer,
  "org.typelevel" %%% "cats-core"   % catsVer,
  "org.typelevel" %%% "cats-free"   % catsVer,

  "com.github.julien-truffaut" %%% "monocle-core"    % monocleVer,
  "com.github.julien-truffaut" %%% "monocle-generic" % monocleVer,
  "com.github.julien-truffaut" %%% "monocle-macro"   % monocleVer,

  "org.scalatest"  %%% "scalatest"  % "3.0.0"  % "test",
  "org.scalacheck" %%% "scalacheck" % "1.13.2" % "test"
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
    requiresDOM := false,
    scalaJSUseRhino := false
  )
