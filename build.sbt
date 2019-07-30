import MyCompileOptions._
import sbt.Keys.resolvers

// Adding two versions here causes the second to fail 2.13.0
// no matter what the order is, sbt bug I think Need to investigate
// If I reload with one it still fails, quiting sbt and running again works
lazy val supportedScalaVersions = List("2.13.0")

scalaVersion in ThisBuild := supportedScalaVersions.head
organization in ThisBuild := "com.odenzo"
version in ThisBuild      := "0.0.3"
name in ThisBuild         := "ripple-local-signing"

coverageMinimum       := 70
coverageFailOnMinimum := false
coverageHighlighting  := true

fork in Test                   := true
publishArtifact in Test        := false
parallelExecution in ThisBuild := false

lazy val root = (project in file("."))
  .aggregate(signing)
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true
  )

lazy val signing = (project in file("modules/shared"))
  .settings(
    // Woops, this is an Ivy Formatted repo instead of Maven!?
    resolvers in ThisBuild += Resolver.bintrayRepo("odenzooss", "maven"),
    //resolvers in ThisBuild += Resolver.bintrayIvyRepo("odenzooss", "maven"),
    crossScalaVersions := supportedScalaVersions,
    scalacOptions := (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => optsV12 ++ warningsV12 ++ lintersV12
      case Some((2, n)) if n >= 13 => optsV13 ++ warningsV13 ++ lintersV13
      case _                       => Seq("-Yno-adapted-args")
    }),
    libraryDependencies += "com.odenzo" %% "ripple-binary-codec" % "0.2.7",
    libraryDependencies ++= xlibs ++ lib_bouncycastle,
    devSettings
  )

val devSettings = Seq(
  logBuffered in Test       := false,
  parallelExecution in Test := false
)

val circeVersion  = "0.12.0-M4"
val catsVersion   = "2.0.0-M4"
val spireVersion  = "0.17.0-M1"
val scribeVersion = "2.7.9"

val scalaTestVersion  = "3.0.8"
val scalaCheckVersion = "1.14.0"

// These are my standard stack and are all ScalaJS enabled.
val xlibs = Seq(
  "org.scalatest"  %% "scalatest"     % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck"    % scalaCheckVersion % Test,
  "io.circe"       %% "circe-core"    % circeVersion,
  "io.circe"       %% "circe-generic" % circeVersion,
  "io.circe"       %% "circe-parser"  % circeVersion,
  "org.typelevel"  %% "cats-core"     % catsVersion,
  "org.typelevel"  %% "cats-effect"   % catsVersion,
  "org.typelevel"  %% "spire"         % spireVersion,
  "com.outr"       %% "scribe"        % scribeVersion
)

// Java Only Crypto Library
val lib_bouncycastle = {
  val version = "1.62"
  Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % version
  )
}
