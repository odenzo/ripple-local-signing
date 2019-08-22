import MyCompileOptions._
import sbt.Keys.resolvers

// Adding two versions here causes the second to fail 2.13.0
// no matter what the order is, sbt bug I think Need to investigate
// If I reload with one it still fails, quiting sbt and running again works
lazy val supportedScalaVersions = List("2.13.0", "2.12.9")

scalaVersion in ThisBuild := supportedScalaVersions.head
organization in ThisBuild := "com.odenzo"
version in ThisBuild      := "0.1.0"
name in ThisBuild         := "ripple-local-signing"

val circeVersion      = "0.12.0-RC3"
val catsVersion       = "2.0.0-RC1"
val catsEffectVersion = "2.0.0-RC1"
val spireVersion      = "0.17.0-M1"
val scribeVersion     = "2.7.9"
val scalaTestVersion  = "3.0.8"
val scalaCheckVersion = "1.14.0"

// SBT 1.3.x options
//turbo in ThisBuild := true

//coverageEnabled in ThisBuild := true
//coverageMinimum              := 70
//coverageFailOnMinimum        := false
//coverageHighlighting         := true

fork in Test                   := true
publishArtifact in Test        := false
parallelExecution in ThisBuild := false

lazy val ripple_local_signing_root = (project in file("."))
  .aggregate(ripple_local_signing)
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true
  )

lazy val ripple_local_signing = (project in file("."))
  .settings(
    name := "ripple-local-signing",
    // Woops, this is an Ivy Formatted repo instead of Maven!?
    resolvers in ThisBuild += Resolver.bintrayRepo("odenzooss", "maven"),
    //resolvers in ThisBuild += Resolver.bintrayIvyRepo("odenzooss", "maven"),
    crossScalaVersions := supportedScalaVersions,
    scalacOptions := (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => optsV12 ++ warningsV12 ++ lintersV12
      case Some((2, n)) if n >= 13 => optsV13 ++ warningsV13 ++ lintersV13
      case _                       => Seq("-Yno-adapted-args")
    }),
    libraryDependencies += "com.odenzo" %% "ripple-binary-codec" % "0.1.0",
    libraryDependencies ++= xlibs ++ lib_bouncycastle,
    devSettings
  )

val devSettings = Seq(
  logBuffered in Test       := false,
  parallelExecution in Test := false
)

// These are my standard stack and are all ScalaJS enabled.
val xlibs = Seq(
  "org.scalatest"  %% "scalatest"            % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck"           % scalaCheckVersion % Test,
  "io.circe"       %% "circe-core"           % circeVersion,
  "io.circe"       %% "circe-generic"        % circeVersion,
  "io.circe"       %% "circe-generic-extras" % circeVersion,
  "io.circe"       %% "circe-parser"         % circeVersion,
  "org.typelevel"  %% "cats-core"            % catsVersion,
  "org.typelevel"  %% "cats-effect"          % catsEffectVersion,
  "org.typelevel"  %% "spire"                % spireVersion,
  "com.outr"       %% "scribe"               % scribeVersion
)

// Java Only Crypto Library
val lib_bouncycastle = {
  val version = "1.62"
  Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % version
  )
}
