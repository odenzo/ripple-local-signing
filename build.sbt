import MyCompileOptions._
import sbt.Keys.resolvers


lazy val supportedScalaVersions = List("2.13.0", "2.12.8")
//scalaVersion := crossScalaVersions.value.head


ThisBuild / organization := "com.odenzo"
ThisBuild / scalaVersion := supportedScalaVersions.head
version in ThisBuild := "0.0.3"
name in ThisBuild := "ripple-local-signing"


coverageMinimum := 70
coverageFailOnMinimum := false
coverageHighlighting := true

publishArtifact in Test := false
parallelExecution in Test := false


lazy val ripple_local_signing = (project in file("."))
  .aggregate(signing)
  .settings(
    crossScalaVersions := Nil,
    publish / skip := true
    )


lazy val signing = (project in file("modules/shared"))
  .settings(
    resolvers in ThisBuild += Resolver.bintrayRepo("odenzooss", "maven"),
    resolvers in ThisBuild += Resolver.bintrayIvyRepo("odenzooss", "maven"),
    crossScalaVersions := supportedScalaVersions,
    name := "ripple-local-signing",

    scalacOptions := (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => optsV12 ++ warningsV12 ++ lintersV12
      case Some((2, n)) if n >= 13 => optsV13 ++ warningsV13 ++ lintersV13
      case _                       => Seq("-Yno-adapted-args")
    }),
    commonSettings,
    libraryDependencies += "com.odenzo" %% "ripple-binary-codec" % "0.2.7",
    libraryDependencies ++= xlibs ++ lib_bouncycastle,
    devSettings,
    )


lazy val commonSettings = Seq(

)
val devSettings = Seq(
  Test / logBuffered := false,
  Test / parallelExecution := false,
)



val circeVersion  = "0.12.0-M4"
val catsVersion   = "2.0.0-M4"
val spireVersion  = "0.17.0-M1"
val scribeVersion = "2.7.9"


// These are my standard stack and are all ScalaJS enabled.
val xlibs = Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsVersion,
  "org.typelevel" %% "spire" % spireVersion,
  "com.outr" %% "scribe" % scribeVersion
  )


// Java Only Crypto Library
val lib_bouncycastle = {
  val version = "1.62"
  Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % version
  )
}
