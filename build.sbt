import MyCompileOptions._
import sbt.Keys.resolvers

lazy val supportedScalaVersions = List("2.13.3")

ThisBuild / scalaVersion := supportedScalaVersions.head
ThisBuild / organization := "com.odenzo"
ThisBuild / name         := "ripple-local-signing"

Test / fork              := true
Test / parallelExecution := false
Test / logBuffered       := false

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.defaultLocal,
  Resolver.bintrayRepo("odenzooss", "maven"),
  Resolver.jcenterRepo
)

val rippleBinaryCodecVersion  = "0.1.6"
val circeVersion              = "0.12.3"
val circeGenericExtrasVersion = "0.12.2"
val catsVersion               = "2.0.0"
val catsEffectVersion         = "2.0.0"
val spireVersion              = "0.17.0-M1"
val scribeVersion             = "2.7.10"
val scalaTestVersion          = "3.0.8"
val scalaCheckVersion         = "1.14.2"
val pprintVersion             = "0.5.6"

lazy val ripple_local_signing_root = (project in file("."))
  .aggregate(signing)
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true
  )

lazy val signing = (project in file("./modules/core"))
  .settings(
    name               := "ripple-local-signing",
    crossScalaVersions := supportedScalaVersions,
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => optsV12 ++ warningsV12 ++ lintersV12
      case Some((2, n)) if n >= 13 => optsV13 ++ warningsV13 ++ lintersV13
      case _                       => Seq("-Yno-adapted-args")
    }),
    libraryDependencies += "com.odenzo" %% "ripple-binary-codec" % rippleBinaryCodecVersion,
    libraryDependencies ++= xlibs ++ lib_bouncycastle
  )

lazy val benchmark = (project in file("modules/benchmark"))
  .settings(
    publish            := {},
    publishLocal       := {},
    publishArtifact    := false,
    crossScalaVersions := Nil,
    javaOptions += "-XX:+UnlockCommercialFeatures"
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(signing) // test->compile not working in IntelliJ?

// These are my standard stack and are all ScalaJS enabled.
val xlibs = Seq(
  "com.lihaoyi"    %% "pprint"               % pprintVersion,
  "org.scalatest"  %% "scalatest"            % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck"           % scalaCheckVersion % Test,
  "io.circe"       %% "circe-core"           % circeVersion,
  "io.circe"       %% "circe-generic"        % circeVersion,
  "io.circe"       %% "circe-generic-extras" % circeGenericExtrasVersion,
  "io.circe"       %% "circe-parser"         % circeVersion,
  "org.typelevel"  %% "cats-core"            % catsVersion,
  "org.typelevel"  %% "cats-effect"          % catsEffectVersion,
  "org.typelevel"  %% "spire"                % spireVersion,
  "com.outr"       %% "scribe"               % scribeVersion
)

// Java Only Crypto Library
val lib_bouncycastle = {
  val version = "1.64"
  Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % version
  )
}
