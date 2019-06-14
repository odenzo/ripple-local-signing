import MyCompileOptions._
import sbt.Keys.resolvers

ThisBuild / organization := "com.odenzo"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.0.2"

name := "rippled-signing"

scalacOptions ++= Seq("-feature",
                      "-deprecation",
                      "-unchecked",
                      "-language:postfixOps",
                      "-language:higherKinds",
                      "-Ypartial-unification")

lazy val signing = (project in file("."))
  .settings(
    commonSettings,
    devSettings,
    scalacOptions ++= opts ++ warnings ++ linters
  )

lazy val commonSettings = Seq(
  libraryDependencies ++= mylibs ++ libs ++ lib_circe ++ lib_cats ++ lib_spire ++ lib_bouncycastle,
  resolvers ++= Seq(
    Resolver.bintrayIvyRepo("odenzo", "rippled-signing"),
    Resolver.defaultLocal, // Usual I pulishLocal to Ivy not maven
    Resolver.jcenterRepo // This is JFrogs Maven Repository for reading
  )
)
val devSettings = Seq(
  Test / logBuffered := false,
  Test / parallelExecution := false,
)

val mylibs = {
  Seq("com.odenzo" %% "ripple-binary-codec" % "0.0.2")
}

val libs = {
  Seq(
    "org.scalatest"              %% "scalatest"      % "3.0.7" % Test,
    "org.scalacheck"             %% "scalacheck"     % "1.14.0" % Test,
    "com.typesafe"               % "config"          % "1.3.4", //  https://github.com/typesafehub/config
    "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2",
    "ch.qos.logback"             % "logback-classic" % "1.2.3"
  )
}

/** JSON Libs == Circe and Associated Support Libs */
val lib_circe = {
  val circeVersion = "0.11.1"

  Seq(
    "io.circe" %% "circe-core"           % circeVersion,
    "io.circe" %% "circe-generic"        % circeVersion,
    "io.circe" %% "circe-java8"          % circeVersion,
    "io.circe" %% "circe-parser"         % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    "io.circe" %% "circe-iteratee" % "0.12.0",
    "io.iteratee" %% "iteratee-files" % "0.18.0"
    )

}

val lib_cats = {
  val catsVersion = "1.6.1"
  Seq(
    "org.typelevel" %% "cats-core"   % catsVersion, // Cats is pulled in via Circe for now
    "org.typelevel" %% "cats-effect" % "1.3.1" withSources () withJavadoc ()
  )
}

val lib_spire = {
  Seq(
    "org.typelevel" %% "spire" % "0.16.2"
  )
}

val lib_bouncycastle = {
  val version = "1.62"
  Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % version
  )
}
